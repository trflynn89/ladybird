/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibCore/Directory.h>
#include <LibCore/EventLoop.h>
#include <LibDevTools/Client/Runtime.h>
#include <LibDevTools/Client/RuntimeProvisioner.h>
#include <LibFileSystem/FileSystem.h>
#include <LibThreading/ThreadPool.h>
#include <LibURL/URL.h>

namespace DevTools::Client {

RuntimeProvisioner::RuntimeProvisioner() = default;

void RuntimeProvisioner::ensure_ready(OnReady callback)
{
    if (m_state == State::Ready) {
        callback({});
        return;
    }

    m_pending.append(move(callback));

    if (m_state == State::Idle) {
        m_attempts = 0;
        begin_provisioning(ForceUpdate::No);
    }
}

void RuntimeProvisioner::begin_provisioning(ForceUpdate force_update)
{
    if (!is_supported_platform()) {
        fail("The DevTools client runtime is not supported on this platform"_string);
        return;
    }

    m_state = State::Provisioning;
    ++m_attempts;

    report_stage(Stage::CheckingRuntime);

    // A missing marker means the installed runtime is not the pinned version, so replace it rather than blessing the
    // existing files with the current version marker.
    if (force_update == ForceUpdate::Yes || !runtime_is_ready()) {
        start_download();
        return;
    }

    // Firefox may replace omni.ja while applying an update. Revalidate the actual runtime patches once per application
    // process rather than trusting persistent state from the previous run.
    start_preparation(ExtractRuntime::No);
}

void RuntimeProvisioner::start_download()
{
    VERIFY(on_runtime_download_requested);

    auto const& archive_path = runtime_download_path();

    if (auto result = Core::Directory::create(archive_path.parent(), Core::Directory::CreateDirectories::Yes); result.is_error()) {
        fail(MUST(String::formatted("{}", result.error())));
        return;
    }

    report_stage(Stage::DownloadingRuntime);

    on_runtime_download_requested(
        runtime_download_url(),
        archive_path,
        [this](ErrorOr<void> result) {
            if (result.is_error())
                retry_or_fail(MUST(String::formatted("Failed to download the DevTools runtime: {}", result.error())));
            else
                start_preparation(ExtractRuntime::Yes);
        });
}

void RuntimeProvisioner::start_preparation(ExtractRuntime extract_runtime)
{
    report_stage(extract_runtime == ExtractRuntime::Yes ? Stage::ExtractingRuntime : Stage::PreparingRuntime);

    // Extraction and the omni.ja rewrite are CPU/IO heavy, so run them off the UI event loop and marshal status updates
    // and the final result back to it.
    Threading::ThreadPool::the().submit([this, extract_runtime, event_loop = Core::EventLoop::current_weak()]() mutable {
        auto post = [event_loop = move(event_loop)](Function<void()> callback) {
            if (auto loop = event_loop->take())
                loop->deferred_invoke(move(callback));
        };

        auto result = [this, extract_runtime, post]() -> ErrorOr<void> {
            if (extract_runtime == ExtractRuntime::Yes) {
                TRY(Client::extract_runtime());
                post([this]() { report_stage(Stage::PreparingRuntime); });
            }

            TRY(install_runtime());
            return {};
        }();

        post([this, result = move(result)]() {
            (void)FileSystem::remove(runtime_download_path().string(), FileSystem::RecursionMode::Allowed);

            if (result.is_error()) {
                retry_or_fail(MUST(String::formatted("Failed to prepare the DevTools runtime: {}", result.error())));
                return;
            }

            finish(Success::Yes);
        });
    });
}

void RuntimeProvisioner::finish(Success success)
{
    m_state = success == Success::Yes ? State::Ready : State::Idle;
    auto pending = move(m_pending);

    for (auto& callback : pending) {
        if (success == Success::Yes)
            callback({});
        else
            callback(Error::from_string_literal("DevTools runtime provisioning failed"));
    }
}

void RuntimeProvisioner::retry_or_fail(String message)
{
    static constexpr size_t MAX_ATTEMPTS = 2;

    if (m_attempts < MAX_ATTEMPTS) {
        begin_provisioning(ForceUpdate::Yes);
        return;
    }

    fail(move(message));
}

void RuntimeProvisioner::fail(String message)
{
    if (on_status_changed) {
        Status status;
        status.stage = Stage::Failed;
        status.error = move(message);
        on_status_changed(status);
    }

    finish(Success::No);
}

void RuntimeProvisioner::report_stage(Stage stage) const
{
    if (!on_status_changed)
        return;

    Status status;
    status.stage = stage;
    on_status_changed(status);
}

}
