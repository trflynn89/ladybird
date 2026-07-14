/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/Function.h>
#include <AK/LexicalPath.h>
#include <AK/String.h>
#include <AK/Vector.h>
#include <LibDevTools/Client/Status.h>
#include <LibDevTools/Forward.h>
#include <LibURL/Forward.h>

namespace DevTools::Client {

// Owns the shared, single-flight job of getting the Firefox runtime ready (download, extract, and splice the Ladybird
// runtime patches). One instance exists per application; the runtime is a shared resource, so only one download /
// extraction runs at a time. Callers that arrive while it is in flight are queued.
class DEVTOOLS_API RuntimeProvisioner {
public:
    using OnFetchComplete = Function<void(ErrorOr<void>)>;
    using OnRuntimeDownloadRequested = Function<void(URL::URL const&, LexicalPath const&, OnFetchComplete)>;
    using OnReady = Function<void(ErrorOr<void>)>;

    RuntimeProvisioner();

    OnRuntimeDownloadRequested on_runtime_download_requested;
    Function<void(Status const&)> on_status_changed;

    void ensure_ready(OnReady);

private:
    enum class State : u8 {
        Idle,
        Provisioning,
        Ready,
    };

    enum class ForceUpdate : u8 {
        No,
        Yes,
    };

    enum class ExtractRuntime : u8 {
        No,
        Yes,
    };

    enum class Success : u8 {
        No,
        Yes,
    };

    void begin_provisioning(ForceUpdate);
    void start_download();
    void start_preparation(ExtractRuntime);
    void finish(Success);

    void retry_or_fail(String message);
    void fail(String message);

    void report_stage(Stage) const;

    State m_state { State::Idle };
    Vector<OnReady> m_pending;
    size_t m_attempts { 0 };
};

}
