/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include "HeadlessTest.h"
#include <AK/Base64.h>
#include <AK/LexicalPath.h>
#include <AK/StringBuilder.h>
#include <LibCore/Environment.h>
#include <LibCore/File.h>
#include <LibCore/Socket.h>
#include <LibCore/TCPServer.h>
#include <LibGfx/SystemTheme.h>
#include <LibTest/TestCase.h>
#include <LibURL/Parser.h>
#include <LibWebView/HeadlessWebView.h>
#include <LibWebView/Utilities.h>

TEST_CASE(autoplay_uses_permission_defaults_without_prompting)
{
    Test::HeadlessTestEnvironment environment { "TestAutoplay"sv };
    auto app = TRY_OR_FAIL(environment.create_application());
    auto theme_path = LexicalPath::join(WebView::s_ladybird_resource_root, "themes"sv, "Default.ini"sv);
    auto theme = TRY_OR_FAIL(Gfx::load_system_theme(theme_path.string()));
    auto view = WebView::HeadlessWebView::create(move(theme), { 800, 600 });

    size_t loads_finished = 0;
    view->on_load_finish = [&](auto const&) { ++loads_finished; };
    Core::EventLoop::current().spin_until([&] { return loads_finished == 1; });

    auto source_directory = Core::Environment::get("LADYBIRD_SOURCE_DIR"sv);
    VERIFY(source_directory.has_value());
    auto video_path = LexicalPath::join(*source_directory, "Tests/LibWeb/Assets/test-webm.webm"sv);
    auto video = TRY_OR_FAIL(Core::File::open(video_path.string(), Core::File::OpenMode::Read));
    auto video_bytes = TRY_OR_FAIL(video->read_until_eof());
    auto encoded_video = TRY_OR_FAIL(encode_base64(video_bytes));

    // Use an HTTP origin so playback obtains its permission snapshot from the browser process.
    auto server = TRY_OR_FAIL(Core::TCPServer::try_create());
    TRY_OR_FAIL(server->listen(IPv4Address(127, 0, 0, 1), 0));
    auto url = URL::Parser::basic_parse(TRY_OR_FAIL(String::formatted("http://127.0.0.1:{}/", *server->local_port()))).release_value();
    String html;
    Vector<NonnullOwnPtr<Core::TCPSocket>> connections;
    server->on_ready_to_accept = [&] {
        auto socket = MUST(server->accept());
        MUST(socket->set_blocking(true));
        socket->on_ready_to_read = [&, client = socket.ptr(), request = StringBuilder {}]() mutable {
            Array<u8, 4096> buffer;
            auto bytes = MUST(client->read_some(buffer));
            if (bytes.is_empty()) {
                client->close();
                return;
            }
            request.append(StringView { bytes });
            if (!request.string_view().contains("\r\n\r\n"sv))
                return;
            auto response = MUST(String::formatted("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n{}", html.bytes().size(), html));
            MUST(client->write_until_depleted(response.bytes()));
            client->close();
        };
        connections.append(move(socket));
    };

    size_t prompt_count = 0;
    view->on_request_permission = [&](auto const&, auto on_complete) {
        ++prompt_count;
        on_complete(Web::Permissions::PromptOutcome::Block);
    };
    Optional<String> result;
    view->on_title_change = [&](auto const& title) {
        if (title == "played"sv || title.ends_with("Error"sv))
            result = title.to_utf8();
    };

    auto check_playback = [&](bool muted, bool allowed) {
        result.clear();
        html = MUST(String::formatted(R"(
            <!DOCTYPE html>
            <video id="video" loop src="data:video/webm;base64,{}"></video>
            <script>
                const video = document.querySelector('#video');
                video.muted = {};
                video.play().then(() => document.title = 'played', error => document.title = error.name);
            </script>
        )",
            encoded_video, muted ? "true"sv : "false"sv));
        view->load(url);
        Core::EventLoop::current().spin_until([&] { return result.has_value(); });
        EXPECT_EQ(*result, allowed ? "played"sv : "NotAllowedError"sv);
    };

    using Web::HTML::AutoplayPolicy;
    for (auto policy : { AutoplayPolicy::AllowAudioAndVideo, AutoplayPolicy::BlockAudio, AutoplayPolicy::BlockAudioAndVideo }) {
        WebView::Application::settings().set_autoplay_policy(policy);
        for (bool muted : { false, true }) {
            auto allowed = policy == AutoplayPolicy::AllowAudioAndVideo || (policy == AutoplayPolicy::BlockAudio && muted);
            check_playback(muted, allowed);
        }
    }

    // A site decision overrides the global default, and removing it restores that default.
    view->set_site_autoplay_policy(url.origin(), AutoplayPolicy::AllowAudioAndVideo);
    check_playback(false, true);
    view->set_site_autoplay_policy(url.origin(), AutoplayPolicy::BlockAudio);
    check_playback(false, false);
    check_playback(true, true);
    view->set_site_autoplay_policy(url.origin(), {});
    check_playback(true, false);
    EXPECT_EQ(prompt_count, 0u);
}
