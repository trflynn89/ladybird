/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/LexicalPath.h>
#include <AK/Random.h>
#include <AK/ScopeGuard.h>
#include <AK/String.h>
#include <AK/Utf16String.h>
#include <LibCore/Directory.h>
#include <LibCore/EventLoop.h>
#include <LibCore/StandardPaths.h>
#include <LibCore/Timer.h>
#include <LibFileSystem/FileSystem.h>
#include <LibGfx/SystemTheme.h>
#include <LibMain/Main.h>
#include <LibWebView/Application.h>
#include <LibWebView/HeadlessWebView.h>
#include <LibWebView/Utilities.h>
#include <stdlib.h>

namespace {

class TestApplication : public WebView::Application {
    WEB_VIEW_APPLICATION(TestApplication)

public:
    explicit TestApplication(Optional<ByteString> ladybird_binary_path)
        : WebView::Application(move(ladybird_binary_path))
    {
    }

    virtual void create_platform_options(WebView::BrowserOptions& browser_options, WebView::RequestServerOptions&, WebView::WebContentOptions& web_content_options) override
    {
        browser_options.headless_mode = WebView::HeadlessMode::Test;
        browser_options.disable_sql_database = WebView::DisableSQLDatabase::Yes;
        web_content_options.is_test_mode = WebView::IsTestMode::No;
    }

    virtual bool should_coordinate_browser_process() const override { return false; }
};

}

ErrorOr<int> ladybird_main(Main::Arguments arguments)
{
    auto test_config_directory = ByteString::formatted("{}/Ladybird-TestContentBlockingIntegration-{}", Core::StandardPaths::tempfile_directory(), generate_random_uuid());
    TRY(Core::Directory::create(test_config_directory, Core::Directory::CreateDirectories::Yes));
    auto cleanup_test_config_directory = ScopeGuard([&] {
        MUST(FileSystem::remove(test_config_directory, FileSystem::RecursionMode::Allowed));
    });
    VERIFY(setenv("XDG_CONFIG_HOME", test_config_directory.characters(), 1) == 0);

#if defined(LADYBIRD_BINARY_PATH)
    auto app = TRY(TestApplication::create(arguments, LADYBIRD_BINARY_PATH));
#else
    auto app = TRY(TestApplication::create(arguments, OptionalNone {}));
#endif

    WebView::Application::settings().set_content_blocker_enabled(true);

    auto theme_path = LexicalPath::join(WebView::s_ladybird_resource_root, "themes"sv, "Default.ini"sv);
    auto theme = TRY(Gfx::load_system_theme(theme_path.string()));

    auto view = WebView::HeadlessWebView::create(move(theme), { 800, 600 });

    auto wait = [&](StringView phase, auto condition) {
        auto watchdog = Core::Timer::create_single_shot(15000, [&] {
            warnln("Content blocking integration timed out: {} (load {}, count {})", phase, view->content_blocking_snapshot().load_id, view->content_blocking_snapshot().blocked_request_count);
            exit(1);
        });
        watchdog->start();
        Core::EventLoop::current().spin_until(move(condition));
        watchdog->stop();
    };

    size_t loads_finished = 0;
    view->on_load_finish = [&](auto const&) { ++loads_finished; };

    // Wait out the initial about:blank load; navigating before it completes would drop the navigation.
    wait("initial blank"sv, [&]() { return loads_finished >= 1; });

    VERIFY(WebView::Application::browser_options().urls.size() == 1);
    auto url = WebView::Application::browser_options().urls[0];
    if (WebView::Application::browser_options().enable_content_blocker == WebView::EnableContentBlocker::No) {
        view->load(url);
        wait("command-line disabled load"sv, [&] { return loads_finished >= 2; });
        VERIFY(view->content_blocking_snapshot().blocked_request_count == 0);
        VERIFY(view->content_blocking_snapshot().state == WebView::ContentBlockingState::CommandLineDisabled);
        return 0;
    }
    size_t changes = 0;
    view->on_content_blocking_change = [&](auto const&) { ++changes; };
    auto load_page = [&] {
        auto previous_loads = loads_finished;
        view->load(url);
        wait("fixture load"sv, [&] { return loads_finished > previous_loads; });
        wait("frame count"sv, [&] { return view->content_blocking_snapshot().blocked_request_count >= 2; });
        VERIFY(view->content_blocking_snapshot().blocked_request_count == 2);
    };
    load_page();
    auto snapshot = view->content_blocking_snapshot();
    VERIFY(snapshot.enabled());
    VERIFY(snapshot.site == "127.0.0.1"sv);
    auto initial_load_id = snapshot.load_id;
    view->run_javascript("for (let i = 0; i < 32; ++i) fetch('/blocked.js').catch(() => {});"_string);
    wait("request burst"sv, [&] { return view->content_blocking_snapshot().blocked_request_count >= 34; });
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 34);
    VERIFY(changes < 34);
    auto& page = view->page();
    auto duplicate = Web::ContentBlockingRequestContext { .load_id = initial_load_id, .navigable_id = view->traversable().id(), .environment_id = {}, .navigation_id = {}, .is_navigation = false };
    view->did_receive_content_blocking_count(page, duplicate, 33);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 34);

    bool history_updated = false;
    Utf16String expected_title = u"Updated"_utf16;
    view->on_title_change = [&](auto const& title) { history_updated = title == expected_title; };
    view->run_javascript("history.pushState({}, '', '#fragment'); document.title = 'Updated';"_string);
    wait("history update"sv, [&] { return history_updated; });
    VERIFY(view->content_blocking_snapshot().load_id == initial_load_id);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 34);
    history_updated = false;
    expected_title = u"Frame removed"_utf16;
    view->run_javascript("document.querySelector('iframe').remove(); fetch('/blocked.js').catch(() => { document.title = 'Frame removed'; });"_string);
    wait("frame removal"sv, [&] { return history_updated && view->content_blocking_snapshot().blocked_request_count == 35; });
    VERIFY(view->content_blocking_snapshot().load_id == initial_load_id);
    history_updated = false;
    expected_title = u"Other failures"_utf16;
    view->run_javascript("Promise.allSettled([fetch('http://127.0.0.1:9/blocked.js'), fetch('/disconnect')]).then(() => { document.title = 'Other failures'; });"_string);
    wait("ordinary failures"sv, [&] { return history_updated; });
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 35);

    // Provisional URLs and canceled navigations cannot replace the displayed owner.
    auto slow_url = url;
    StringView slow_path[] = { "slow"sv };
    slow_url.set_path(slow_path);
    bool navigation_started = false;
    view->on_load_start = [&] { navigation_started = true; };
    view->load(slow_url);
    wait("slow navigation started"sv, [&] { return navigation_started; });
    view->on_load_start = {};
    VERIFY(view->content_blocking_snapshot().load_id == initial_load_id);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 35);
    view->stop_loading();
    VERIFY(view->content_blocking_snapshot().load_id == initial_load_id);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 35);
    // Policy changes preserve counts, and only explicit reloads create a new load.
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, false));
    VERIFY(view->content_blocking_snapshot().state == WebView::ContentBlockingState::SiteDisabled);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 35);
    auto previous_loads = loads_finished;
    view->reload();
    wait("site-disabled reload"sv, [&] { return loads_finished > previous_loads; });
    VERIFY(view->content_blocking_snapshot().load_id != initial_load_id);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 0);
    auto exempt_load_id = view->content_blocking_snapshot().load_id;
    view->did_receive_content_blocking_count(view->page(), duplicate, 1000);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 0);
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, true));
    VERIFY(view->content_blocking_snapshot().load_id == exempt_load_id);
    load_page();
    VERIFY(view->content_blocking_snapshot().load_id != exempt_load_id);

    // Two simultaneous blocked frame navigations share the top-level counter.
    auto frames_url = url;
    StringView frames_path[] = { "frames"sv };
    frames_url.set_path(frames_path);
    previous_loads = loads_finished;
    view->load(frames_url);
    wait("parallel blocked frames"sv, [&] { return loads_finished > previous_loads; });
    VERIFY(view->content_blocking_snapshot().site == snapshot.site);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 2);

    // New tabs have independent loads, while regular and private tabs share site preferences.
    auto other_theme = TRY(Gfx::load_system_theme(theme_path.string()));
    auto other_view = WebView::HeadlessWebView::create(move(other_theme), { 800, 600 });
    size_t other_loads = 0;
    other_view->on_load_finish = [&](auto const& loaded_url) { if (loaded_url == frames_url) ++other_loads; };
    wait("new tab blank"sv, [&] { return other_view->traversable().hosted_state().has_value(); });
    other_view->load(frames_url);
    wait("new tab frame load"sv, [&] { return other_loads >= 1; });
    VERIFY(other_view->content_blocking_snapshot().blocked_request_count == 2);
    VERIFY(other_view->content_blocking_snapshot().load_id != view->content_blocking_snapshot().load_id);
    auto other_load_id = other_view->content_blocking_snapshot().load_id;
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, false));
    VERIFY(other_view->content_blocking_snapshot().state == WebView::ContentBlockingState::SiteDisabled);
    VERIFY(other_view->content_blocking_snapshot().load_id == other_load_id);
    VERIFY(other_view->content_blocking_snapshot().blocked_request_count == 2);
    auto private_theme = TRY(Gfx::load_system_theme(theme_path.string()));
    auto private_view = WebView::HeadlessWebView::create(move(private_theme), { 800, 600 }, WebView::IsPrivate::Yes);
    size_t private_loads = 0;
    private_view->on_load_finish = [&](auto const& loaded_url) { if (loaded_url == frames_url) ++private_loads; };
    wait("private tab blank"sv, [&] { return private_view->traversable().hosted_state().has_value(); });
    private_view->load(frames_url);
    wait("private baseline load"sv, [&] { return private_loads >= 1; });
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 0);
    MUST(private_view->set_content_blocker_enabled_for_site(*snapshot.site, true));
    VERIFY(private_view->content_blocking_snapshot().enabled());
    VERIFY(view->content_blocking_snapshot().enabled());
    VERIFY(other_view->content_blocking_snapshot().enabled());
    VERIFY(other_view->content_blocking_snapshot().load_id == other_load_id);
    VERIFY(other_view->content_blocking_snapshot().blocked_request_count == 2);
    auto settings_path = LexicalPath::join(WebView::Application::profile().paths().config, "Settings.json"sv).string();
    VERIFY(WebView::Settings::create(settings_path).content_blocker_enabled_for_site(*snapshot.site));
    private_view->reload();
    wait("private enabled load"sv, [&] { return private_loads >= 2; });
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 2);
    WebView::Application::the().reset_private_browsing_session();
    VERIFY(private_view->content_blocking_snapshot().enabled());
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 2);
    VERIFY(WebView::Settings::create(settings_path).content_blocker_enabled_for_site(*snapshot.site));
    MUST(private_view->set_content_blocker_enabled_for_site(*snapshot.site, false));
    VERIFY(private_view->content_blocking_snapshot().state == WebView::ContentBlockingState::SiteDisabled);
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 2);
    VERIFY(view->content_blocking_snapshot().state == WebView::ContentBlockingState::SiteDisabled);
    VERIFY(other_view->content_blocking_snapshot().state == WebView::ContentBlockingState::SiteDisabled);
    VERIFY(!WebView::Settings::create(settings_path).content_blocker_enabled_for_site(*snapshot.site));
    private_view->reload();
    wait("private disabled load"sv, [&] { return private_loads >= 3; });
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 0);
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, true));
    VERIFY(private_view->content_blocking_snapshot().enabled());
    VERIFY(private_view->content_blocking_snapshot().blocked_request_count == 0);

    // Enabling filtering after an allowed cached script preserves the existing cache path.
    auto cached_url = url;
    StringView cached_path[] = { "cached"sv };
    cached_url.set_path(cached_path);
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, false));
    previous_loads = loads_finished;
    view->load(cached_url);
    wait("cached allowed load"sv, [&] { return loads_finished > previous_loads; });
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 0);
    MUST(view->set_content_blocker_enabled_for_site(*snapshot.site, true));
    previous_loads = loads_finished;
    view->reload();
    wait("cached enabled reload"sv, [&] { return loads_finished > previous_loads; });
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 0);

    // A rejected top-level document retains its destination and pre-commit count.
    auto failed_url = url;
    StringView failed_path[] = { "blocked-document"sv };
    failed_url.set_path(failed_path);
    previous_loads = loads_finished;
    view->load(failed_url);
    wait("blocked destination"sv, [&] { return loads_finished > previous_loads; });
    VERIFY(view->content_blocking_snapshot().site == snapshot.site);
    VERIFY(view->content_blocking_snapshot().blocked_request_count == 1);
    outln("PASS: request rejection, frame aggregation, reload identity, and policy counts");
    return 0;
}
