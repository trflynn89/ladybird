/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/LexicalPath.h>
#include <AK/Random.h>
#include <AK/ScopeGuard.h>
#include <LibCore/Directory.h>
#include <LibCore/Environment.h>
#include <LibCore/GeolocationProvider.h>
#include <LibCore/StandardPaths.h>
#include <LibFileSystem/FileSystem.h>
#include <LibGfx/SystemTheme.h>
#include <LibMain/Main.h>
#include <LibURL/Parser.h>
#include <LibWebView/Application.h>
#include <LibWebView/CanonicalEnvironmentSettingsObject.h>
#include <LibWebView/CanonicalTraversable.h>
#include <LibWebView/CanonicalWindow.h>
#include <LibWebView/HeadlessWebView.h>
#include <LibWebView/Utilities.h>
#include <LibWebView/WebContentPage.h>

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

class TestGeolocationProvider final : public Core::GeolocationProvider {
public:
    AK_ALLOC_WITH_KMALLOC;

    virtual ErrorOr<RequestId, Core::GeolocationError> request_current_position(SuccessCallback on_success, ErrorCallback) override
    {
        auto id = ++next_id;
        requests.set(id, move(on_success));
        return id;
    }

    virtual void cancel_current_position_request(RequestId id) override
    {
        VERIFY(requests.contains(id));
        late_callbacks.append(requests.take(id).release_value());
        ++cancelled_requests;
    }

    virtual ErrorOr<WatchId, Core::GeolocationError> start_watching_position(SuccessCallback on_success, ErrorCallback) override
    {
        auto id = ++next_id;
        watches.set(id, move(on_success));
        return id;
    }

    virtual void stop_watching_position(WatchId id) override
    {
        VERIFY(watches.contains(id));
        late_callbacks.append(watches.take(id).release_value());
        ++stopped_watches;
    }

    u64 next_id { 0 };
    size_t cancelled_requests { 0 };
    size_t stopped_watches { 0 };
    HashMap<u64, SuccessCallback> requests;
    HashMap<u64, SuccessCallback> watches;
    Vector<SuccessCallback> late_callbacks;
};

TestGeolocationProvider* provider;
bool provider_available = true;

}

ErrorOr<int> ladybird_main(Main::Arguments arguments)
{
    auto directory = ByteString::formatted("{}/Ladybird-TestGeolocationPermissions-{}", Core::StandardPaths::tempfile_directory(), generate_random_uuid());
    TRY(Core::Directory::create(directory, Core::Directory::CreateDirectories::Yes));
    auto cleanup = ScopeGuard([&] { MUST(FileSystem::remove(directory, FileSystem::RecursionMode::Allowed)); });
    MUST(Core::Environment::set("XDG_CONFIG_HOME"sv, directory, Core::Environment::Overwrite::Yes));

#if defined(LADYBIRD_BINARY_PATH)
    auto app = TRY(TestApplication::create(arguments, LADYBIRD_BINARY_PATH));
#else
    auto app = TRY(TestApplication::create(arguments, OptionalNone {}));
#endif

    Core::GeolocationProvider::set_provider_functions([]() -> ErrorOr<NonnullOwnPtr<Core::GeolocationProvider>> {
        auto instance = make<TestGeolocationProvider>();
        provider = instance.ptr();
        return instance; }, [] { return provider_available; });

    auto theme = TRY(Gfx::load_system_theme(LexicalPath::join(WebView::s_ladybird_resource_root, "themes"sv, "Default.ini"sv).string()));
    auto first = WebView::HeadlessWebView::create(theme, { 800, 600 });
    auto second = WebView::HeadlessWebView::create(theme, { 800, 600 });
    auto environment_id = [](auto& view) { return view.traversable().active_document().relevant_global_object().relevant_settings_object().id(); };
    auto first_id = environment_id(*first);
    auto second_id = environment_id(*second);
    auto first_origin = URL::Parser::basic_parse("https://example.com"sv)->origin();
    auto second_origin = URL::Parser::basic_parse("https://other.test"sv)->origin();
    first->page().spoof_document_origin_for_testing(first_id, first_origin);
    second->page().spoof_document_origin_for_testing(second_id, second_origin);
    auto& settings = WebView::Application::settings();
    settings.set_geolocation_policy(WebView::GeolocationPolicy::Block);
    settings.remove_all_geolocation_site_filters();

    VERIFY(!first->page().is_geolocation_allowed(first_id));
    first->on_request_geolocation_position(first->page(), 1, first_id);
    first->on_start_geolocation_position_watch(first->page(), 2, first_id);
    VERIFY(!provider);

    settings.add_geolocation_site_filter("example.com"_string);
    settings.add_geolocation_site_filter("other.test"_string);
    VERIFY(first->page().is_geolocation_allowed(first_id));
    VERIFY(!first->page().is_geolocation_allowed(second_id));
    VERIFY(!first->page().is_geolocation_allowed(Web::HTML::EnvironmentId::generate()));
    first->on_request_geolocation_position(first->page(), 3, first_id);
    first->on_start_geolocation_position_watch(first->page(), 4, first_id);
    second->on_request_geolocation_position(second->page(), 3, second_id);
    second->on_start_geolocation_position_watch(second->page(), 4, second_id);
    VERIFY(provider->requests.size() == 2 && provider->watches.size() == 2);

    // Revoking one origin cancels only its pending requests and watches.
    settings.remove_geolocation_site_filter("https://example.com"_string);
    VERIFY(provider->cancelled_requests == 1 && provider->stopped_watches == 1);
    VERIFY(provider->requests.size() == 1 && provider->watches.size() == 1);
    VERIFY(!first->page().is_geolocation_allowed(first_id));
    VERIFY(second->page().is_geolocation_allowed(second_id));

    // A provider may already have queued an update when it receives the cancellation.
    for (auto& callback : provider->late_callbacks)
        callback({ .latitude = 1, .longitude = 2, .accuracy = 3, .altitude = {}, .altitude_accuracy = {}, .heading = {}, .speed = {} });
    provider->late_callbacks.clear();

    settings.set_geolocation_policy(WebView::GeolocationPolicy::Allow);
    VERIFY(first->page().is_geolocation_allowed(first_id));
    settings.remove_all_geolocation_site_filters();
    VERIFY(provider->requests.size() == 1 && provider->watches.size() == 1);
    settings.set_geolocation_policy(WebView::GeolocationPolicy::Block);
    VERIFY(provider->cancelled_requests == 2 && provider->stopped_watches == 2);
    VERIFY(provider->requests.is_empty() && provider->watches.is_empty());

    settings.add_geolocation_site_filter("example.com"_string);
    provider_available = false;
    VERIFY(!first->page().is_geolocation_allowed(first_id));
    provider_available = true;
    VERIFY(first->page().is_geolocation_allowed(first_id));
    first->page().spoof_document_origin_for_testing(first_id, URL::Origin::create_opaque());
    VERIFY(!first->page().is_geolocation_allowed(first_id));

    outln("PASS: geolocation is authorized per origin and revocation stops only affected requests and watches");
    return 0;
}
