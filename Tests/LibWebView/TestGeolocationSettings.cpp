/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/LexicalPath.h>
#include <LibCore/File.h>
#include <LibCore/StandardPaths.h>
#include <LibCore/System.h>
#include <LibFileSystem/FileSystem.h>
#include <LibTest/TestCase.h>
#include <LibURL/Origin.h>
#include <LibURL/Parser.h>
#include <LibWebView/Settings.h>

static ByteString settings_path()
{
    return LexicalPath::join(Core::StandardPaths::tempfile_directory(), ByteString::formatted("test-geolocation-settings-{}.json", Core::System::getpid())).string();
}

static void remove_settings_file()
{
    if (FileSystem::exists(settings_path()))
        MUST(FileSystem::remove(settings_path(), FileSystem::RecursionMode::Disallowed));
}

static void write_settings_file(StringView contents)
{
    auto file = MUST(Core::File::open(settings_path(), Core::File::OpenMode::Write));
    MUST(file->write_until_depleted(contents.bytes()));
}

static URL::Origin origin(StringView url)
{
    return URL::Parser::basic_parse(url)->origin();
}

TEST_CASE(blocked_by_default)
{
    remove_settings_file();
    auto settings = WebView::Settings::create(settings_path());
    EXPECT_EQ(settings.geolocation_settings().policy, WebView::GeolocationPolicy::Block);
    EXPECT(!settings.geolocation_settings().is_allowed(origin("https://example.com"sv)));
}

TEST_CASE(allowlist_uses_exact_origins)
{
    remove_settings_file();
    auto settings = WebView::Settings::create(settings_path());
    settings.add_geolocation_site_filter("  EXAMPLE.com/path  "_string);
    settings.add_geolocation_site_filter("https://example.com:443"_string);
    settings.add_geolocation_site_filter("localhost:8443"_string);
    settings.add_geolocation_site_filter("data:text/plain,hello"_string);
    settings.add_geolocation_site_filter("file:///tmp/example"_string);
    settings.add_geolocation_site_filter("https://"_string);
    settings.add_geolocation_site_filter("   "_string);

    auto const& geolocation = settings.geolocation_settings();
    EXPECT_EQ(geolocation.site_filters.size(), 2u);
    EXPECT(geolocation.is_allowed(origin("https://example.com/other"sv)));
    EXPECT(geolocation.is_allowed(origin("https://localhost:8443"sv)));
    EXPECT(!geolocation.is_allowed(origin("https://sub.example.com"sv)));
    EXPECT(!geolocation.is_allowed(origin("http://example.com"sv)));
    EXPECT(!geolocation.is_allowed(origin("https://example.com:8443"sv)));
    EXPECT(!geolocation.is_allowed(origin("https://example.com.evil.test"sv)));
    EXPECT(!geolocation.is_allowed(URL::Origin::create_opaque()));
    remove_settings_file();
}

TEST_CASE(policies_and_exceptions_persist_and_can_be_revoked)
{
    remove_settings_file();
    {
        auto settings = WebView::Settings::create(settings_path());
        settings.add_geolocation_site_filter("example.com"_string);
        settings.set_geolocation_policy(WebView::GeolocationPolicy::Allow);
    }
    {
        auto settings = WebView::Settings::create(settings_path());
        EXPECT_EQ(settings.geolocation_settings().policy, WebView::GeolocationPolicy::Allow);
        EXPECT(settings.geolocation_settings().is_allowed(origin("https://other.test"sv)));
        settings.set_geolocation_policy(WebView::GeolocationPolicy::Block);
        EXPECT(settings.geolocation_settings().is_allowed(origin("https://example.com"sv)));
        EXPECT(!settings.geolocation_settings().is_allowed(origin("https://other.test"sv)));
        settings.remove_geolocation_site_filter("https://example.com"_string);
    }
    {
        auto settings = WebView::Settings::create(settings_path());
        EXPECT(!settings.geolocation_settings().is_allowed(origin("https://example.com"sv)));
        settings.add_geolocation_site_filter("example.com"_string);
        settings.remove_all_geolocation_site_filters();
    }
    auto settings = WebView::Settings::create(settings_path());
    EXPECT(settings.geolocation_settings().site_filters.is_empty());
    remove_settings_file();
}

TEST_CASE(invalid_settings_do_not_grant_access)
{
    write_settings_file(R"({"geolocation": {"policy": "invalid", "siteFilters": [123, "file:///tmp/example", "https://"]}})"sv);
    auto settings = WebView::Settings::create(settings_path());
    EXPECT(!settings.geolocation_settings().is_allowed(origin("https://example.com"sv)));
    EXPECT(!settings.geolocation_settings().is_allowed(URL::Origin::create_opaque()));
    remove_settings_file();
}
