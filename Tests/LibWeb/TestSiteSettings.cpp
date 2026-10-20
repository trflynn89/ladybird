/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibTest/TestCase.h>
#include <LibURL/Parser.h>
#include <LibURL/URL.h>
#include <LibWeb/Platform/SiteSettings.h>

using Web::Bindings::PermissionState;
using Web::HTML::AutoplayPolicy;
using Web::Platform::SiteSettings;

static URL::Origin origin(StringView url)
{
    return URL::Parser::basic_parse(url)->origin();
}

TEST_CASE(unconfigured_settings_have_no_value)
{
    SiteSettings settings;
    EXPECT(!settings.value_for_origin("unknown"_utf16, origin("https://example.com"sv)).has_value());
}

TEST_CASE(allowlists_use_exact_origins)
{
    SiteSettings settings;
    Array allowlist { "example.com"_utf16, "https://example.org:8443/path"_utf16, "https://"_utf16 };
    settings.set("geolocation"_utf16, PermissionState::Denied, PermissionState::Granted, allowlist);

    auto allowed = [&](StringView url) {
        return settings.value_for_origin("geolocation"_utf16, origin(url))->get<PermissionState>() == PermissionState::Granted;
    };
    EXPECT(allowed("https://example.com/path"sv));
    EXPECT(allowed("https://example.com:443"sv));
    EXPECT(allowed("https://example.org:8443"sv));
    EXPECT(!allowed("https://sub.example.com"sv));
    EXPECT(!allowed("http://example.com"sv));
    EXPECT(!allowed("https://example.com:8443"sv));
    EXPECT(!allowed("https://example.org"sv));
    EXPECT(!allowed("https://example.com.evil.test"sv));
    EXPECT_EQ(settings.value_for_origin("geolocation"_utf16, URL::Origin::create_opaque())->get<PermissionState>(), PermissionState::Denied);
}

TEST_CASE(updates_replace_policies_and_allowlists_without_changing_other_settings)
{
    SiteSettings settings;
    Array allowlist { "example.com"_utf16 };
    settings.set("autoplay"_utf16, AutoplayPolicy::BlockAudio, AutoplayPolicy::AllowAudioAndVideo, allowlist);
    settings.set("geolocation"_utf16, PermissionState::Denied, PermissionState::Granted, allowlist);
    auto example = origin("https://example.com"sv);
    EXPECT_EQ(settings.value_for_origin("autoplay"_utf16, example)->get<AutoplayPolicy>(), AutoplayPolicy::AllowAudioAndVideo);
    EXPECT_EQ(settings.value_for_origin("geolocation"_utf16, example)->get<PermissionState>(), PermissionState::Granted);

    settings.set("geolocation"_utf16, PermissionState::Denied, PermissionState::Granted, {});
    EXPECT_EQ(settings.value_for_origin("geolocation"_utf16, example)->get<PermissionState>(), PermissionState::Denied);
    EXPECT_EQ(settings.value_for_origin("autoplay"_utf16, example)->get<AutoplayPolicy>(), AutoplayPolicy::AllowAudioAndVideo);

    settings.set("geolocation"_utf16, PermissionState::Granted, PermissionState::Granted, {});
    EXPECT_EQ(settings.value_for_origin("geolocation"_utf16, origin("https://other.test"sv))->get<PermissionState>(), PermissionState::Granted);
}

TEST_CASE(autoplay_can_preserve_same_origin_domain_matching)
{
    SiteSettings settings;
    Array allowlist { "https://example.com"_utf16 };
    settings.set("autoplay"_utf16, AutoplayPolicy::BlockAudio, AutoplayPolicy::AllowAudioAndVideo, allowlist);
    auto example = origin("https://example.com"sv);
    URL::Origin relaxed { example.scheme(), example.host(), example.port(), example.host() };

    EXPECT_EQ(settings.value_for_origin("autoplay"_utf16, relaxed)->get<AutoplayPolicy>(), AutoplayPolicy::AllowAudioAndVideo);
    EXPECT_EQ(settings.value_for_origin("autoplay"_utf16, relaxed, SiteSettings::OriginMatching::SameOriginDomain)->get<AutoplayPolicy>(), AutoplayPolicy::BlockAudio);
}
