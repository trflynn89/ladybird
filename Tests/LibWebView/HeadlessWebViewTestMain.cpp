/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include "HeadlessWebViewTest.h"
#include <AK/Array.h>
#include <AK/Format.h>
#include <AK/Vector.h>
#include <LibMain/Main.h>

namespace {

struct TestCase {
    StringView name;
    ErrorOr<int> (*run)(Main::Arguments);
};

constexpr Array test_cases {
    TestCase { "TestAutocomplete"sv, HeadlessTests::Autocomplete::run },
    TestCase { "TestFavicon"sv, HeadlessTests::Favicon::run },
    TestCase { "TestFileDownloader"sv, HeadlessTests::FileDownloader::run },
    TestCase { "TestNavigationDownloads"sv, HeadlessTests::NavigationDownloads::run },
    TestCase { "TestCrashOverlay"sv, HeadlessTests::CrashOverlay::run },
    TestCase { "TestBrowserHistoryTraversal"sv, HeadlessTests::BrowserHistoryTraversal::run },
    TestCase { "TestBeforeUnload"sv, HeadlessTests::BeforeUnload::run },
    TestCase { "TestInputMethodState"sv, HeadlessTests::InputMethodState::run },
    TestCase { "TestLinkHoverStatus"sv, HeadlessTests::LinkHoverStatus::run },
    TestCase { "TestUserActivation"sv, HeadlessTests::UserActivation::run },
    TestCase { "TestPictureInPictureUserActivation"sv, HeadlessTests::PictureInPictureUserActivation::run },
    TestCase { "TestPictureInPictureWindowControls"sv, HeadlessTests::PictureInPictureWindowControls::run },
    TestCase { "TestPictureInPictureWindowLifetime"sv, HeadlessTests::PictureInPictureWindowLifetime::run },
    TestCase { "TestWebCompatReportPage"sv, HeadlessTests::WebCompatReportPage::run },
    TestCase { "TestShadowTreeTooltipsAndLinks"sv, HeadlessTests::ShadowTreeTooltipsAndLinks::run },
    TestCase { "TestCompositorInputRouting"sv, HeadlessTests::CompositorInputRouting::run },
    TestCase { "TestRendererCookieAccess"sv, HeadlessTests::RendererCookieAccess::run },
    TestCase { "TestRendererOriginClaims"sv, HeadlessTests::RendererOriginClaims::run },
    TestCase { "TestImageDecoderIsolation"sv, HeadlessTests::ImageDecoderIsolation::run },
    TestCase { "TestCrashReportSubmission"sv, HeadlessTests::CrashReportSubmission::run },
    TestCase { "TestWebCompatReport"sv, HeadlessTests::WebCompatReport::run },
    TestCase { "TestRequestServerClientSessions"sv, HeadlessTests::RequestServerClientSessions::run },
    TestCase { "TestTabPerformanceMonitor"sv, HeadlessTests::TabPerformanceMonitor::run },
};

}

ErrorOr<int> ladybird_main(Main::Arguments arguments)
{
    if (arguments.strings.size() < 2) {
        warnln("Usage: {} <test-case> [arguments]", arguments.strings[0]);
        return 1;
    }
    auto name = arguments.strings[1];
    if (name == "--list"sv) {
        for (auto const& test : test_cases)
            outln("{}", test.name);
        return 0;
    }
    for (auto const& test : test_cases) {
        if (test.name != name)
            continue;

        // Each scenario receives the original program name and its own arguments.
        Vector<StringView> strings;
        Vector<char*> argv;
        strings.append(arguments.strings[0]);
        argv.append(arguments.argv[0]);
        for (int i = 2; i < arguments.argc; ++i) {
            strings.append(arguments.strings[i]);
            argv.append(arguments.argv[i]);
        }
        argv.append(nullptr);
        return test.run({
            .argc = arguments.argc - 1,
            .argv = argv.data(),
            .strings = strings.span(),
        });
    }
    warnln("Unknown headless WebView test: {}", name);
    return 1;
}
