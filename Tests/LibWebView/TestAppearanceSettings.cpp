/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/JsonValue.h>
#include <LibTest/TestCase.h>
#include <LibWebView/Settings.h>

TEST_CASE(bookmarks_bar_modes)
{
    auto parse = [](StringView json) {
        return WebView::Settings::parse_appearance(MUST(JsonValue::from_string(json))).show_bookmarks_bar;
    };

    EXPECT_EQ(parse(R"({"showBookmarksBar":"always"})"sv), WebView::ShowBookmarksBar::Always);
    EXPECT_EQ(parse(R"({"showBookmarksBar":"never"})"sv), WebView::ShowBookmarksBar::Never);
    EXPECT_EQ(parse(R"({"showBookmarksBar":"onLocationHover"})"sv), WebView::ShowBookmarksBar::OnLocationHover);

    // Preserve preferences written before the boolean setting became a three-way choice.
    EXPECT_EQ(parse(R"({"showBookmarksBar":true})"sv), WebView::ShowBookmarksBar::Always);
    EXPECT_EQ(parse(R"({"showBookmarksBar":false})"sv), WebView::ShowBookmarksBar::Never);

    EXPECT_EQ(parse("{}"sv), WebView::ShowBookmarksBar::Always);
    EXPECT_EQ(parse(R"({"showBookmarksBar":"unknown"})"sv), WebView::ShowBookmarksBar::Always);
    EXPECT_EQ(parse(R"({"showBookmarksBar":42})"sv), WebView::ShowBookmarksBar::Always);
}
