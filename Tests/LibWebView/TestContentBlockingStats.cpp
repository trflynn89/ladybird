/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/NumericLimits.h>
#include <AK/WeakPtr.h>
#include <LibTest/TestCase.h>
#include <LibWebView/ContentBlockingStats.h>

TEST_CASE(cumulative_reports_and_distinct_loads)
{
    auto load = WebView::ContentBlockingStats::create();
    auto other = WebView::ContentBlockingStats::create();
    EXPECT_EQ(load->count(), 0u);
    EXPECT(load->accept_report(load->load_id(), 1, 12));
    EXPECT(!load->accept_report(load->load_id(), 1, 12));
    EXPECT(!load->accept_report(load->load_id(), 1, 11));
    EXPECT(load->accept_report(load->load_id(), 2, 4));
    EXPECT(load->accept_report(load->load_id(), 1, 15));
    EXPECT_EQ(load->count(), 19u);
    EXPECT(!load->accept_report(other->load_id(), 1, 100));
    EXPECT_EQ(load->count(), 19u);
    EXPECT_EQ(other->count(), 0u);
}

TEST_CASE(cumulative_count_saturates)
{
    auto load = WebView::ContentBlockingStats::create();
    EXPECT(load->accept_report(load->load_id(), 1, NumericLimits<u64>::max() - 1));
    EXPECT(load->accept_report(load->load_id(), 2, 100));
    EXPECT_EQ(load->count(), NumericLimits<u64>::max());
    EXPECT(load->accept_report(load->load_id(), 1, NumericLimits<u64>::max()));
    EXPECT_EQ(load->count(), NumericLimits<u64>::max());
    EXPECT(!load->accept_report(load->load_id(), 1, 0));
}

TEST_CASE(discarded_load_releases_accounting)
{
    WeakPtr<WebView::ContentBlockingStats> weak;
    {
        auto load = WebView::ContentBlockingStats::create();
        weak = load->make_weak_ptr();
        EXPECT(weak);
    }
    EXPECT(!weak);
}

TEST_CASE(discard_notifies_owner_once)
{
    u64 discarded_load = 0;
    u64 load_id = 0;
    {
        auto load = WebView::ContentBlockingStats::create();
        load_id = load->load_id();
        load->set_discard_callback([&](u64 id) { EXPECT_EQ(discarded_load, 0u); discarded_load = id; });
        EXPECT_EQ(discarded_load, 0u);
    }
    EXPECT_EQ(discarded_load, load_id);
}
