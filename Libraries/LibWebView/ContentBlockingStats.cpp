/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/NumericLimits.h>
#include <LibWebView/ContentBlockingStats.h>

namespace WebView {

NonnullRefPtr<ContentBlockingStats> ContentBlockingStats::create()
{
    static u64 next_load_id = 1;
    VERIFY(next_load_id != NumericLimits<u64>::max());
    return adopt_ref(*new ContentBlockingStats(next_load_id++));
}

ContentBlockingStats::~ContentBlockingStats()
{
    if (m_on_discard)
        m_on_discard(m_load_id);
}

bool ContentBlockingStats::accept_report(u64 load_id, u64 sender, u64 cumulative_count)
{
    if (load_id != m_load_id)
        return false;
    auto previous = m_sender_counts.get(sender).value_or(0);
    if (cumulative_count <= previous)
        return false;
    auto delta = cumulative_count - previous;
    m_sender_counts.set(sender, cumulative_count);
    m_count += min(delta, NumericLimits<u64>::max() - m_count);
    return true;
}

}
