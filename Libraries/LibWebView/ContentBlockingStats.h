/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Function.h>
#include <AK/HashMap.h>
#include <AK/NonnullRefPtr.h>
#include <AK/RefCounted.h>
#include <AK/Weakable.h>
#include <LibURL/URL.h>
#include <LibWebView/Export.h>

namespace WebView {

// One object survives with its document or pending population, never with the active tab URL.
class WEBVIEW_API ContentBlockingStats final
    : public RefCounted<ContentBlockingStats>
    , public Weakable<ContentBlockingStats> {
public:
    static NonnullRefPtr<ContentBlockingStats> create();
    ~ContentBlockingStats();
    void set_discard_callback(Function<void(u64)> callback) { m_on_discard = move(callback); }
    u64 load_id() const { return m_load_id; }
    u64 count() const { return m_count; }
    bool has_sender(u64 sender) const { return m_sender_counts.contains(sender); }
    bool accept_report(u64 load_id, u64 sender, u64 cumulative_count);
    Optional<URL::URL> const& failed_url() const { return m_failed_url; }
    void set_failed_url(URL::URL url) { m_failed_url = move(url); }

private:
    explicit ContentBlockingStats(u64 load_id)
        : m_load_id(load_id)
    {
    }
    Function<void(u64)> m_on_discard;
    Optional<URL::URL> m_failed_url;
    u64 m_load_id { 0 };
    u64 m_count { 0 };
    HashMap<u64, u64> m_sender_counts;
};

enum class ContentBlockingState : u8 {
    Unavailable,
    CommandLineDisabled,
    GlobalDisabled,
    SiteDisabled,
    Enabled,
    NoRules,
};

struct ContentBlockingSnapshot {
    Optional<String> site;
    ContentBlockingState state { ContentBlockingState::Unavailable };
    u64 load_id { 0 };
    u64 blocked_request_count { 0 };

    bool enabled() const { return state == ContentBlockingState::Enabled || state == ContentBlockingState::NoRules; }
    bool can_toggle() const { return site.has_value() && (enabled() || state == ContentBlockingState::SiteDisabled); }
};

}
