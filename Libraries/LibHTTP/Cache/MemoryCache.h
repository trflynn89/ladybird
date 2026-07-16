/*
 * Copyright (c) 2025-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/ByteString.h>
#include <AK/HashMap.h>
#include <AK/NonnullRefPtr.h>
#include <AK/RefCounted.h>
#include <AK/Time.h>
#include <AK/Types.h>
#include <LibCore/ImmutableBytes.h>
#include <LibHTTP/Cache/CacheMode.h>
#include <LibHTTP/Forward.h>
#include <LibHTTP/HeaderList.h>
#include <LibURL/URL.h>

namespace HTTP {

class MemoryCache : public RefCounted<MemoryCache> {
public:
    struct Limits {
        size_t maximum_size { 64 * MiB };
        size_t maximum_entry_body_size { 8 * MiB };
    };

    struct Entry {
        u64 vary_key { 0 };

        u32 status_code { 0 };
        ByteString reason_phrase;
        NonnullRefPtr<HeaderList> request_headers;
        NonnullRefPtr<HeaderList> response_headers;
        Core::ImmutableBytes response_body;
        Optional<Core::ImmutableBytes> javascript_bytecode_cache;
        Optional<u64> javascript_bytecode_cache_vary_key;

        UnixDateTime request_time;
        UnixDateTime response_time;
    };

    static NonnullRefPtr<MemoryCache> create();
    static NonnullRefPtr<MemoryCache> create(Limits);

    Optional<Entry> open_entry(URL::URL const&, StringView method, HeaderList const& request_headers, CacheMode);

    void create_entry(URL::URL const&, StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Optional<Core::ImmutableBytes> javascript_bytecode_cache = {}, Optional<u64> javascript_bytecode_cache_vary_key = {});
    void finalize_entry(URL::URL const&, StringView method, HeaderList const& request_headers, u32 status_code, HeaderList const& response_headers, Core::ImmutableBytes response_body);
    bool store_entry(URL::URL const&, StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Core::ImmutableBytes response_body, Optional<Core::ImmutableBytes> javascript_bytecode_cache = {}, Optional<u64> javascript_bytecode_cache_vary_key = {});
    void update_javascript_bytecode_cache(URL::URL const&, StringView method, HeaderList const& request_headers, u64 vary_key, Core::ImmutableBytes javascript_bytecode_cache);

    Limits const& limits() const { return m_limits; }

private:
    struct PendingEntry {
        Entry entry;
        size_t size { 0 };
    };

    struct StoredEntry {
        Entry entry;
        size_t size { 0 };
        u64 last_access_sequence { 0 };
    };

    struct EntryIdentifier {
        u64 cache_key { 0 };
        u64 vary_key { 0 };
    };

    explicit MemoryCache(Limits);

    Optional<Entry> prepare_entry(StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Optional<Core::ImmutableBytes> javascript_bytecode_cache, Optional<u64> javascript_bytecode_cache_vary_key) const;
    bool commit_entry(u64 cache_key, Entry);

    static Optional<size_t> calculate_entry_size(Entry const&);
    bool ensure_space(size_t, Optional<EntryIdentifier> protected_entry = {});
    bool evict_least_recently_used_entry(Optional<EntryIdentifier> protected_entry = {});
    void remove_complete_entries(u64 cache_key, u64 vary_key);
    void remove_complete_entry(u64 cache_key, size_t index);

    Limits m_limits;
    size_t m_size { 0 };
    u64 m_access_sequence { 0 };

    HashMap<u64, Vector<PendingEntry>, IdentityHashTraits<u64>> m_pending_entries;
    HashMap<u64, Vector<StoredEntry>, IdentityHashTraits<u64>> m_complete_entries;
};

}
