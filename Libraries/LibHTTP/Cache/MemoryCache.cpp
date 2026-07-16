/*
 * Copyright (c) 2025-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Checked.h>
#include <AK/Debug.h>
#include <LibHTTP/Cache/MemoryCache.h>
#include <LibHTTP/Cache/Utilities.h>

namespace HTTP {

NonnullRefPtr<MemoryCache> MemoryCache::create()
{
    return create({});
}

NonnullRefPtr<MemoryCache> MemoryCache::create(Limits limits)
{
    return adopt_ref(*new MemoryCache(limits));
}

MemoryCache::MemoryCache(Limits limits)
    : m_limits(limits)
{
}

// A stored response satisfies a request only if the request header fields nominated by the response's Vary header
// match those of the request that produced the entry, per RFC 9111 4.1. open_entry() and update_javascript_bytecode_cache()
// must select entries identically, so share the predicate.
static bool entry_matches_request(HeaderList const& request_headers, MemoryCache::Entry const& entry)
{
    return create_vary_key(request_headers, entry.response_headers) == entry.vary_key;
}

// https://httpwg.org/specs/rfc9111.html#constructing.responses.from.caches
Optional<MemoryCache::Entry> MemoryCache::open_entry(URL::URL const& url, StringView method, HeaderList const& request_headers, CacheMode cache_mode)
{
    if (cache_mode == CacheMode::Reload || cache_mode == CacheMode::NoCache)
        return {};

    // When presented with a request, a cache MUST NOT reuse a stored response unless:
    // - the presented target URI (Section 7.1 of [HTTP]) and that of the stored response match, and
    // - the request method associated with the stored response allows it to be used for the presented request, and
    if (!is_cacheable(method, request_headers))
        return {};

    auto serialized_url = serialize_url_for_cache_storage(url);
    auto cache_key = create_cache_key(serialized_url, method);

    auto cache_entries = m_complete_entries.get(cache_key);
    if (!cache_entries.has_value()) {
        dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[35;1mNo cache entry for\033[0m {}", url);
        return {};
    }

    // - request header fields nominated by the stored response (if any) match those presented (see Section 4.1), and
    auto cache_entry_index = cache_entries->find_first_index_if([&](auto const& stored_entry) {
        return entry_matches_request(request_headers, stored_entry.entry);
    });
    if (!cache_entry_index.has_value()) {
        dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[35;1mVary mismatch for\033[0m {}", url);
        return {};
    }

    auto& cache_entry = cache_entries->at(*cache_entry_index).entry;

    // - the stored response does not contain the no-cache directive (Section 5.2.2.4), unless it is successfully
    //   validated (Section 4.3), and
    // - the stored response is one of the following:
    //       * fresh (see Section 4.2), or
    //       * allowed to be served stale (see Section 4.2.4), or
    //       * successfully validated (see Section 4.3).
    auto freshness_lifetime = calculate_freshness_lifetime(cache_entry.status_code, cache_entry.response_headers);
    auto current_age = calculate_age(cache_entry.response_headers, cache_entry.request_time, cache_entry.response_time);

    switch (cache_lifetime_status(request_headers, cache_entry.response_headers, freshness_lifetime, current_age)) {
    case CacheLifetimeStatus::Fresh:
        dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[32;1mOpened cache entry for\033[0m {} (lifetime={}s age={}s) ({} bytes)", url, freshness_lifetime.to_seconds(), current_age.to_seconds(), cache_entry.response_body.size());
        cache_entries->at(*cache_entry_index).last_access_sequence = ++m_access_sequence;
        return cache_entry;

    case CacheLifetimeStatus::Expired:
    case CacheLifetimeStatus::MustRevalidate:
    case CacheLifetimeStatus::StaleWhileRevalidate:
        if (cache_mode_permits_stale_responses(cache_mode)) {
            dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[32;1mOpened expired cache entry for\033[0m {} (lifetime={}s age={}s) ({} bytes)", url, freshness_lifetime.to_seconds(), current_age.to_seconds(), cache_entry.response_body.size());
            cache_entries->at(*cache_entry_index).last_access_sequence = ++m_access_sequence;
            return cache_entry;
        }

        dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[33;1mCache entry expired for\033[0m {} (lifetime={}s age={}s)", url, freshness_lifetime.to_seconds(), current_age.to_seconds());
        remove_complete_entry(cache_key, *cache_entry_index);
        return {};
    }

    VERIFY_NOT_REACHED();
}

Optional<MemoryCache::Entry> MemoryCache::prepare_entry(StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Optional<Core::ImmutableBytes> javascript_bytecode_cache, Optional<u64> javascript_bytecode_cache_vary_key) const
{
    if (!is_cacheable(method, request_headers))
        return {};
    if (!is_cacheable(status_code, response_headers))
        return {};

    auto vary_key = create_vary_key(request_headers, response_headers);

    auto request_headers_copy = HeaderList::create();
    store_header_and_trailer_fields(request_headers_copy, request_headers);

    auto response_headers_copy = HeaderList::create();
    store_header_and_trailer_fields(response_headers_copy, response_headers);

    return Entry {
        .vary_key = vary_key,
        .status_code = status_code,
        .reason_phrase = move(reason_phrase),
        .request_headers = move(request_headers_copy),
        .response_headers = move(response_headers_copy),
        .response_body = {},
        .javascript_bytecode_cache = move(javascript_bytecode_cache),
        .javascript_bytecode_cache_vary_key = javascript_bytecode_cache_vary_key,
        .request_time = request_time,
        .response_time = UnixDateTime::now(),
    };
}

Optional<size_t> MemoryCache::calculate_entry_size(Entry const& entry)
{
    Checked<size_t> size = entry.reason_phrase.length();

    auto account_headers = [&](HeaderList const& headers) {
        for (auto const& header : headers) {
            size += header.name.length();
            size += header.value.length();
        }
    };

    account_headers(entry.request_headers);
    account_headers(entry.response_headers);
    size += entry.response_body.size();
    if (entry.javascript_bytecode_cache.has_value())
        size += entry.javascript_bytecode_cache->size();

    if (size.has_overflow())
        return {};
    return size.value();
}

void MemoryCache::create_entry(URL::URL const& url, StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Optional<Core::ImmutableBytes> javascript_bytecode_cache, Optional<u64> javascript_bytecode_cache_vary_key)
{
    auto cache_entry = prepare_entry(method, request_headers, request_time, status_code, move(reason_phrase), response_headers, move(javascript_bytecode_cache), javascript_bytecode_cache_vary_key);
    if (!cache_entry.has_value())
        return;

    auto entry_size = calculate_entry_size(*cache_entry);
    if (!entry_size.has_value() || *entry_size > m_limits.maximum_size || !ensure_space(*entry_size))
        return;

    auto serialized_url = serialize_url_for_cache_storage(url);
    auto cache_key = create_cache_key(serialized_url, method);

    dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[32;1mCreated cache entry for\033[0m {}", url);
    m_pending_entries.ensure(cache_key).append({ cache_entry.release_value(), *entry_size });
    m_size += *entry_size;
}

// FIXME: It would be nicer if create_entry just returned the cache and vary keys. But the call sites of create_entry and
//        finalize_entry are pretty far apart, so passing that information along is rather awkward in Fetch.
void MemoryCache::finalize_entry(URL::URL const& url, StringView method, HeaderList const& request_headers, u32 status_code, HeaderList const& response_headers, Core::ImmutableBytes response_body)
{
    if (!is_cacheable(method, request_headers) || !is_cacheable(status_code, response_headers))
        return;

    auto serialized_url = serialize_url_for_cache_storage(url);
    auto cache_key = create_cache_key(serialized_url, method);
    auto vary_key = create_vary_key(request_headers, response_headers);

    if (auto cache_entries = m_pending_entries.get(cache_key); cache_entries.has_value()) {
        auto index = cache_entries->find_first_index_if([&](auto const& pending_entry) {
            return vary_key == pending_entry.entry.vary_key;
        });
        if (!index.has_value())
            return;

        dbgln_if(HTTP_MEMORY_CACHE_DEBUG, "\033[37m[memory]\033[0m \033[34;1mFinished caching\033[0m {} ({} bytes)", url, response_body.size());

        auto pending_entry = cache_entries->take(*index);
        m_size -= pending_entry.size;

        if (cache_entries->is_empty())
            m_pending_entries.remove(cache_key);

        pending_entry.entry.response_body = move(response_body);
        (void)commit_entry(cache_key, move(pending_entry.entry));
    }
}

bool MemoryCache::store_entry(URL::URL const& url, StringView method, HeaderList const& request_headers, UnixDateTime request_time, u32 status_code, ByteString reason_phrase, HeaderList const& response_headers, Core::ImmutableBytes response_body, Optional<Core::ImmutableBytes> javascript_bytecode_cache, Optional<u64> javascript_bytecode_cache_vary_key)
{
    auto cache_entry = prepare_entry(method, request_headers, request_time, status_code, move(reason_phrase), response_headers, move(javascript_bytecode_cache), javascript_bytecode_cache_vary_key);
    if (!cache_entry.has_value())
        return false;

    cache_entry->response_body = move(response_body);

    auto serialized_url = serialize_url_for_cache_storage(url);
    auto cache_key = create_cache_key(serialized_url, method);
    return commit_entry(cache_key, cache_entry.release_value());
}

bool MemoryCache::commit_entry(u64 cache_key, Entry cache_entry)
{
    if (cache_entry.response_body.size() > m_limits.maximum_entry_body_size)
        return false;

    auto entry_size = calculate_entry_size(cache_entry);
    if (!entry_size.has_value() || *entry_size > m_limits.maximum_size)
        return false;

    EntryIdentifier identifier { cache_key, cache_entry.vary_key };
    size_t replaced_size = 0;
    if (auto cache_entries = m_complete_entries.get(cache_key); cache_entries.has_value()) {
        for (auto const& stored_entry : *cache_entries) {
            if (stored_entry.entry.vary_key == cache_entry.vary_key)
                replaced_size += stored_entry.size;
        }
    }

    auto additional_size = *entry_size > replaced_size ? *entry_size - replaced_size : 0;
    if (!ensure_space(additional_size, identifier))
        return false;

    remove_complete_entries(cache_key, cache_entry.vary_key);
    m_complete_entries.ensure(cache_key).append({ move(cache_entry), *entry_size, ++m_access_sequence });
    m_size += *entry_size;
    return true;
}

bool MemoryCache::ensure_space(size_t size, Optional<EntryIdentifier> protected_entry)
{
    if (size > m_limits.maximum_size)
        return false;

    while (m_size > m_limits.maximum_size - size) {
        if (!evict_least_recently_used_entry(protected_entry))
            return false;
    }

    return true;
}

bool MemoryCache::evict_least_recently_used_entry(Optional<EntryIdentifier> protected_entry)
{
    Optional<u64> least_recently_used_cache_key;
    Optional<size_t> least_recently_used_index;
    u64 least_recently_used_sequence = NumericLimits<u64>::max();

    for (auto const& [cache_key, cache_entries] : m_complete_entries) {
        for (size_t index = 0; index < cache_entries.size(); ++index) {
            auto const& stored_entry = cache_entries[index];
            if (protected_entry.has_value() && protected_entry->cache_key == cache_key && protected_entry->vary_key == stored_entry.entry.vary_key)
                continue;
            if (stored_entry.last_access_sequence >= least_recently_used_sequence)
                continue;

            least_recently_used_cache_key = cache_key;
            least_recently_used_index = index;
            least_recently_used_sequence = stored_entry.last_access_sequence;
        }
    }

    if (!least_recently_used_cache_key.has_value())
        return false;

    remove_complete_entry(*least_recently_used_cache_key, *least_recently_used_index);
    return true;
}

void MemoryCache::remove_complete_entries(u64 cache_key, u64 vary_key)
{
    auto cache_entries = m_complete_entries.get(cache_key);
    if (!cache_entries.has_value())
        return;

    for (;;) {
        auto index = cache_entries->find_first_index_if([&](auto const& stored_entry) { return stored_entry.entry.vary_key == vary_key; });
        if (!index.has_value())
            break;
        m_size -= cache_entries->at(*index).size;
        cache_entries->remove(*index);
    }

    if (cache_entries->is_empty())
        m_complete_entries.remove(cache_key);
}

void MemoryCache::remove_complete_entry(u64 cache_key, size_t index)
{
    auto cache_entries = m_complete_entries.get(cache_key);
    VERIFY(cache_entries.has_value());
    VERIFY(index < cache_entries->size());

    m_size -= cache_entries->at(index).size;
    cache_entries->remove(index);
    if (cache_entries->is_empty())
        m_complete_entries.remove(cache_key);
}

void MemoryCache::update_javascript_bytecode_cache(URL::URL const& url, StringView method, HeaderList const& request_headers, u64 vary_key, Core::ImmutableBytes javascript_bytecode_cache)
{
    if (!is_cacheable(method, request_headers))
        return;

    auto serialized_url = serialize_url_for_cache_storage(url);
    auto cache_key = create_cache_key(serialized_url, method);

    // Attach the generated bytecode to every cache entry this request would select. Match entries the same way
    // open_entry() does, by re-deriving the vary key from the request and the entry's stored response headers, rather
    // than trusting the supplied vary key (which was computed in another process and can diverge for Vary responses).
    // The supplied vary key is still kept as javascript_bytecode_cache_vary_key so it can be replayed onto a served
    // response.
    auto update_pending_entries = [&](Vector<PendingEntry>& entries) {
        for (auto& pending_entry : entries) {
            if (!entry_matches_request(request_headers, pending_entry.entry))
                continue;

            auto updated_entry = pending_entry.entry;
            updated_entry.javascript_bytecode_cache = javascript_bytecode_cache;
            updated_entry.javascript_bytecode_cache_vary_key = vary_key;

            auto updated_size = calculate_entry_size(updated_entry);
            if (!updated_size.has_value() || *updated_size > m_limits.maximum_size)
                continue;

            if (*updated_size > pending_entry.size && !ensure_space(*updated_size - pending_entry.size))
                continue;

            m_size -= pending_entry.size;
            pending_entry.entry = move(updated_entry);
            pending_entry.size = *updated_size;
            m_size += pending_entry.size;
        }
    };

    Vector<u64> matching_vary_keys;
    if (auto cache_entries = m_complete_entries.get(cache_key); cache_entries.has_value()) {
        for (auto const& stored_entry : *cache_entries) {
            if (entry_matches_request(request_headers, stored_entry.entry))
                matching_vary_keys.append(stored_entry.entry.vary_key);
        }
    }

    for (auto matching_vary_key : matching_vary_keys) {
        auto cache_entries = m_complete_entries.get(cache_key);
        if (!cache_entries.has_value())
            break;

        auto index = cache_entries->find_first_index_if([&](auto const& stored_entry) { return stored_entry.entry.vary_key == matching_vary_key; });
        if (!index.has_value())
            continue;

        auto updated_entry = cache_entries->at(*index).entry;
        updated_entry.javascript_bytecode_cache = javascript_bytecode_cache;
        updated_entry.javascript_bytecode_cache_vary_key = vary_key;

        auto updated_size = calculate_entry_size(updated_entry);
        if (!updated_size.has_value() || *updated_size > m_limits.maximum_size)
            continue;

        auto old_size = cache_entries->at(*index).size;
        if (*updated_size > old_size) {
            EntryIdentifier identifier { cache_key, matching_vary_key };
            if (!ensure_space(*updated_size - old_size, identifier))
                continue;
        }

        cache_entries = m_complete_entries.get(cache_key);
        VERIFY(cache_entries.has_value());
        index = cache_entries->find_first_index_if([&](auto const& stored_entry) { return stored_entry.entry.vary_key == matching_vary_key; });
        VERIFY(index.has_value());

        m_size -= cache_entries->at(*index).size;
        cache_entries->at(*index).entry = move(updated_entry);
        cache_entries->at(*index).size = *updated_size;
        m_size += *updated_size;
    }

    if (auto cache_entries = m_pending_entries.get(cache_key); cache_entries.has_value())
        update_pending_entries(*cache_entries);
}

}
