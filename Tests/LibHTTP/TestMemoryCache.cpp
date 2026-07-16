/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibCore/ImmutableBytes.h>
#include <LibHTTP/Cache/MemoryCache.h>
#include <LibHTTP/Cache/Utilities.h>
#include <LibHTTP/HeaderList.h>
#include <LibTest/TestCase.h>
#include <LibURL/Parser.h>

static URL::URL parse_url(StringView url)
{
    return URL::Parser::basic_parse(url).release_value();
}

static NonnullRefPtr<HTTP::HeaderList> create_cacheable_request_headers()
{
    return HTTP::HeaderList::create({
        { "Accept"sv, "*/*"sv },
    });
}

static NonnullRefPtr<HTTP::HeaderList> create_cacheable_response_headers()
{
    return HTTP::HeaderList::create({
        { "Cache-Control"sv, "max-age=60"sv },
    });
}

static NonnullRefPtr<HTTP::HeaderList> create_vary_user_agent_response_headers()
{
    return HTTP::HeaderList::create({
        { "Cache-Control"sv, "max-age=60"sv },
        { "Vary"sv, "User-Agent"sv },
    });
}

static Core::ImmutableBytes immutable_bytes(StringView bytes)
{
    return MUST(Core::ImmutableBytes::copy(bytes.bytes()));
}

static Core::ImmutableBytes immutable_bytes(size_t size, u8 value)
{
    auto bytes = MUST(ByteBuffer::create_uninitialized(size));
    bytes.bytes().fill(value);
    return Core::ImmutableBytes::adopt(move(bytes));
}

static bool store_entry(HTTP::MemoryCache& cache, URL::URL const& url, HTTP::HeaderList const& request_headers, HTTP::HeaderList const& response_headers, StringView body, UnixDateTime request_time = UnixDateTime::now())
{
    return cache.store_entry(url, "GET"sv, request_headers, request_time, 200, "OK", response_headers, immutable_bytes(body));
}

TEST_CASE(memory_cache_returns_fresh_entries_and_unique_misses)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/resource"sv);
    auto missing_url = parse_url("https://example.com/missing"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(store_entry(*cache, url, *request_headers, *response_headers, "body"sv));

    auto entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    EXPECT_EQ(entry->response_body.bytes(), "body"sv.bytes());
    EXPECT(!cache->open_entry(missing_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(memory_cache_honors_lookup_cache_modes)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/resource"sv);
    auto stale_url = parse_url("https://example.com/stale"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();
    auto stale_response_headers = HTTP::HeaderList::create({
        { "Cache-Control"sv, "max-age=1"sv },
    });

    EXPECT(store_entry(*cache, url, *request_headers, *response_headers, "body"sv));
    EXPECT(store_entry(*cache, stale_url, *request_headers, *stale_response_headers, "stale"sv, UnixDateTime::now() - AK::Duration::from_seconds(120)));

    EXPECT(cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(!cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Reload).has_value());
    EXPECT(!cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::NoCache).has_value());
    EXPECT(cache->open_entry(stale_url, "GET"sv, *request_headers, HTTP::CacheMode::ForceCache).has_value());
    EXPECT(cache->open_entry(stale_url, "GET"sv, *request_headers, HTTP::CacheMode::OnlyIfCached).has_value());
}

TEST_CASE(memory_cache_does_not_store_no_store_requests_or_responses)
{
    auto cache = HTTP::MemoryCache::create();
    auto request_url = parse_url("https://example.com/request-no-store"sv);
    auto response_url = parse_url("https://example.com/response-no-store"sv);
    auto request_headers = HTTP::HeaderList::create({
        { "Cache-Control"sv, "no-store"sv },
    });
    auto response_headers = create_cacheable_response_headers();
    auto response_no_store_headers = HTTP::HeaderList::create({
        { "Cache-Control"sv, "no-store"sv },
    });
    auto cacheable_request_headers = create_cacheable_request_headers();

    EXPECT(!store_entry(*cache, request_url, *request_headers, *response_headers, "body"sv));
    EXPECT(!store_entry(*cache, response_url, *cacheable_request_headers, *response_no_store_headers, "body"sv));
    EXPECT(!cache->open_entry(request_url, "GET"sv, *cacheable_request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(!cache->open_entry(response_url, "GET"sv, *cacheable_request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(memory_cache_matches_vary_headers)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/resource"sv);
    auto ladybird_request_headers = HTTP::HeaderList::create({
        { "User-Agent"sv, "Ladybird"sv },
    });
    auto other_request_headers = HTTP::HeaderList::create({
        { "User-Agent"sv, "Other"sv },
    });
    auto response_headers = create_vary_user_agent_response_headers();

    EXPECT(store_entry(*cache, url, *ladybird_request_headers, *response_headers, "ladybird"sv));

    EXPECT(cache->open_entry(url, "GET"sv, *ladybird_request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(!cache->open_entry(url, "GET"sv, *other_request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(memory_cache_replaces_existing_variants)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/resource"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(store_entry(*cache, url, *request_headers, *response_headers, "first"sv));
    auto original_entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(original_entry.has_value());
    EXPECT(store_entry(*cache, url, *request_headers, *response_headers, "second"sv));

    auto entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    EXPECT_EQ(entry->response_body.bytes(), "second"sv.bytes());
    EXPECT_EQ(original_entry->response_body.bytes(), "first"sv.bytes());
}

TEST_CASE(expiring_one_memory_cache_variant_preserves_other_variants)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/resource"sv);
    auto old_request_headers = HTTP::HeaderList::create({
        { "User-Agent"sv, "Old"sv },
    });
    auto fresh_request_headers = HTTP::HeaderList::create({
        { "User-Agent"sv, "Fresh"sv },
    });
    auto old_response_headers = HTTP::HeaderList::create({
        { "Cache-Control"sv, "max-age=1"sv },
        { "Vary"sv, "User-Agent"sv },
    });
    auto fresh_response_headers = create_vary_user_agent_response_headers();

    EXPECT(store_entry(*cache, url, *old_request_headers, *old_response_headers, "old"sv, UnixDateTime::now() - AK::Duration::from_seconds(120)));
    EXPECT(store_entry(*cache, url, *fresh_request_headers, *fresh_response_headers, "fresh"sv));

    EXPECT(!cache->open_entry(url, "GET"sv, *old_request_headers, HTTP::CacheMode::Default).has_value());

    auto fresh_entry = cache->open_entry(url, "GET"sv, *fresh_request_headers, HTTP::CacheMode::Default);
    VERIFY(fresh_entry.has_value());
    EXPECT_EQ(fresh_entry->response_body.bytes(), "fresh"sv.bytes());
}

TEST_CASE(memory_cache_evicts_the_least_recently_used_entry)
{
    auto cache = HTTP::MemoryCache::create({
        .maximum_size = 240,
        .maximum_entry_body_size = 100,
    });
    auto first_url = parse_url("https://example.com/first"sv);
    auto second_url = parse_url("https://example.com/second"sv);
    auto third_url = parse_url("https://example.com/third"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(cache->store_entry(first_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(80, '1')));
    EXPECT(cache->store_entry(second_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(80, '2')));

    EXPECT(cache->open_entry(first_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(cache->store_entry(third_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(80, '3')));

    EXPECT(cache->open_entry(first_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(!cache->open_entry(second_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(cache->open_entry(third_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(memory_cache_accounts_for_associated_data)
{
    auto cache = HTTP::MemoryCache::create({
        .maximum_size = 240,
        .maximum_entry_body_size = 100,
    });
    auto first_url = parse_url("https://example.com/first"sv);
    auto second_url = parse_url("https://example.com/second"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(cache->store_entry(first_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(80, '1')));
    EXPECT(cache->store_entry(second_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(80, '2')));

    cache->update_javascript_bytecode_cache(first_url, "GET"sv, *request_headers, 0, immutable_bytes(20, 'j'));

    auto first_entry = cache->open_entry(first_url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(first_entry.has_value());
    VERIFY(first_entry->javascript_bytecode_cache.has_value());
    EXPECT_EQ(first_entry->javascript_bytecode_cache->size(), 20u);
    EXPECT(!cache->open_entry(second_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(memory_cache_rejects_oversized_entries)
{
    auto body_limited_cache = HTTP::MemoryCache::create({
        .maximum_size = 1024,
        .maximum_entry_body_size = 3,
    });
    auto total_limited_cache = HTTP::MemoryCache::create({
        .maximum_size = 20,
        .maximum_entry_body_size = 20,
    });
    auto url = parse_url("https://example.com/resource"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(!store_entry(*body_limited_cache, url, *request_headers, *response_headers, "four"sv));
    EXPECT(!store_entry(*total_limited_cache, url, *request_headers, *response_headers, "x"sv));
    EXPECT(!body_limited_cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT(!total_limited_cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
}

TEST_CASE(opened_memory_cache_entries_remain_valid_after_eviction)
{
    auto cache = HTTP::MemoryCache::create({
        .maximum_size = 140,
        .maximum_entry_body_size = 100,
    });
    auto first_url = parse_url("https://example.com/first"sv);
    auto second_url = parse_url("https://example.com/second"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();

    EXPECT(store_entry(*cache, first_url, *request_headers, *response_headers, "retained body"sv));
    auto opened_entry = cache->open_entry(first_url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(opened_entry.has_value());

    EXPECT(cache->store_entry(second_url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK", *response_headers, immutable_bytes(100, '2')));
    EXPECT(!cache->open_entry(first_url, "GET"sv, *request_headers, HTTP::CacheMode::Default).has_value());
    EXPECT_EQ(opened_entry->response_body.bytes(), "retained body"sv.bytes());
}

TEST_CASE(javascript_bytecode_cache_round_trips_with_memory_cache_entry)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/script.js"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();
    auto bytecode = immutable_bytes("cached bytecode"sv);

    cache->create_entry(url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK"sv, *response_headers, bytecode, 0);
    cache->finalize_entry(url, "GET"sv, *request_headers, 200, *response_headers, immutable_bytes("console.log('hello');"sv));

    auto entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    VERIFY(entry->javascript_bytecode_cache.has_value());
    EXPECT_EQ(entry->javascript_bytecode_cache->bytes(), bytecode.bytes());
    EXPECT_EQ(entry->javascript_bytecode_cache_vary_key, Optional<u64> { 0 });
}

TEST_CASE(javascript_bytecode_cache_update_matches_memory_cache_vary_headers)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/script.js"sv);
    auto script_request_headers = create_cacheable_request_headers();
    auto cache_request_headers = HTTP::HeaderList::create({
        { "Accept"sv, "*/*"sv },
        { "User-Agent"sv, "Ladybird"sv },
    });
    auto response_headers = create_vary_user_agent_response_headers();
    auto bytecode = immutable_bytes("generated bytecode"sv);
    auto vary_key = HTTP::create_vary_key(*cache_request_headers, *response_headers);

    cache->create_entry(url, "GET"sv, *cache_request_headers, UnixDateTime::now(), 200, "OK"sv, *response_headers);
    cache->finalize_entry(url, "GET"sv, *cache_request_headers, 200, *response_headers, immutable_bytes("console.log('hello');"sv));

    cache->update_javascript_bytecode_cache(url, "GET"sv, *script_request_headers, vary_key, bytecode);

    auto entry = cache->open_entry(url, "GET"sv, *cache_request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    EXPECT(!entry->javascript_bytecode_cache.has_value());
    EXPECT(!entry->javascript_bytecode_cache_vary_key.has_value());

    cache->update_javascript_bytecode_cache(url, "GET"sv, *cache_request_headers, vary_key, bytecode);

    entry = cache->open_entry(url, "GET"sv, *cache_request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    VERIFY(entry->javascript_bytecode_cache.has_value());
    EXPECT_EQ(entry->javascript_bytecode_cache->bytes(), bytecode.bytes());
    EXPECT_EQ(entry->javascript_bytecode_cache_vary_key, Optional<u64> { vary_key });
}

TEST_CASE(javascript_bytecode_cache_can_be_added_after_memory_cache_entry_is_complete)
{
    auto cache = HTTP::MemoryCache::create();
    auto url = parse_url("https://example.com/script.js"sv);
    auto request_headers = create_cacheable_request_headers();
    auto response_headers = create_cacheable_response_headers();
    auto bytecode = immutable_bytes("generated bytecode"sv);

    cache->create_entry(url, "GET"sv, *request_headers, UnixDateTime::now(), 200, "OK"sv, *response_headers);
    cache->finalize_entry(url, "GET"sv, *request_headers, 200, *response_headers, immutable_bytes("console.log('hello');"sv));

    auto entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    EXPECT(!entry->javascript_bytecode_cache.has_value());
    EXPECT(!entry->javascript_bytecode_cache_vary_key.has_value());

    cache->update_javascript_bytecode_cache(url, "GET"sv, *request_headers, 0, bytecode);

    entry = cache->open_entry(url, "GET"sv, *request_headers, HTTP::CacheMode::Default);
    VERIFY(entry.has_value());
    VERIFY(entry->javascript_bytecode_cache.has_value());
    EXPECT_EQ(entry->javascript_bytecode_cache->bytes(), bytecode.bytes());
    EXPECT_EQ(entry->javascript_bytecode_cache_vary_key, Optional<u64> { 0 });
}
