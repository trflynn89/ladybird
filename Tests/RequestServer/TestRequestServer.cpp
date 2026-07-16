/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Array.h>
#include <AK/Atomic.h>
#include <AK/QuickSort.h>
#include <AK/Random.h>
#include <LibCore/DirIterator.h>
#include <LibCore/EventLoop.h>
#include <LibCore/File.h>
#include <LibCore/StandardPaths.h>
#include <LibCore/System.h>
#include <LibCore/TCPServer.h>
#include <LibFileSystem/FileSystem.h>
#include <LibHTTP/Cache/DiskCache.h>
#include <LibIPC/Transport.h>
#include <LibRequests/Request.h>
#include <LibRequests/RequestClient.h>
#include <LibTest/TestCase.h>
#include <LibThreading/Thread.h>
#include <LibURL/Parser.h>
#include <RequestServer/CURL.h>
#include <RequestServer/CacheCoordinator.h>
#include <RequestServer/ConnectionFromClient.h>
#include <RequestServer/ResourceSubstitutionMap.h>

namespace RequestServer {

OwnPtr<ResourceSubstitutionMap> g_resource_substitution_map;

}

enum class UseDiskCache {
    No,
    Yes,
};

class RequestServerFixture {
public:
    explicit RequestServerFixture(UseDiskCache use_disk_cache = UseDiskCache::No)
    {
        MUST(RequestServer::initialize_libcurl());

        if (use_disk_cache == UseDiskCache::Yes)
            m_cache_root = ByteString::formatted("{}/ladybird-request-server-test-{}", Core::StandardPaths::tempfile_directory(), generate_random_uuid());

        auto paired_transport = MUST(IPC::Transport::create_paired());
        m_client_transport = MUST(paired_transport.remote_handle.create_transport());

        m_request_server_thread = Threading::Thread::construct("RequestServer test"sv, [this, server_transport = move(paired_transport.local), cache_root = m_cache_root]() mutable -> intptr_t {
            Core::EventLoop event_loop;
            auto event_loop_reference = Core::EventLoop::current_weak();
            m_request_server_event_loop.store(event_loop_reference.ptr());

            RequestServer::ConnectionFromClient::ConnectionMap connections;
            Optional<HTTP::DiskCache> disk_cache;
            if (cache_root.has_value())
                disk_cache = MUST(HTTP::DiskCache::create(HTTP::DiskCache::Mode::Normal, LexicalPath { *cache_root })).release_value();

            RequestServer::CacheCoordinator cache_coordinator { disk_cache };
            auto connection = RequestServer::ConnectionFromClient::construct(
                move(server_transport),
                RequestServer::ConnectionFromClient::IsPrimaryConnection::Yes,
                RequestServer::IsPrivate::No,
                connections,
                cache_coordinator,
                ByteString {});

            auto result = event_loop.exec();
            connection->shutdown();
            return result;
        });
        m_request_server_thread->start();
    }

    ~RequestServerFixture()
    {
        for (auto& request_client : m_child_request_clients)
            request_client->shutdown();
        m_child_request_clients.clear();

        if (m_request_client)
            m_request_client->shutdown();
        m_request_client.clear();

        {
            auto event_loop = m_request_server_event_loop.load()->take();
            if (event_loop) {
                event_loop->deferred_invoke([] {
                    Core::EventLoop::current().quit(0);
                });
                event_loop->wake();
            }
        }
        MUST(m_request_server_thread->join());

        if (m_cache_root.has_value())
            MUST(FileSystem::remove(*m_cache_root, FileSystem::RecursionMode::Allowed));
    }

    Requests::RequestClient& request_client()
    {
        if (!m_request_client) {
            m_request_client = adopt_ref(*new Requests::RequestClient(m_client_transport.release_nonnull()));
            configure_request_client(*m_request_client);
        }
        return *m_request_client;
    }

    NonnullRefPtr<Requests::RequestClient> create_client(RequestServer::IsPrivate is_private)
    {
        auto response = request_client().send_sync_but_allow_failure<Messages::RequestServer::ConnectNewClient>(is_private);
        VERIFY(response);

        auto transport = MUST(response->take_handle().create_transport());
        auto request_client = adopt_ref(*new Requests::RequestClient(move(transport)));
        configure_request_client(request_client);
        m_child_request_clients.append(request_client);
        return request_client;
    }

    void reset_private_memory_cache()
    {
        request_client().reset_private_memory_cache();
    }

    ByteString const& cache_root() const
    {
        return m_cache_root.value();
    }

private:
    static void configure_request_client(Requests::RequestClient& request_client)
    {
        request_client.on_retrieve_http_cookie = [](auto const&, auto) {
            return String {};
        };
    }

    OwnPtr<IPC::Transport> m_client_transport;
    RefPtr<Requests::RequestClient> m_request_client;
    Vector<NonnullRefPtr<Requests::RequestClient>> m_child_request_clients;
    RefPtr<Threading::Thread> m_request_server_thread;
    Atomic<Core::WeakEventLoopReference*> m_request_server_event_loop { nullptr };
    Optional<ByteString> m_cache_root;
};

class TestHTTPServer {
public:
    TestHTTPServer()
        : m_server(MUST(Core::TCPServer::try_create()))
    {
        MUST(m_server->listen({ 127, 0, 0, 1 }, 0));

        m_server->on_ready_to_accept = [&] {
            m_pending_connections.append(MUST(m_server->accept()));
            ++m_request_count;
        };
    }

    ~TestHTTPServer()
    {
        for (auto& response_thread : m_response_threads)
            MUST(response_thread->join());
    }

    URL::URL url(StringView path) const
    {
        return URL::Parser::basic_parse(ByteString::formatted("http://127.0.0.1:{}/{}", m_server->local_port().value(), path)).release_value();
    }

    size_t request_count() const { return m_request_count; }

    void send_response(ReadonlyBytes body, StringView cache_control = "max-age=3600"sv, StringView additional_headers = {}, Optional<size_t> declared_content_length = {})
    {
        VERIFY(!m_pending_connections.is_empty());
        auto socket = m_pending_connections.take_first();
        MUST(socket->set_blocking(true));

        Array<u8, 4096> request_buffer;
        ByteBuffer request;
        while (!StringView { request.data(), request.size() }.contains("\r\n\r\n"sv)) {
            auto bytes = MUST(socket->read_some(request_buffer));
            VERIFY(!bytes.is_empty());
            request.append(bytes);
        }

        auto headers = ByteString::formatted(
            "HTTP/1.1 200 OK\r\nCache-Control: {}\r\nContent-Length: {}\r\nConnection: close\r\n{}\r\n",
            cache_control,
            declared_content_length.value_or(body.size()),
            additional_headers);
        auto response = MUST(ByteBuffer::create_uninitialized(headers.length() + body.size()));
        response.overwrite(0, headers.characters(), headers.length());
        response.overwrite(headers.length(), body.data(), body.size());

        auto response_file = MUST(Core::File::adopt_fd(MUST(Core::System::dup(socket->fd())), Core::File::OpenMode::Write));
        socket->close();

        auto response_thread = Threading::Thread::construct("HTTP test response"sv, [response_file = move(response_file), response = move(response)]() mutable -> intptr_t {
            MUST(response_file->write_until_depleted(response));
            response_file->close();
            return 0;
        });
        response_thread->start();
        m_response_threads.append(move(response_thread));
    }

    void close_connection_without_response()
    {
        VERIFY(!m_pending_connections.is_empty());
        m_pending_connections.take_first()->close();
    }

private:
    NonnullRefPtr<Core::TCPServer> m_server;
    Vector<NonnullOwnPtr<Core::TCPSocket>> m_pending_connections;
    Vector<NonnullRefPtr<Threading::Thread>> m_response_threads;
    size_t m_request_count { 0 };
};

struct BufferedRequestResult {
    bool finished { false };
    Optional<Requests::NetworkError> network_error;
    RefPtr<HTTP::HeaderList> response_headers;
    Optional<u32> response_code;
    Optional<u64> javascript_bytecode_cache_vary_key;
    Requests::CameFromCache came_from_cache { Requests::CameFromCache::No };
    Optional<Core::ImmutableBytes> body;
};

static NonnullRefPtr<Requests::Request> start_request(Requests::RequestClient& request_client, URL::URL const& url, HTTP::CacheMode cache_mode, BufferedRequestResult& result, Optional<HTTP::HeaderList const&> request_headers = {})
{
    auto request = request_client.start_request("GET", url, request_headers, {}, cache_mode);
    VERIFY(request);
    request->set_buffered_request_finished_callback([&result](auto, auto const&, auto const& network_error, auto response_headers, auto response_code, auto, auto, auto javascript_bytecode_cache_vary_key, auto came_from_cache, auto body) {
        result.network_error = network_error;
        result.response_headers = move(response_headers);
        result.response_code = response_code;
        result.javascript_bytecode_cache_vary_key = javascript_bytecode_cache_vary_key;
        result.came_from_cache = came_from_cache;
        result.body = move(body);
        result.finished = true;
    });
    return request.release_nonnull();
}

static void wait_for_request(Core::EventLoop& event_loop, TestHTTPServer const& http_server, size_t request_count)
{
    event_loop.spin_until([&] { return http_server.request_count() >= request_count; });
}

static void wait_for_completion(Core::EventLoop& event_loop, BufferedRequestResult const& result)
{
    event_loop.spin_until([&] { return result.finished; });
}

static void perform_network_request(Core::EventLoop& event_loop, TestHTTPServer& http_server, Requests::RequestClient& request_client, URL::URL const& url, BufferedRequestResult& result, ReadonlyBytes body, HTTP::CacheMode cache_mode = HTTP::CacheMode::Default, StringView cache_control = "max-age=3600"sv, StringView additional_headers = {}, Optional<HTTP::HeaderList const&> request_headers = {}, Optional<size_t> declared_content_length = {})
{
    auto expected_request_count = http_server.request_count() + 1;
    auto request = start_request(request_client, url, cache_mode, result, request_headers);
    wait_for_request(event_loop, http_server, expected_request_count);
    http_server.send_response(body, cache_control, additional_headers, declared_content_length);
    wait_for_completion(event_loop, result);
}

static void perform_cache_only_request(Core::EventLoop& event_loop, TestHTTPServer& http_server, Requests::RequestClient& request_client, URL::URL const& url, BufferedRequestResult& result, Optional<HTTP::HeaderList const&> request_headers = {})
{
    auto initial_request_count = http_server.request_count();
    auto request = start_request(request_client, url, HTTP::CacheMode::OnlyIfCached, result, request_headers);
    event_loop.spin_until([&] { return result.finished || http_server.request_count() != initial_request_count; });

    if (!result.finished) {
        http_server.close_connection_without_response();
        wait_for_completion(event_loop, result);
    }
}

static ErrorOr<void> collect_paths_recursively(ByteString const& path, Vector<ByteString>& paths)
{
    Core::DirIterator iterator(path, Core::DirIterator::Flags::SkipDots);
    while (iterator.has_next()) {
        auto full_path = iterator.next_full_path();
        paths.append(full_path);
        if (FileSystem::is_directory(full_path))
            TRY(collect_paths_recursively(full_path, paths));
    }
    if (iterator.has_error())
        return iterator.error();
    return {};
}

static ErrorOr<Vector<ByteString>> collect_paths(ByteString const& path)
{
    Vector<ByteString> paths;
    TRY(collect_paths_recursively(path, paths));
    quick_sort(paths);
    return paths;
}

TEST_CASE(private_cache_only_miss_does_not_reach_the_network)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);

    BufferedRequestResult result;
    perform_cache_only_request(event_loop, http_server, private_client, http_server.url("cache-only-miss"sv), result);

    EXPECT(result.network_error.has_value());
    EXPECT_EQ(http_server.request_count(), 0u);
}

TEST_CASE(private_clients_share_cached_responses_and_honor_vary)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto first_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto second_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto english_headers = HTTP::HeaderList::create({ { "Accept-Language", "en" } });
    auto french_headers = HTTP::HeaderList::create({ { "Accept-Language", "fr" } });
    auto url = http_server.url("shared-vary"sv);

    BufferedRequestResult network_result;
    perform_network_request(event_loop, http_server, first_private_client, url, network_result, "shared body"sv.bytes(), HTTP::CacheMode::Default, "max-age=3600"sv, "Vary: Accept-Language\r\n"sv, *english_headers);
    EXPECT(!network_result.network_error.has_value());
    EXPECT_EQ(network_result.came_from_cache, Requests::CameFromCache::No);
    EXPECT(network_result.javascript_bytecode_cache_vary_key.has_value());

    BufferedRequestResult cached_result;
    perform_cache_only_request(event_loop, http_server, second_private_client, url, cached_result, *english_headers);
    EXPECT(!cached_result.network_error.has_value());
    EXPECT_EQ(cached_result.came_from_cache, Requests::CameFromCache::Yes);
    EXPECT_EQ(cached_result.body->bytes(), "shared body"sv.bytes());
    EXPECT_EQ(http_server.request_count(), 1u);

    BufferedRequestResult vary_miss_result;
    perform_cache_only_request(event_loop, http_server, second_private_client, url, vary_miss_result, *french_headers);
    EXPECT(vary_miss_result.network_error.has_value());
    EXPECT_EQ(http_server.request_count(), 1u);

    auto empty_url = http_server.url("empty"sv);
    BufferedRequestResult empty_network_result;
    perform_network_request(event_loop, http_server, first_private_client, empty_url, empty_network_result, {});
    EXPECT(!empty_network_result.network_error.has_value());

    BufferedRequestResult empty_cached_result;
    perform_cache_only_request(event_loop, http_server, second_private_client, empty_url, empty_cached_result);
    EXPECT(!empty_cached_result.network_error.has_value());
    EXPECT_EQ(empty_cached_result.came_from_cache, Requests::CameFromCache::Yes);
    EXPECT(empty_cached_result.body->is_empty());
    EXPECT_EQ(http_server.request_count(), 2u);
}

TEST_CASE(normal_and_private_cache_entries_are_isolated)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server { UseDiskCache::Yes };
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);

    auto private_url = http_server.url("private-entry"sv);
    BufferedRequestResult private_network_result;
    perform_network_request(event_loop, http_server, private_client, private_url, private_network_result, "private"sv.bytes());
    EXPECT(!private_network_result.network_error.has_value());

    BufferedRequestResult normal_miss_result;
    perform_cache_only_request(event_loop, http_server, request_server.request_client(), private_url, normal_miss_result);
    EXPECT(normal_miss_result.network_error.has_value());

    auto normal_url = http_server.url("normal-entry"sv);
    BufferedRequestResult normal_network_result;
    perform_network_request(event_loop, http_server, request_server.request_client(), normal_url, normal_network_result, "normal"sv.bytes());
    EXPECT(!normal_network_result.network_error.has_value());

    BufferedRequestResult private_miss_result;
    perform_cache_only_request(event_loop, http_server, private_client, normal_url, private_miss_result);
    EXPECT(private_miss_result.network_error.has_value());
    EXPECT_EQ(http_server.request_count(), 2u);
}

TEST_CASE(private_cache_rejects_no_store_failed_and_incomplete_responses)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);

    auto no_store_url = http_server.url("no-store"sv);
    BufferedRequestResult no_store_result;
    perform_network_request(event_loop, http_server, private_client, no_store_url, no_store_result, "not stored"sv.bytes(), HTTP::CacheMode::Default, "no-store"sv);
    EXPECT(!no_store_result.network_error.has_value());

    BufferedRequestResult no_store_miss_result;
    perform_cache_only_request(event_loop, http_server, private_client, no_store_url, no_store_miss_result);
    EXPECT(no_store_miss_result.network_error.has_value());

    auto failed_url = http_server.url("failed"sv);
    BufferedRequestResult failed_result;
    auto failed_request_count = http_server.request_count() + 1;
    auto failed_request = start_request(private_client, failed_url, HTTP::CacheMode::Default, failed_result);
    wait_for_request(event_loop, http_server, failed_request_count);
    http_server.close_connection_without_response();
    wait_for_completion(event_loop, failed_result);
    EXPECT(failed_result.network_error.has_value());

    BufferedRequestResult failed_miss_result;
    perform_cache_only_request(event_loop, http_server, private_client, failed_url, failed_miss_result);
    EXPECT(failed_miss_result.network_error.has_value());

    auto incomplete_url = http_server.url("incomplete"sv);
    BufferedRequestResult incomplete_result;
    perform_network_request(event_loop, http_server, private_client, incomplete_url, incomplete_result, "short"sv.bytes(), HTTP::CacheMode::Default, "max-age=3600"sv, {}, {}, 20);
    EXPECT(incomplete_result.network_error.has_value());

    BufferedRequestResult incomplete_miss_result;
    perform_cache_only_request(event_loop, http_server, private_client, incomplete_url, incomplete_miss_result);
    EXPECT(incomplete_miss_result.network_error.has_value());
    EXPECT_EQ(http_server.request_count(), 3u);
}

TEST_CASE(private_reload_and_no_cache_replace_cached_responses)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto url = http_server.url("replacement"sv);

    BufferedRequestResult initial_result;
    perform_network_request(event_loop, http_server, private_client, url, initial_result, "initial"sv.bytes());

    BufferedRequestResult reload_result;
    perform_network_request(event_loop, http_server, private_client, url, reload_result, "reload"sv.bytes(), HTTP::CacheMode::Reload);
    EXPECT_EQ(reload_result.came_from_cache, Requests::CameFromCache::No);

    BufferedRequestResult after_reload_result;
    perform_cache_only_request(event_loop, http_server, private_client, url, after_reload_result);
    EXPECT_EQ(after_reload_result.body->bytes(), "reload"sv.bytes());

    BufferedRequestResult no_cache_result;
    perform_network_request(event_loop, http_server, private_client, url, no_cache_result, "no-cache"sv.bytes(), HTTP::CacheMode::NoCache);
    EXPECT_EQ(no_cache_result.came_from_cache, Requests::CameFromCache::No);

    BufferedRequestResult after_no_cache_result;
    perform_cache_only_request(event_loop, http_server, private_client, url, after_no_cache_result);
    EXPECT_EQ(after_no_cache_result.body->bytes(), "no-cache"sv.bytes());
    EXPECT_EQ(http_server.request_count(), 3u);
}

TEST_CASE(private_cached_responses_resume_after_pipe_backpressure)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto url = http_server.url("backpressure"sv);
    auto body = MUST(ByteBuffer::create_uninitialized(1 * MiB));
    body.bytes().fill('x');

    BufferedRequestResult network_result;
    perform_network_request(event_loop, http_server, private_client, url, network_result, body.bytes());
    EXPECT(!network_result.network_error.has_value());

    BufferedRequestResult cached_result;
    perform_cache_only_request(event_loop, http_server, private_client, url, cached_result);
    EXPECT(!cached_result.network_error.has_value());
    EXPECT_EQ(cached_result.came_from_cache, Requests::CameFromCache::Yes);
    EXPECT_EQ(cached_result.body->bytes(), body.bytes());
    EXPECT_EQ(http_server.request_count(), 1u);
}

TEST_CASE(private_oversized_responses_are_delivered_but_not_cached)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto url = http_server.url("oversized"sv);
    auto oversized_body = MUST(ByteBuffer::create_uninitialized(8 * MiB + 1));
    oversized_body.bytes().fill('x');

    BufferedRequestResult network_result;
    perform_network_request(event_loop, http_server, private_client, url, network_result, oversized_body.bytes());
    EXPECT(!network_result.network_error.has_value());
    EXPECT_EQ(network_result.body->size(), oversized_body.size());

    BufferedRequestResult miss_result;
    perform_cache_only_request(event_loop, http_server, private_client, url, miss_result);
    EXPECT(miss_result.network_error.has_value());
    EXPECT_EQ(http_server.request_count(), 1u);
}

TEST_CASE(private_cache_rotation_isolates_sessions)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto old_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto url = http_server.url("old-session"sv);

    BufferedRequestResult network_result;
    perform_network_request(event_loop, http_server, old_private_client, url, network_result, "old session"sv.bytes());

    request_server.reset_private_memory_cache();
    auto new_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);

    BufferedRequestResult new_session_miss_result;
    perform_cache_only_request(event_loop, http_server, new_private_client, url, new_session_miss_result);
    EXPECT(new_session_miss_result.network_error.has_value());

    BufferedRequestResult old_session_hit_result;
    perform_cache_only_request(event_loop, http_server, old_private_client, url, old_session_hit_result);
    EXPECT(!old_session_hit_result.network_error.has_value());
    EXPECT_EQ(old_session_hit_result.body->bytes(), "old session"sv.bytes());
    EXPECT_EQ(http_server.request_count(), 1u);
}

TEST_CASE(old_in_flight_requests_cannot_populate_a_new_private_session)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server;
    auto old_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto url = http_server.url("in-flight"sv);

    BufferedRequestResult in_flight_result;
    auto in_flight_request = start_request(old_private_client, url, HTTP::CacheMode::Default, in_flight_result);
    wait_for_request(event_loop, http_server, 1);

    request_server.reset_private_memory_cache();
    auto new_private_client = request_server.create_client(RequestServer::IsPrivate::Yes);

    http_server.send_response("late response"sv.bytes());
    wait_for_completion(event_loop, in_flight_result);
    EXPECT(!in_flight_result.network_error.has_value());

    BufferedRequestResult new_session_miss_result;
    perform_cache_only_request(event_loop, http_server, new_private_client, url, new_session_miss_result);
    EXPECT(new_session_miss_result.network_error.has_value());

    BufferedRequestResult old_session_hit_result;
    perform_cache_only_request(event_loop, http_server, old_private_client, url, old_session_hit_result);
    EXPECT(!old_session_hit_result.network_error.has_value());
    EXPECT_EQ(old_session_hit_result.body->bytes(), "late response"sv.bytes());
    EXPECT_EQ(http_server.request_count(), 1u);
}

TEST_CASE(private_requests_do_not_create_disk_cache_files)
{
    Core::EventLoop event_loop;
    TestHTTPServer http_server;
    RequestServerFixture request_server { UseDiskCache::Yes };
    auto private_client = request_server.create_client(RequestServer::IsPrivate::Yes);
    auto paths_before_request = TRY_OR_FAIL(collect_paths(request_server.cache_root()));

    BufferedRequestResult network_result;
    perform_network_request(event_loop, http_server, private_client, http_server.url("memory-only"sv), network_result, "private response"sv.bytes());
    EXPECT(!network_result.network_error.has_value());

    auto paths_after_request = TRY_OR_FAIL(collect_paths(request_server.cache_root()));
    EXPECT_EQ(paths_after_request, paths_before_request);
}
