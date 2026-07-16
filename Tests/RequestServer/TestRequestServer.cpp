/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Atomic.h>
#include <LibCore/EventLoop.h>
#include <LibCore/TCPServer.h>
#include <LibHTTP/Cache/DiskCache.h>
#include <LibIPC/Transport.h>
#include <LibRequests/Request.h>
#include <LibRequests/RequestClient.h>
#include <LibTest/TestCase.h>
#include <LibThreading/Thread.h>
#include <LibURL/Parser.h>
#include <RequestServer/CURL.h>
#include <RequestServer/ConnectionFromClient.h>
#include <RequestServer/ResourceSubstitutionMap.h>

namespace RequestServer {

OwnPtr<ResourceSubstitutionMap> g_resource_substitution_map;

}

class RequestServerFixture {
public:
    RequestServerFixture()
    {
        auto paired_transport = MUST(IPC::Transport::create_paired());
        m_client_transport = MUST(paired_transport.remote_handle.create_transport());

        m_request_server_thread = Threading::Thread::construct("RequestServer test"sv, [this, server_transport = move(paired_transport.local)]() mutable -> intptr_t {
            Core::EventLoop event_loop;
            auto event_loop_reference = Core::EventLoop::current_weak();
            m_request_server_event_loop.store(event_loop_reference.ptr());

            RequestServer::ConnectionFromClient::ConnectionMap connections;
            Optional<HTTP::DiskCache&> disk_cache;

            auto connection = RequestServer::ConnectionFromClient::construct(
                move(server_transport),
                RequestServer::ConnectionFromClient::IsPrimaryConnection::Yes,
                RequestServer::IsPrivate::No,
                connections,
                disk_cache,
                ByteString {});

            return event_loop.exec();
        });
        m_request_server_thread->start();
    }

    ~RequestServerFixture()
    {
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
    }

    Requests::RequestClient& request_client()
    {
        if (!m_request_client) {
            m_request_client = adopt_ref(*new Requests::RequestClient(m_client_transport.release_nonnull()));
            m_request_client->on_retrieve_http_cookie = [](auto const&, auto) {
                return String {};
            };
        }
        return *m_request_client;
    }

private:
    OwnPtr<IPC::Transport> m_client_transport;
    RefPtr<Requests::RequestClient> m_request_client;
    RefPtr<Threading::Thread> m_request_server_thread;
    Atomic<Core::WeakEventLoopReference*> m_request_server_event_loop { nullptr };
};

TEST_CASE(cache_only_miss_does_not_reach_the_network_without_a_cache)
{
    Core::EventLoop event_loop;
    TRY_OR_FAIL(RequestServer::initialize_libcurl());

    auto http_server = TRY_OR_FAIL(Core::TCPServer::try_create());
    TRY_OR_FAIL(http_server->listen({ 127, 0, 0, 1 }, 0));

    size_t request_count = 0;
    http_server->on_ready_to_accept = [&] {
        auto socket = MUST(http_server->accept());
        ++request_count;
        MUST(socket->write_until_depleted("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"sv.bytes()));
    };

    RequestServerFixture request_server;
    auto url = URL::Parser::basic_parse(ByteString::formatted("http://127.0.0.1:{}/cache-only-miss", http_server->local_port().value())).release_value();
    auto request = request_server.request_client().start_request("GET", url, {}, {}, HTTP::CacheMode::OnlyIfCached);

    bool request_finished = false;
    Optional<Requests::NetworkError> network_error;
    request->set_buffered_request_finished_callback([&](auto, auto const&, auto const& error, auto, auto, auto const&, auto, auto, auto, auto) {
        network_error = error;
        request_finished = true;
    });

    event_loop.spin_until([&] { return request_finished; });

    EXPECT(network_error.has_value());
    EXPECT_EQ(request_count, 0u);
}
