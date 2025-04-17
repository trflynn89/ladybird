/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibGC/Function.h>
#include <LibWeb/Bindings/ExceptionOrUtils.h>
#include <LibWeb/Fetch/Fetching/FetchedDataReceiver.h>
#include <LibWeb/Fetch/Infrastructure/FetchParams.h>
#include <LibWeb/Fetch/Infrastructure/Task.h>
#include <LibWeb/HTML/Scripting/ExceptionReporter.h>
#include <LibWeb/HTML/Scripting/TemporaryExecutionContext.h>
#include <LibWeb/Streams/ReadableStream.h>
#include <LibWeb/WebIDL/Promise.h>

namespace Web::Fetch::Fetching {

GC_DEFINE_ALLOCATOR(FetchedDataReceiver);

FetchedDataReceiver::FetchedDataReceiver(GC::Ref<Infrastructure::FetchParams const> fetch_params, GC::Ref<Streams::ReadableStream> stream)
    : m_fetch_params(fetch_params)
    , m_stream(stream)
{
}

FetchedDataReceiver::~FetchedDataReceiver() = default;

void FetchedDataReceiver::visit_edges(Visitor& visitor)
{
    Base::visit_edges(visitor);
    visitor.visit(m_fetch_params);
    visitor.visit(m_stream);
    visitor.visit(m_pending_promise);
}

void FetchedDataReceiver::set_pending_promise(GC::Ref<WebIDL::Promise> promise)
{
    auto had_pending_promise = m_pending_promise != nullptr;
    m_pending_promise = promise;

    if (!had_pending_promise && !m_buffer.is_empty()) {
        pull_bytes_into_stream(move(m_buffer));
    }
}

// This implements the parallel steps of the pullAlgorithm in HTTP-network-fetch.
// https://fetch.spec.whatwg.org/#ref-for-in-parallel⑤
void FetchedDataReceiver::handle_network_bytes(ReadonlyBytes bytes, State state)
{
    if (!m_pending_promise) {
        if (state == State::Ongoing)
            m_buffer.append(bytes);
        else
            m_complete = true;

        return;
    }

    // 1. If one or more bytes have been transmitted from response’s message body, then:
    if (!bytes.is_empty()) {
        // 1. Let bytes be the transmitted bytes.

        // FIXME: 2. Let codings be the result of extracting header list values given `Content-Encoding` and response’s header list.
        // FIXME: 3. Increase response’s body info’s encoded size by bytes’s length.
        // FIXME: 4. Set bytes to the result of handling content codings given codings and bytes.
        // FIXME: 5. Increase response’s body info’s decoded size by bytes’s length.
        // FIXME: 6. If bytes is failure, then terminate fetchParams’s controller.

        // 7. Append bytes to buffer.
        pull_bytes_into_stream(MUST(ByteBuffer::copy(bytes)));

        // FIXME: 8. If the size of buffer is larger than an upper limit chosen by the user agent, ask the user agent
        //           to suspend the ongoing fetch.
    }
    // 2. Otherwise, if the bytes transmission for response’s message body is done normally and stream is readable,
    //    then close stream, and abort these in-parallel steps.
    else if (state == State::Complete && m_stream->is_readable()) {
        HTML::TemporaryExecutionContext execution_context { m_stream->realm(), HTML::TemporaryExecutionContext::CallbacksEnabled::Yes };
        m_stream->close();
    }
}

// This implements the parallel steps of the pullAlgorithm in HTTP-network-fetch.
// https://fetch.spec.whatwg.org/#ref-for-in-parallel④
void FetchedDataReceiver::pull_bytes_into_stream(ByteBuffer bytes)
{
    // FIXME: 1. If the size of buffer is smaller than a lower limit chosen by the user agent and the ongoing fetch
    //           is suspended, resume the fetch.

    // 2. Wait until buffer is not empty.
    VERIFY(!bytes.is_empty());

    // 3. Queue a fetch task to run the following steps, with fetchParams’s task destination.
    Infrastructure::queue_fetch_task(
        m_fetch_params->controller(),
        m_fetch_params->task_destination().get<GC::Ref<JS::Object>>(),
        GC::create_function(heap(), [this, bytes = move(bytes)]() mutable {
            HTML::TemporaryExecutionContext execution_context { m_stream->realm(), HTML::TemporaryExecutionContext::CallbacksEnabled::Yes };

            // 1. Pull from bytes buffer into stream.
            if (auto result = m_stream->pull_from_bytes(move(bytes)); result.is_error()) {
                auto throw_completion = Bindings::exception_to_throw_completion(m_stream->vm(), result.release_error());

                dbgln("FetchedDataReceiver: Stream error pulling bytes");
                HTML::report_exception(throw_completion, m_stream->realm());

                return;
            }

            // 2. If stream is errored, then terminate fetchParams’s controller.
            if (m_stream->is_errored())
                m_fetch_params->controller()->terminate();

            // 3. Resolve promise with undefined.
            WebIDL::resolve_promise(m_stream->realm(), *m_pending_promise, JS::js_undefined());
        }));
}

}
