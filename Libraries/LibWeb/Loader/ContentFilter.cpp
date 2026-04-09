/*
 * Copyright (c) 2021, Andreas Kling <andreas@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWeb/Loader/ContentFilter.h>
#include <LibWeb/RustFFI.h>

namespace Web {

ContentFilter& ContentFilter::the()
{
    static ContentFilter filter;
    return filter;
}

ContentFilter::ContentFilter() = default;

ContentFilter::~ContentFilter()
{
    if (m_engine)
        FFI::adblock_engine_free(m_engine);
}

ErrorOr<ByteBuffer> ContentFilter::serialize_filter_list(ReadonlyBytes filter_list_text)
{
    u8* data = nullptr;
    size_t length = 0;

    if (!FFI::adblock_serialize_filter_list(filter_list_text.data(), filter_list_text.size(), &data, &length))
        return Error::from_string_literal("Failed to serialize content filter list");

    auto buffer = TRY(ByteBuffer::copy(data, length));
    FFI::adblock_free_serialized(data, length);

    return buffer;
}

void ContentFilter::set_filter_list(ReadonlyBytes filter_list)
{
    if (m_engine) {
        FFI::adblock_engine_free(m_engine);
        m_engine = nullptr;
    }

    if (filter_list.is_empty())
        return;

    m_engine = FFI::adblock_engine_create(filter_list.data(), filter_list.size());
}

void ContentFilter::set_serialized_filter_list(ReadonlyBytes serialized)
{
    if (m_engine) {
        FFI::adblock_engine_free(m_engine);
        m_engine = nullptr;
    }

    if (serialized.is_empty())
        return;

    m_engine = FFI::adblock_engine_deserialize(serialized.data(), serialized.size());
}

ErrorOr<void> ContentFilter::set_patterns(ReadonlySpan<String> patterns)
{
    if (patterns.is_empty()) {
        set_filter_list({});
        return {};
    }

    auto filter_list = TRY(String::join('\n', patterns));
    set_filter_list(filter_list.bytes());

    return {};
}

bool ContentFilter::is_filtered(URL::URL const& url) const
{
    if (!m_engine || !filtering_enabled())
        return false;
    if (url.scheme() == "data")
        return false;

    auto url_string = url.to_string();
    return FFI::adblock_engine_is_filtered(m_engine, url_string.bytes().data(), url_string.bytes().size(), nullptr, 0);
}

}
