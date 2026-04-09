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

ErrorOr<void> ContentFilter::set_patterns(ReadonlySpan<String> patterns)
{
    if (m_engine) {
        FFI::adblock_engine_free(m_engine);
        m_engine = nullptr;
    }

    if (patterns.is_empty())
        return {};

    auto filter_list = TRY(String::join('\n', patterns));
    m_engine = FFI::adblock_engine_create(filter_list.bytes().data(), filter_list.bytes().size());

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
