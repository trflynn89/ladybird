/*
 * Copyright (c) 2021, Andreas Kling <andreas@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/String.h>
#include <AK/Vector.h>
#include <LibURL/URL.h>
#include <LibWeb/Export.h>

namespace Web::FFI {

struct AdblockEngine;

}

namespace Web {

class WEB_API ContentFilter {
public:
    static ContentFilter& the();

    bool filtering_enabled() const { return m_filtering_enabled; }
    void set_filtering_enabled(bool const enabled) { m_filtering_enabled = enabled; }

    ErrorOr<void> set_patterns(ReadonlySpan<String>);
    void set_filter_list(ReadonlyBytes);
    bool is_filtered(URL::URL const&) const;

private:
    ContentFilter();
    ~ContentFilter();

    FFI::AdblockEngine* m_engine { nullptr };
    bool m_filtering_enabled { true };
};

}
