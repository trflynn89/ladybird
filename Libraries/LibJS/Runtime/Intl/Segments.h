/*
 * Copyright (c) 2022, Idan Horowitz <idan.horowitz@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Utf16View.h>
#include <LibJS/Runtime/Intl/Segmenter.h>
#include <LibJS/Runtime/Object.h>
#include <LibUnicode/Segmenter.h>

namespace JS::Intl {

class Segments final : public Object {
    JS_OBJECT(Segments, Object);
    GC_DECLARE_ALLOCATOR(Segments);

public:
    static GC::Ref<Segments> create(Realm&, Unicode::Segmenter const&, Utf16String);

    virtual ~Segments() override = default;

    Unicode::Segmenter& segments_segmenter() const { return *m_segments_segmenter; }

    Utf16String const& segments_string() const { return m_segments_string; }

private:
    Segments(Realm&, Unicode::Segmenter const&, Utf16String);

    NonnullOwnPtr<Unicode::Segmenter> m_segments_segmenter; // [[SegmentsSegmenter]]
    Utf16String m_segments_string;                          // [[SegmentsString]]
};

}
