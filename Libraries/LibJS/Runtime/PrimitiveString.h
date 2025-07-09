/*
 * Copyright (c) 2020-2025, Andreas Kling <andreas@ladybird.org>
 * Copyright (c) 2022, Linus Groh <linusg@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/String.h>
#include <AK/StringView.h>
#include <AK/Utf16String.h>
#include <LibGC/CellAllocator.h>
#include <LibJS/Forward.h>
#include <LibJS/Heap/Cell.h>
#include <LibJS/Runtime/Completion.h>
#include <LibJS/Runtime/Value.h>

namespace JS {

class JS_API PrimitiveString : public Cell {
    GC_CELL(PrimitiveString, Cell);
    GC_DECLARE_ALLOCATOR(PrimitiveString);

public:
    [[nodiscard]] static GC::Ref<PrimitiveString> create(VM&, StringView);
    [[nodiscard]] static GC::Ref<PrimitiveString> create(VM&, Utf16String);
    [[nodiscard]] static GC::Ref<PrimitiveString> create(VM&, Utf16View const&);
    [[nodiscard]] static GC::Ref<PrimitiveString> create(VM&, Utf16FlyString const&);
    [[nodiscard]] static GC::Ref<PrimitiveString> create(VM&, PrimitiveString&, PrimitiveString&);

    virtual ~PrimitiveString();

    PrimitiveString(PrimitiveString const&) = delete;
    PrimitiveString& operator=(PrimitiveString const&) = delete;

    [[nodiscard]] bool is_empty() const
    {
        // NOTE: We never make an empty rope string.
        if (m_is_rope)
            return false;
        return m_string.is_empty();
    }

    Utf16String const& string() const { return m_string; }
    Utf16View view() const { return m_string; }

    StringView ascii_view() const
    {
        VERIFY(m_string.has_ascii_storage());
        return m_string.ascii_view();
    }

    [[nodiscard]] size_t length_in_code_units() const { return m_string.length_in_code_units(); }

    ThrowCompletionOr<Optional<Value>> get(VM&, PropertyKey const&) const;

    [[nodiscard]] bool operator==(PrimitiveString const&) const;

protected:
    enum class RopeTag { Rope };
    explicit PrimitiveString(RopeTag)
        : m_is_rope(true)
    {
    }

    mutable bool m_is_rope { false };

    mutable Utf16String m_string;

private:
    friend class RopeString;

    explicit PrimitiveString(Utf16String);

    void resolve_rope_if_needed() const;
};

class RopeString final : public PrimitiveString {
    GC_CELL(RopeString, PrimitiveString);
    GC_DECLARE_ALLOCATOR(RopeString);

public:
    virtual ~RopeString() override;

private:
    friend class PrimitiveString;

    explicit RopeString(GC::Ref<PrimitiveString>, GC::Ref<PrimitiveString>);

    virtual void visit_edges(Visitor&) override;

    void resolve() const;

    mutable GC::Ptr<PrimitiveString> m_lhs;
    mutable GC::Ptr<PrimitiveString> m_rhs;
};

}
