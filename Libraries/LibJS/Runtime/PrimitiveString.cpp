/*
 * Copyright (c) 2020-2025, Andreas Kling <andreas@ladybird.org>
 * Copyright (c) 2022, Linus Groh <linusg@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/CharacterTypes.h>
#include <AK/StringBuilder.h>
#include <AK/UnicodeUtils.h>
#include <AK/Utf16FlyString.h>
#include <AK/Utf16View.h>
#include <AK/Utf8View.h>
#include <LibJS/Runtime/AbstractOperations.h>
#include <LibJS/Runtime/GlobalObject.h>
#include <LibJS/Runtime/PrimitiveString.h>
#include <LibJS/Runtime/PropertyKey.h>
#include <LibJS/Runtime/VM.h>
#include <LibJS/Runtime/Value.h>

namespace JS {

GC_DEFINE_ALLOCATOR(PrimitiveString);
GC_DEFINE_ALLOCATOR(RopeString);

GC::Ref<PrimitiveString> PrimitiveString::create(VM& vm, StringView string)
{
    return create(vm, Utf16String::from_utf8(string));
}

GC::Ref<PrimitiveString> PrimitiveString::create(VM& vm, Utf16String string)
{
    if (string.is_empty())
        return vm.empty_string();

    if (string.length_in_code_units() == 1) {
        u16 code_unit = string.code_unit_at(0);
        if (is_ascii(code_unit))
            return vm.single_ascii_character_string(static_cast<u8>(code_unit));
    }

    auto& string_cache = vm.string_cache();
    if (auto it = string_cache.find(string); it != string_cache.end())
        return *it->value;

    auto new_string = vm.heap().allocate<PrimitiveString>(string);
    string_cache.set(move(string), new_string);
    return *new_string;
}

GC::Ref<PrimitiveString> PrimitiveString::create(VM& vm, Utf16View const& string)
{
    return create(vm, Utf16String::from_utf16(string));
}

GC::Ref<PrimitiveString> PrimitiveString::create(VM& vm, Utf16FlyString const& string)
{
    return create(vm, string.to_utf16_string());
}

GC::Ref<PrimitiveString> PrimitiveString::create(VM& vm, PrimitiveString& lhs, PrimitiveString& rhs)
{
    // We're here to concatenate two strings into a new rope string.
    // However, if any of them are empty, no rope is required.

    bool lhs_empty = lhs.is_empty();
    bool rhs_empty = rhs.is_empty();

    if (lhs_empty && rhs_empty)
        return vm.empty_string();

    if (lhs_empty)
        return rhs;

    if (rhs_empty)
        return lhs;

    return vm.heap().allocate<RopeString>(lhs, rhs);
}

PrimitiveString::PrimitiveString(Utf16String string)
    : m_string(move(string))
{
}

PrimitiveString::~PrimitiveString()
{
    vm().string_cache().remove(m_string);
}

bool PrimitiveString::operator==(PrimitiveString const& other) const
{
    if (this == &other)
        return true;
    return m_string == other.m_string;
}

ThrowCompletionOr<Optional<Value>> PrimitiveString::get(VM& vm, PropertyKey const& property_key) const
{
    if (property_key.is_symbol())
        return Optional<Value> {};

    if (property_key.is_string()) {
        if (property_key.as_string() == vm.names.length.as_string())
            return Value(static_cast<double>(length_in_code_units()));
    }

    auto index = canonical_numeric_index_string(property_key, CanonicalIndexMode::IgnoreNumericRoundtrip);
    if (!index.is_index())
        return Optional<Value> {};
    if (length_in_code_units() <= index.as_index())
        return Optional<Value> {};

    return create(vm, view().substring_view(index.as_index(), 1));
}

void PrimitiveString::resolve_rope_if_needed() const
{
    if (!m_is_rope)
        return;

    auto const& rope_string = static_cast<RopeString const&>(*this);
    rope_string.resolve();
}

void RopeString::resolve() const
{
    // This vector will hold all the pieces of the rope that need to be assembled into the resolved string.
    Vector<PrimitiveString const*> pieces;
    size_t length_in_code_units = 0;

    // NOTE: We traverse the rope tree without using recursion, since we'd run out of stack space quickly when handling
    //       a long sequence of unresolved concatenations.
    Vector<PrimitiveString const*> stack;
    stack.append(m_rhs);
    stack.append(m_lhs);

    while (!stack.is_empty()) {
        auto const* current = stack.take_last();
        if (current->m_is_rope) {
            auto& current_rope_string = static_cast<RopeString const&>(*current);
            stack.append(current_rope_string.m_rhs);
            stack.append(current_rope_string.m_lhs);
            continue;
        }

        length_in_code_units += current->length_in_code_units();
        pieces.append(current);
    }

    StringBuilder builder(StringBuilder::Mode::UTF16, length_in_code_units);
    for (auto const* current : pieces)
        builder.append(current->m_string);

    m_string = builder.to_utf16_string_without_validation();
    m_is_rope = false;
    m_lhs = nullptr;
    m_rhs = nullptr;
}

RopeString::RopeString(GC::Ref<PrimitiveString> lhs, GC::Ref<PrimitiveString> rhs)
    : PrimitiveString(RopeTag::Rope)
    , m_lhs(lhs)
    , m_rhs(rhs)
{
}

RopeString::~RopeString() = default;

void RopeString::visit_edges(Cell::Visitor& visitor)
{
    Base::visit_edges(visitor);
    visitor.visit(m_lhs);
    visitor.visit(m_rhs);
}

}
