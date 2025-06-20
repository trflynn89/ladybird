/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/CharacterTypes.h>
#include <AK/NonnullRefPtr.h>
#include <AK/StringBase.h>
#include <AK/StringView.h>
#include <AK/Utf16StringData.h>
#include <AK/Utf16View.h>

namespace AK::Detail {

class Utf16StringBase {
public:
    static constexpr auto SHORT_STRING_FLAG = StringBase::SHORT_STRING_FLAG;

    constexpr Utf16StringBase()
        : Utf16StringBase(ShortString::create_empty())
    {
    }

    explicit constexpr Utf16StringBase(ShortString short_string)
        : m_value { .short_ascii_string = short_string }
    {
    }

    ALWAYS_INLINE explicit Utf16StringBase(NonnullRefPtr<Utf16StringData const> value)
        : m_value { .data = &value.leak_ref() }
    {
    }

    ALWAYS_INLINE Utf16StringBase(Utf16StringBase const& other)
        : m_value(other.m_value)
    {
        if (has_long_storage())
            data_without_union_member_assertion()->ref();
    }

    constexpr Utf16StringBase(Utf16StringBase&& other)
        : m_value(other.m_value)
    {
        other.m_value = { .short_ascii_string = ShortString::create_empty() };
    }

    constexpr ~Utf16StringBase()
    {
        if (!is_constant_evaluated())
            destroy_string();
    }

    ALWAYS_INLINE Utf16StringBase& operator=(Utf16StringBase const& other)
    {
        if (&other != this) {
            if (has_long_storage())
                data_without_union_member_assertion()->unref();

            m_value = other.m_value;

            if (has_long_storage())
                data_without_union_member_assertion()->ref();
        }

        return *this;
    }

    ALWAYS_INLINE Utf16StringBase& operator=(Utf16StringBase&& other)
    {
        if (has_long_storage())
            data_without_union_member_assertion()->unref();

        m_value = exchange(other.m_value, { .short_ascii_string = ShortString::create_empty() });
        return *this;
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(Utf16StringBase const& other) const
    {
        if (has_short_ascii_storage() && other.has_short_ascii_storage())
            return bit_cast<FlatPtr>(m_value) == bit_cast<FlatPtr>(other.m_value);

        if (has_long_storage() && other.has_long_storage())
            return *data_without_union_member_assertion() == *other.data_without_union_member_assertion();

        return slow_utf16_equals(*this, other);
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(Utf16View const& other) const
    {
        if (has_long_storage())
            return *data_without_union_member_assertion() == other;
        return slow_utf16_equals(other, ascii_view());
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(StringView other) const
    {
        if (has_long_storage())
            return *data_without_union_member_assertion() == other;
        return ascii_view() == other;
    }

    [[nodiscard]] bool equals_ignoring_ascii_case(Utf16View const& other) const
    {
        if (has_long_utf16_storage())
            return utf16_view_unsafe().equals_ignoring_case(other);

        if (length_in_code_units() != other.length_in_code_units())
            return false;

        for (size_t i = 0; i < length_in_code_units(); ++i) {
            if (to_ascii_lowercase(code_unit_at(i)) != to_ascii_lowercase(other.code_unit_at(i)))
                return false;
        }

        return true;
    }

    [[nodiscard]] bool equals_ignoring_ascii_case(Utf16StringBase const& other) const
    {
        if (has_ascii_storage() && other.has_ascii_storage())
            return ascii_view().equals_ignoring_ascii_case(other.ascii_view());

        if (length_in_code_units() != other.length_in_code_units())
            return false;

        for (size_t i = 0; i < length_in_code_units(); ++i) {
            if (to_ascii_lowercase(code_unit_at(i)) != to_ascii_lowercase(other.code_unit_at(i)))
                return false;
        }

        return true;
    }

    template<typename... Ts>
    [[nodiscard]] ALWAYS_INLINE bool is_one_of(Ts&&... strings) const
    {
        return (this->operator==(forward<Ts>(strings)) || ...);
    }

    template<typename... Ts>
    [[nodiscard]] ALWAYS_INLINE bool is_one_of_ignoring_ascii_case(Ts&&... strings) const
    {
        return (this->equals_ignoring_ascii_case(forward<Ts>(strings)) || ...);
    }

    [[nodiscard]] ALWAYS_INLINE u32 hash() const
    {
        if (has_short_ascii_storage())
            return ascii_view().hash();
        return data_without_union_member_assertion()->hash();
    }

    [[nodiscard]] ALWAYS_INLINE bool is_empty() const { return length_in_code_units() == 0uz; }

    [[nodiscard]] ALWAYS_INLINE bool is_ascii() const
    {
        if (has_ascii_storage())
            return true;
        return utf16_view_unsafe().is_ascii();
    }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_units() const
    {
        if (has_short_ascii_storage())
            return short_ascii_string_without_union_member_assertion().byte_count();
        return data_without_union_member_assertion()->length_in_code_units();
    }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_points() const
    {
        if (has_short_ascii_storage())
            return short_ascii_string_without_union_member_assertion().byte_count();
        return data_without_union_member_assertion()->length_in_code_points();
    }

    [[nodiscard]] ALWAYS_INLINE char16_t code_unit_at(size_t code_unit_offset) const
    {
        if (has_ascii_storage())
            return static_cast<char16_t>(ascii_view()[code_unit_offset]);
        return utf16_view_unsafe().code_unit_at(code_unit_offset);
    }

    [[nodiscard]] ALWAYS_INLINE u32 code_point_at(size_t code_unit_offset) const
    {
        if (has_ascii_storage())
            return static_cast<u32>(ascii_view()[code_unit_offset]);
        return utf16_view_unsafe().code_point_at(code_unit_offset);
    }

    [[nodiscard]] ALWAYS_INLINE size_t code_unit_offset_of(size_t code_point_offset) const
    {
        if (has_ascii_storage())
            return code_point_offset;
        return utf16_view_unsafe().code_unit_offset_of(code_point_offset);
    }

    [[nodiscard]] ALWAYS_INLINE size_t code_point_offset_of(size_t code_unit_offset) const
    {
        if (has_ascii_storage())
            return code_unit_offset;
        return utf16_view_unsafe().code_point_offset_of(code_unit_offset);
    }

    [[nodiscard]] ALWAYS_INLINE StringView ascii_view() const&
    {
        if (has_short_ascii_storage())
            return short_ascii_string_without_union_member_assertion().bytes();
        return data_without_union_member_assertion()->ascii_view();
    }

    [[nodiscard]] ALWAYS_INLINE Utf16View utf16_view() const&
    {
        ensure_utf16_storage();
        return utf16_view_unsafe();
    }

    StringView ascii_view() const&& = delete;
    Utf16View utf16_view() const&& = delete;

    // This is primarily interesting to unit tests.
    [[nodiscard]] constexpr bool has_short_ascii_storage() const
    {
        if (is_constant_evaluated())
            return (m_value.short_ascii_string.byte_count_and_short_string_flag & SHORT_STRING_FLAG) != 0;
        return (short_ascii_string_without_union_member_assertion().byte_count_and_short_string_flag & SHORT_STRING_FLAG) != 0;
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] ALWAYS_INLINE bool has_long_ascii_storage() const
    {
        if (has_short_ascii_storage())
            return false;
        return data_without_union_member_assertion()->has_ascii_storage();
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] ALWAYS_INLINE bool has_ascii_storage() const
    {
        return has_short_ascii_storage() || has_long_ascii_storage();
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] ALWAYS_INLINE bool has_long_utf16_storage() const
    {
        if (has_short_ascii_storage())
            return false;
        return data_without_union_member_assertion()->has_utf16_storage();
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] ALWAYS_INLINE bool has_long_storage() const
    {
        return !has_short_ascii_storage();
    }

    [[nodiscard]] ALWAYS_INLINE Utf16StringData const* data(Badge<Utf16FlyString>) const
    {
        VERIFY(has_long_storage());
        return data_without_union_member_assertion();
    }

    ALWAYS_INLINE void set_data(Badge<Utf16FlyString>, Utf16StringData const* data)
    {
        auto const** this_data = __builtin_launder(&m_value.data);
        (*this_data) = data;
        (*this_data)->ref();
    }

    [[nodiscard]] constexpr FlatPtr raw(Badge<Utf16FlyString>) const { return bit_cast<FlatPtr>(m_value); }

protected:
    [[nodiscard]] ALWAYS_INLINE Utf16View utf16_view_unsafe() const { return data_without_union_member_assertion()->utf16_view(); }

    ALWAYS_INLINE void ensure_utf16_storage() const
    {
        if (has_long_utf16_storage())
            return;

        auto value = Utf16StringData::from_utf8(ascii_view(), Utf16StringData::AllowASCIIStorage::No);

        if (has_long_ascii_storage())
            data_without_union_member_assertion()->unref();

        const_cast<Utf16StringBase&>(*this).m_value = { .data = &value.leak_ref() };
    }

    ALWAYS_INLINE void destroy_string() const
    {
        if (has_long_storage())
            data_without_union_member_assertion()->unref();
    }

    // This is technically **invalid**! See StringBase for details.
    ALWAYS_INLINE ShortString const& short_ascii_string_without_union_member_assertion() const { return *__builtin_launder(&m_value.short_ascii_string); }
    ALWAYS_INLINE Utf16StringData const* data_without_union_member_assertion() const { return *__builtin_launder(&m_value.data); }

    union {
        ShortString short_ascii_string;
        Utf16StringData const* data;
    } m_value;
};

}
