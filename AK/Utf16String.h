/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Format.h>
#include <AK/NonnullRefPtr.h>
#include <AK/StringBase.h>
#include <AK/StringView.h>
#include <AK/Utf16StringData.h>
#include <AK/Utf16View.h>

namespace AK {

class Utf16StringIterator {
public:
    Utf16StringIterator() = default;
    ~Utf16StringIterator() = default;

    Utf16StringIterator& operator++();
    u32 operator*() const;

    [[nodiscard]] ALWAYS_INLINE bool operator==(Utf16StringIterator const& other) const
    {
        // Note that this also protects against iterators with different underlying storage.
        if (m_remaining_code_units != other.m_remaining_code_units)
            return false;

        if (has_ascii_storage())
            return m_iterator.ascii == other.m_iterator.ascii;
        return m_iterator.utf16 == other.m_iterator.utf16;
    }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_units()
    {
        if (has_ascii_storage())
            return 1;
        return *(*this) < Utf16View::first_supplementary_plane_code_point ? 1uz : 2uz;
    }

private:
    friend class Utf16String;

    Utf16StringIterator(char const* iterator, size_t length)
        : m_iterator { .ascii = iterator }
        , m_remaining_code_units(length)
    {
    }

    Utf16StringIterator(char16_t const* iterator, size_t length)
        : m_iterator { .utf16 = iterator }
        , m_remaining_code_units(length)
    {
        m_remaining_code_units |= 1uz << Detail::Utf16StringData::UTF16_FLAG;
    }

    bool has_ascii_storage() const { return m_remaining_code_units >> Detail::Utf16StringData::UTF16_FLAG == 0; }
    size_t remaining_code_units() const { return m_remaining_code_units & ~(1uz << Detail::Utf16StringData::UTF16_FLAG); }

    union {
        char const* ascii;
        char16_t const* utf16;
    } m_iterator { .ascii = nullptr };

    // Just like Utf16StringData, we store whether this string has ASCII or UTF-16 storage by setting the most
    // significant bit of m_code_unit_length for UTF-16 storage.
    size_t m_remaining_code_units { 0 };
};

// Utf16String is a strongly owned sequence of Unicode code points encoded as UTF-16.
//
// The data may or may not be heap-allocated, and may or may not be reference counted. As a memory optimization, if the
// UTF-16 string is entirely ASCII, the string may be stored as 8-bit bytes. If any operation requires 16-bit code units,
// the storage is changed to match.
class Utf16String {
public:
    [[nodiscard]] static Utf16String from_utf8(StringView);
    [[nodiscard]] static Utf16String from_utf16(Utf16View const&);

    constexpr Utf16String()
        : Utf16String(Detail::ShortString::create_empty())
    {
    }

    ALWAYS_INLINE Utf16String(Utf16String const& other)
        : m_value(other.m_value)
    {
        if (!has_short_ascii_storage())
            m_value.data->ref();
    }

    constexpr Utf16String(Utf16String&& other)
        : m_value(other.m_value)
    {
        other.m_value = { .short_ascii_string = Detail::ShortString::create_empty() };
    }

    constexpr ~Utf16String()
    {
        if (!is_constant_evaluated())
            destroy_string();
    }

    ALWAYS_INLINE Utf16String& operator=(Utf16String const& other)
    {
        if (&other != this) {
            if (!has_short_ascii_storage())
                m_value.data->unref();

            m_value = other.m_value;

            if (!has_short_ascii_storage())
                m_value.data->ref();
        }

        return *this;
    }

    ALWAYS_INLINE Utf16String& operator=(Utf16String&& other)
    {
        if (!has_short_ascii_storage())
            m_value.data->unref();

        m_value = exchange(other.m_value, { .short_ascii_string = Detail::ShortString::create_empty() });
        return *this;
    }

    [[nodiscard]] bool operator==(Utf16String const&) const;
    [[nodiscard]] bool equals_ignoring_ascii_case(Utf16String const&) const;

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
            return m_value.short_ascii_string.byte_count();
        return m_value.data->length_in_code_units();
    }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_points() const
    {
        if (has_short_ascii_storage())
            return m_value.short_ascii_string.byte_count();
        return m_value.data->length_in_code_points();
    }

    [[nodiscard]] Utf16StringIterator begin() const
    {
        if (has_ascii_storage()) {
            auto view = ascii_view();
            return { view.characters_without_null_termination(), view.length() };
        }

        auto view = utf16_view_unsafe();
        return { view.char_data(), view.length_in_code_units() };
    }

    [[nodiscard]] Utf16StringIterator end() const
    {
        if (has_ascii_storage()) {
            auto view = ascii_view();
            return { view.characters_without_null_termination() + view.length(), 0 };
        }

        auto view = utf16_view_unsafe();
        return { view.char_data() + view.length_in_code_units(), 0 };
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

    [[nodiscard]] ALWAYS_INLINE StringView ascii_view() const
    {
        if (has_short_ascii_storage())
            return m_value.short_ascii_string.bytes();
        return m_value.data->ascii_view();
    }

    [[nodiscard]] ALWAYS_INLINE Utf16View utf16_view() const
    {
        switch_to_utf16_storage_if_needed();
        return utf16_view_unsafe();
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] constexpr bool has_short_ascii_storage() const
    {
        if (is_constant_evaluated())
            return (m_value.short_ascii_string.byte_count_and_short_string_flag & Detail::StringBase::SHORT_STRING_FLAG) != 0;
        return (short_ascii_string_without_union_member_assertion().byte_count_and_short_string_flag & Detail::StringBase::SHORT_STRING_FLAG) != 0;
    }

    // This is primarily interesting to unit tests.
    [[nodiscard]] ALWAYS_INLINE bool has_long_ascii_storage() const
    {
        if (has_short_ascii_storage())
            return false;
        return m_value.data->has_ascii_storage();
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
        return !m_value.data->has_ascii_storage();
    }

private:
    explicit constexpr Utf16String(Detail::ShortString short_string)
        : m_value { .short_ascii_string = short_string }
    {
    }

    ALWAYS_INLINE explicit Utf16String(NonnullRefPtr<Detail::Utf16StringData const> value)
        : m_value { .data = &value.leak_ref() }
    {
    }

    [[nodiscard]] ALWAYS_INLINE Utf16View utf16_view_unsafe() const { return m_value.data->utf16_view(); }

    ALWAYS_INLINE void switch_to_utf16_storage_if_needed() const
    {
        if (!has_long_utf16_storage())
            switch_to_utf16_storage();
    }

    void switch_to_utf16_storage() const;

    ALWAYS_INLINE void destroy_string() const
    {
        if (!has_short_ascii_storage()) {
            if (auto const* data = m_value.data)
                data->unref();
        }
    }

    // This is technically **invalid**! See StringBase for details.
    ALWAYS_INLINE Detail::ShortString short_ascii_string_without_union_member_assertion() const { return *__builtin_launder(&m_value.short_ascii_string); }

    union {
        Detail::ShortString short_ascii_string;
        Detail::Utf16StringData const* data;
    } m_value;
};

template<>
struct Formatter<Utf16String> : Formatter<FormatString> {
    ErrorOr<void> format(FormatBuilder&, Utf16String const&);
};

}

[[nodiscard]] ALWAYS_INLINE Utf16String operator""_utf16(char const* string, size_t length)
{
    return Utf16String::from_utf8({ string, length });
}

[[nodiscard]] ALWAYS_INLINE Utf16String operator""_utf16(char16_t const* string, size_t length)
{
    return Utf16String::from_utf16(Utf16View { { reinterpret_cast<u16 const*>(string), length } });
}
