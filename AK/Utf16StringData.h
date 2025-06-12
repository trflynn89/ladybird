/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullRefPtr.h>
#include <AK/NumericLimits.h>
#include <AK/RefCounted.h>
#include <AK/Span.h>
#include <AK/StringView.h>
#include <AK/Types.h>
#include <AK/Utf16View.h>

namespace AK::Detail {

template<typename T, typename U>
[[nodiscard]] bool slow_utf16_equals(T const& string1, U const& string2)
{
    if (string1.length_in_code_units() != string2.length_in_code_units())
        return false;

    for (size_t i = 0; i < string1.length_in_code_units(); ++i) {
        if (string1.code_unit_at(i) != string2.code_unit_at(i))
            return false;
    }

    return true;
}

template<typename T>
[[nodiscard]] bool slow_utf16_equals(T const& string1, StringView string2)
{
    if (string1.length_in_code_units() != string2.length())
        return false;

    for (size_t i = 0; i < string1.length_in_code_units(); ++i) {
        if (string1.code_unit_at(i) != static_cast<char16_t>(string2[i]))
            return false;
    }

    return true;
}

class Utf16StringData final : public RefCounted<Utf16StringData> {
public:
    static constexpr auto UTF16_FLAG = NumericLimits<size_t>::digits() - 1;

    enum class StorageType : u8 {
        ASCII,
        UTF16,
    };

    enum class AllowASCIIStorage : u8 {
        No,
        Yes,
    };

    static NonnullRefPtr<Utf16StringData> from_utf8(StringView, AllowASCIIStorage);
    static NonnullRefPtr<Utf16StringData> from_utf16(Utf16View const&);

    ~Utf16StringData() = default;

    void operator delete(void* ptr)
    {
        free(ptr);
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(Utf16StringData const& other) const
    {
        if (has_ascii_storage() && other.has_ascii_storage())
            return ascii_view() == other.ascii_view();
        if (has_utf16_storage() && other.has_utf16_storage())
            return utf16_view() == other.utf16_view();
        return slow_utf16_equals(*this, other);
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(Utf16View const& other) const
    {
        if (has_utf16_storage())
            return utf16_view() == other;
        return slow_utf16_equals(*this, other);
    }

    [[nodiscard]] ALWAYS_INLINE bool operator==(StringView const& other) const
    {
        if (has_ascii_storage())
            return ascii_view() == other;
        return slow_utf16_equals(*this, other);
    }

    [[nodiscard]] ALWAYS_INLINE bool has_ascii_storage() const { return m_code_unit_length >> Detail::Utf16StringData::UTF16_FLAG == 0; }
    [[nodiscard]] ALWAYS_INLINE bool has_utf16_storage() const { return m_code_unit_length >> Detail::Utf16StringData::UTF16_FLAG != 0; }

    ALWAYS_INLINE u32 hash() const
    {
        if (!m_has_hash)
            m_hash = calculate_hash();
        return m_hash;
    }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_units() const { return m_code_unit_length & ~(1uz << Detail::Utf16StringData::UTF16_FLAG); }
    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_points() const
    {
        if (has_ascii_storage())
            return length_in_code_units();
        if (m_code_point_length == NumericLimits<size_t>::max())
            m_code_point_length = calculate_code_point_length();
        return m_code_point_length;
    }

    [[nodiscard]] ALWAYS_INLINE char16_t code_unit_at(size_t code_unit_offset) const
    {
        if (has_ascii_storage())
            return static_cast<char16_t>(ascii_view()[code_unit_offset]);
        return utf16_view().code_unit_at(code_unit_offset);
    }

    [[nodiscard]] ALWAYS_INLINE StringView ascii_view() const
    {
        ASSERT(has_ascii_storage());
        return { m_ascii_data, length_in_code_units() };
    }

    [[nodiscard]] ALWAYS_INLINE Utf16View utf16_view() const
    {
        ASSERT(!has_ascii_storage());
        return Utf16View { { reinterpret_cast<u16 const*>(m_utf16_data), length_in_code_units() } };
    }

private:
    ALWAYS_INLINE Utf16StringData(StorageType storage_type, size_t code_unit_length)
        : m_code_unit_length(code_unit_length)
    {
        if (storage_type == StorageType::UTF16)
            m_code_unit_length |= 1uz << Detail::Utf16StringData::UTF16_FLAG;
    }

    static NonnullRefPtr<Utf16StringData> create_uninitialized(StorageType storage_type, size_t code_unit_length);

    [[nodiscard]] size_t calculate_code_point_length() const;

    [[nodiscard]] ALWAYS_INLINE u32 calculate_hash() const
    {
        if (has_ascii_storage())
            return ascii_view().hash();
        return utf16_view().hash();
    }

    // We store whether this string has ASCII or UTF-16 storage by setting the most significant bit of m_code_unit_length
    // to 1 for UTF-16 storage. This shrinks the size of most UTF-16 string related classes, at the cost of not being
    // allowed to create a string larger than 2**63 - 1.
    size_t m_code_unit_length { 0 };
    mutable size_t m_code_point_length { NumericLimits<size_t>::max() };

    mutable u32 m_hash { 0 };
    mutable bool m_has_hash { false };

    union alignas(8) {
        char m_ascii_data[0];
        char16_t m_utf16_data[0];
    };
};

}
