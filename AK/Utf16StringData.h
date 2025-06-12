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

class Utf16StringData : public RefCounted<Utf16StringData> {
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

    [[nodiscard]] ALWAYS_INLINE bool has_ascii_storage() const { return m_code_unit_length >> Detail::Utf16StringData::UTF16_FLAG == 0; }

    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_units() const { return m_code_unit_length & ~(1uz << Detail::Utf16StringData::UTF16_FLAG); }
    [[nodiscard]] ALWAYS_INLINE size_t length_in_code_points() const
    {
        if (has_ascii_storage())
            return length_in_code_units();
        if (m_code_point_length == NumericLimits<size_t>::max())
            m_code_point_length = calculate_code_point_length();
        return m_code_point_length;
    }

    [[nodiscard]] StringView ascii_view() const
    {
        ASSERT(has_ascii_storage());
        return { m_ascii_data, length_in_code_units() };
    }

    [[nodiscard]] Utf16View utf16_view() const
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

    size_t calculate_code_point_length() const;

    // We store whether this string has ASCII or UTF-16 storage by setting the most significant bit of m_code_unit_length
    // to 1 for UTF-16 storage. This shrinks the size of most UTF-16 string related classes, at the cost of not being
    // allowed to create a string larger than 2**63 - 1.
    size_t m_code_unit_length { 0 };
    mutable size_t m_code_point_length { NumericLimits<size_t>::max() };

    union {
        char m_ascii_data[0];
        char16_t m_utf16_data[0];
    };
};

}
