/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/TypedTransfer.h>
#include <AK/Utf16StringData.h>
#include <AK/Utf8View.h>

#include <simdutf.h>

namespace AK::Detail {

NonnullRefPtr<Utf16StringData> Utf16StringData::create_uninitialized(StorageType storage_type, size_t code_unit_length)
{
    auto allocation_size = storage_type == Utf16StringData::StorageType::ASCII
        ? sizeof(Utf16StringData) + (sizeof(char) * code_unit_length)
        : sizeof(Utf16StringData) + (sizeof(char16_t) * code_unit_length);

    void* slot = malloc(allocation_size);
    VERIFY(slot);

    return adopt_ref(*new (slot) Utf16StringData(storage_type, code_unit_length));
}

NonnullRefPtr<Utf16StringData> Utf16StringData::from_utf8(StringView utf8_string, AllowASCIIStorage allow_ascii_storage)
{
    // Due to internal optimizations, we have an explicit maximum string length of 2**63 - 1.
    VERIFY(utf8_string.length() >> Detail::UTF16_FLAG == 0);

    RefPtr<Utf16StringData> string;

    if (allow_ascii_storage == AllowASCIIStorage::Yes && utf8_string.is_ascii()) {
        string = create_uninitialized(StorageType::ASCII, utf8_string.length());
        TypedTransfer<char>::copy(string->m_ascii_data, utf8_string.characters_without_null_termination(), utf8_string.length());
    } else if (Utf8View view { utf8_string }; view.validate(AllowLonelySurrogates::No)) {
        auto code_unit_length = simdutf::utf16_length_from_utf8(utf8_string.characters_without_null_termination(), utf8_string.length());
        string = create_uninitialized(StorageType::UTF16, code_unit_length);

        auto result = simdutf::convert_utf8_to_utf16(utf8_string.characters_without_null_termination(), utf8_string.length(), string->m_utf16_data);
        VERIFY(result == code_unit_length);
    } else {
        size_t code_unit_length = 0;
        for (auto code_point : view)
            code_unit_length += UnicodeUtils::code_unit_length_for_code_point(code_point);

        string = create_uninitialized(StorageType::UTF16, code_unit_length);
        size_t code_unit_index = 0;

        for (auto code_point : view) {
            (void)UnicodeUtils::code_point_to_utf16(code_point, [&](auto code_unit) {
                string->m_utf16_data[code_unit_index++] = code_unit;
            });
        }
    }

    return string.release_nonnull();
}

NonnullRefPtr<Utf16StringData> Utf16StringData::from_utf16(Utf16View const& utf16_string)
{
    // Due to internal optimizations, we have an explicit maximum string length of 2**63 - 1.
    VERIFY(utf16_string.length_in_code_units() >> Detail::UTF16_FLAG == 0);

    RefPtr<Utf16StringData> string;

    if (utf16_string.has_ascii_storage()) {
        string = create_uninitialized(StorageType::ASCII, utf16_string.length_in_code_units());
        TypedTransfer<char>::copy(string->m_ascii_data, utf16_string.ascii_span().data(), utf16_string.length_in_code_units());
    } else if (utf16_string.is_ascii()) {
        string = create_uninitialized(StorageType::ASCII, utf16_string.length_in_code_units());

        auto result = simdutf::convert_utf16_to_utf8(utf16_string.utf16_span().data(), utf16_string.length_in_code_units(), string->m_ascii_data);
        VERIFY(result == utf16_string.length_in_code_units());
    } else {
        string = create_uninitialized(StorageType::UTF16, utf16_string.length_in_code_units());
        TypedTransfer<char16_t>::copy(string->m_utf16_data, utf16_string.utf16_span().data(), utf16_string.length_in_code_units());
    }

    return string.release_nonnull();
}

size_t Utf16StringData::calculate_code_point_length() const
{
    ASSERT(!has_ascii_storage());

    if (simdutf::validate_utf16(m_utf16_data, length_in_code_units()))
        return simdutf::count_utf16(m_utf16_data, length_in_code_units());

    size_t code_points = 0;
    for ([[maybe_unused]] auto code_point : utf16_view())
        ++code_points;
    return code_points;
}

}
