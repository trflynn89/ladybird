/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Utf16String.h>

#include <simdutf.h>

namespace AK {

static_assert(sizeof(Detail::ShortString) == sizeof(Detail::Utf16StringData*));

Utf16String Utf16String::from_utf8_without_validation(StringView utf8_string)
{
    if (utf8_string.length() <= Detail::MAX_SHORT_STRING_BYTE_COUNT && utf8_string.is_ascii()) {
        Utf16String string;
        string.m_value.short_ascii_string = Detail::ShortString::create_with_byte_count(utf8_string.length());

        auto result = utf8_string.bytes().copy_to(string.m_value.short_ascii_string.storage);
        VERIFY(result == utf8_string.length());

        return string;
    }

    return Utf16String { Detail::Utf16StringData::from_utf8(utf8_string, Detail::Utf16StringData::AllowASCIIStorage::Yes) };
}

Utf16String Utf16String::from_utf16_without_validation(Utf16View const& utf16_string)
{
    if (utf16_string.length_in_code_units() <= Detail::MAX_SHORT_STRING_BYTE_COUNT && utf16_string.is_ascii()) {
        Utf16String string;
        string.m_value.short_ascii_string = Detail::ShortString::create_with_byte_count(utf16_string.length_in_code_units());

        auto result = simdutf::convert_utf16_to_utf8(utf16_string.char_data(), utf16_string.length_in_code_units(), reinterpret_cast<char*>(string.m_value.short_ascii_string.storage));
        VERIFY(result == utf16_string.length_in_code_units());

        return string;
    }

    return Utf16String { Detail::Utf16StringData::from_utf16(utf16_string) };
}

Utf16String Utf16String::from_string_builder_without_validation(StringBuilder& builder)
{
    return Utf16String { Detail::Utf16StringData::from_string_builder(builder) };
}

Utf16StringIterator& Utf16StringIterator::operator++()
{
    auto remaining_code_units = this->remaining_code_units();
    VERIFY(remaining_code_units > 0);

    if (has_ascii_storage()) {
        ++m_iterator.ascii;
        --m_remaining_code_units;
    } else {
        auto length = min(length_in_code_units(), remaining_code_units);

        m_iterator.utf16 += length;
        m_remaining_code_units -= length;
    }

    return *this;
}

u32 Utf16StringIterator::operator*() const
{
    auto remaining_code_units = this->remaining_code_units();
    VERIFY(remaining_code_units > 0);

    if (has_ascii_storage())
        return *m_iterator.ascii;

    auto code_unit = *m_iterator.utf16;

    if (UnicodeUtils::is_utf16_high_surrogate(code_unit)) {
        if (remaining_code_units > 1) {
            auto next_code_unit = *(m_iterator.utf16 + 1);

            if (UnicodeUtils::is_utf16_low_surrogate(next_code_unit))
                return UnicodeUtils::decode_utf16_surrogate_pair(code_unit, next_code_unit);
        }

        return UnicodeUtils::REPLACEMENT_CODE_POINT;
    }

    if (UnicodeUtils::is_utf16_low_surrogate(code_unit))
        return UnicodeUtils::REPLACEMENT_CODE_POINT;

    return static_cast<u32>(code_unit);
}

ErrorOr<void> Formatter<Utf16String>::format(FormatBuilder& builder, Utf16String const& utf16_string)
{
    if (utf16_string.has_long_utf16_storage())
        return builder.builder().try_append(utf16_string.utf16_view());
    return builder.put_string(utf16_string.ascii_view());
}

}
