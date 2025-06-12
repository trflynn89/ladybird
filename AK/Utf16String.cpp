/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/CharacterTypes.h>
#include <AK/Utf16String.h>

#include <simdutf.h>

namespace AK {

static_assert(sizeof(Detail::ShortString) == sizeof(Detail::Utf16StringData*));

Utf16String Utf16String::from_utf8(StringView utf8_string)
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

Utf16String Utf16String::from_utf16(Utf16View const& utf16_string)
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

bool Utf16String::operator==(Utf16String const& other) const
{
    if (has_short_ascii_storage() && other.has_short_ascii_storage())
        return bit_cast<FlatPtr>(m_value) == bit_cast<FlatPtr>(other.m_value);

    if (has_ascii_storage() && other.has_ascii_storage())
        return ascii_view() == other.ascii_view();

    // We have this branch because the underlying comparison will be a __builtin_memcmp if both sides are UTF-16.
    if (has_long_utf16_storage() && other.has_long_utf16_storage())
        return utf16_view_unsafe() == other.utf16_view_unsafe();

    if (length_in_code_units() != other.length_in_code_units())
        return false;

    for (size_t i = 0; i < length_in_code_units(); ++i) {
        if (code_unit_at(i) != other.code_unit_at(i))
            return false;
    }

    return true;
}

bool Utf16String::equals_ignoring_ascii_case(Utf16String const& other) const
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

void Utf16String::switch_to_utf16_storage() const
{
    auto value = Detail::Utf16StringData::from_utf8(ascii_view(), Detail::Utf16StringData::AllowASCIIStorage::No);

    if (has_long_ascii_storage())
        m_value.data->unref();

    const_cast<Utf16String&>(*this).m_value = { .data = &value.leak_ref() };
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

    if (Utf16View::is_high_surrogate(code_unit)) {
        if (remaining_code_units > 1) {
            auto next_code_unit = *(m_iterator.utf16 + 1);

            if (Utf16View::is_low_surrogate(next_code_unit))
                return Utf16View::decode_surrogate_pair(code_unit, next_code_unit);
        }

        return Utf16View::replacement_code_point;
    }

    if (Utf16View::is_low_surrogate(code_unit))
        return Utf16View::replacement_code_point;

    return static_cast<u32>(code_unit);
}

ErrorOr<void> Formatter<Utf16String>::format(FormatBuilder& builder, Utf16String const& utf16_string)
{
    if (utf16_string.has_long_utf16_storage())
        return builder.builder().try_append(utf16_string.utf16_view());
    return builder.put_string(utf16_string.ascii_view());
}

}
