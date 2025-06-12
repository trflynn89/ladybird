/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/Format.h>
#include <AK/NonnullRefPtr.h>
#include <AK/StringView.h>
#include <AK/Traits.h>
#include <AK/UnicodeUtils.h>
#include <AK/Utf16StringBase.h>
#include <AK/Utf16StringData.h>
#include <AK/Utf16View.h>
#include <AK/Utf8View.h>

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
        return UnicodeUtils::code_unit_length_for_code_point(**this);
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
class [[nodiscard]] Utf16String : public Detail::Utf16StringBase {
public:
    using Utf16StringBase::Utf16StringBase;

    explicit constexpr Utf16String(Utf16StringBase&& base)
        : Utf16StringBase(move(base))
    {
    }

    ALWAYS_INLINE static Utf16String from_utf8(StringView utf8_string)
    {
        VERIFY(Utf8View { utf8_string }.validate());
        return from_utf8_without_validation(utf8_string);
    }

    ALWAYS_INLINE static ErrorOr<Utf16String> try_from_utf8(StringView utf8_string)
    {
        if (!Utf8View { utf8_string }.validate())
            return Error::from_string_literal("Input was not valid UTF-8");
        return from_utf8_without_validation(utf8_string);
    }

    ALWAYS_INLINE static Utf16String from_utf16(Utf16View const& utf16_string)
    {
        VERIFY(utf16_string.validate(Utf16View::AllowInvalidCodeUnits::Yes));
        return from_utf16_without_validation(utf16_string);
    }

    ALWAYS_INLINE static ErrorOr<Utf16String> try_from_utf16(Utf16View const& utf16_string)
    {
        if (!utf16_string.validate(Utf16View::AllowInvalidCodeUnits::Yes))
            return Error::from_string_literal("Input was not valid UTF-16");
        return from_utf16_without_validation(utf16_string);
    }

    static Utf16String from_utf8_without_validation(StringView);
    static Utf16String from_utf16_without_validation(Utf16View const&);

    template<typename T>
    requires(IsOneOf<RemoveCVReference<T>, Utf16String>)
    static Utf16String from_utf16(T&&) = delete;

    template<typename T>
    requires(IsOneOf<RemoveCVReference<T>, Utf16String>)
    static ErrorOr<Utf16String> try_from_utf16(T&&) = delete;

    template<typename T>
    requires(IsOneOf<RemoveCVReference<T>, Utf16String>)
    static Utf16String from_utf16_without_validation(T&&) = delete;

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

private:
    ALWAYS_INLINE explicit Utf16String(NonnullRefPtr<Detail::Utf16StringData const> value)
        : Utf16StringBase(move(value))
    {
    }
};

template<>
struct Formatter<Utf16String> : Formatter<FormatString> {
    ErrorOr<void> format(FormatBuilder&, Utf16String const&);
};

template<>
struct Traits<Utf16String> : public DefaultTraits<Utf16String> {
    static unsigned hash(Utf16String const& s) { return s.hash(); }
};

}

[[nodiscard]] ALWAYS_INLINE Utf16String operator""_utf16(char const* string, size_t length)
{
    StringView view { string, length };

    ASSERT(Utf8View { view }.validate());
    return Utf16String::from_utf8_without_validation(view);
}

[[nodiscard]] ALWAYS_INLINE Utf16String operator""_utf16(char16_t const* string, size_t length)
{
    Utf16View view { { reinterpret_cast<u16 const*>(string), length } };

    ASSERT(view.validate());
    return Utf16String::from_utf16_without_validation(view);
}
