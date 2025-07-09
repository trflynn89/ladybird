/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullOwnPtr.h>
#include <AK/Utf16String.h>
#include <AK/Utf16View.h>
#include <AK/Vector.h>
#include <LibUnicode/Locale.h>

namespace Unicode {

enum class ListFormatType {
    Conjunction,
    Disjunction,
    Unit,
};
ListFormatType list_format_type_from_string(Utf16View const&);
Utf16View list_format_type_to_string(ListFormatType);

class ListFormat {
public:
    static NonnullOwnPtr<ListFormat> create(StringView locale, ListFormatType, Style);
    virtual ~ListFormat() = default;

    struct Partition {
        StringView type;
        Utf16String value;
    };

    virtual Utf16String format(ReadonlySpan<Utf16String> list) const = 0;
    virtual Vector<Partition> format_to_parts(ReadonlySpan<Utf16String> list) const = 0;

protected:
    ListFormat() = default;
};

}
