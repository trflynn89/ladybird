/*
 * Copyright (c) 2022, mat
 * Copyright (c) 2024-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/StringBuilder.h>
#include <LibUnicode/Normalize.h>
#include <LibUnicode/RustFFI.h>

namespace Unicode {

NormalizationForm normalization_form_from_string(StringView form)
{
    if (form == "NFD"sv)
        return NormalizationForm::NFD;
    if (form == "NFC"sv)
        return NormalizationForm::NFC;
    if (form == "NFKD"sv)
        return NormalizationForm::NFKD;
    if (form == "NFKC"sv)
        return NormalizationForm::NFKC;
    VERIFY_NOT_REACHED();
}

static bool append_normalized_chunk(void* context, u8 const* bytes, size_t length)
{
    auto& builder = *reinterpret_cast<StringBuilder*>(context);
    return !builder.try_append(reinterpret_cast<char const*>(bytes), length).is_error();
}

String normalize(StringView string, NormalizationForm form)
{
    if (string.is_empty())
        return {};

    StringBuilder builder { string.length() };

    if (FFI::normalize(string.bytes().data(), string.length(), to_underlying(form), &builder, append_normalized_chunk))
        return MUST(builder.to_string());

    return MUST(String::from_utf8(string));
}

}
