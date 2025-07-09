/*
 * Copyright (c) 2022, mat
 * Copyright (c) 2024, Tim Flynn <trflynn89@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/String.h>
#include <AK/StringView.h>
#include <AK/Utf16View.h>

namespace Unicode {

enum class NormalizationForm {
    NFD,
    NFC,
    NFKD,
    NFKC
};
NormalizationForm normalization_form_from_string(Utf16View const&);
Utf16View normalization_form_to_string(NormalizationForm);

String normalize(StringView string, NormalizationForm form);

}
