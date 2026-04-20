/*
 * Copyright (c) 2022, mat
 * Copyright (c) 2024-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/String.h>
#include <AK/StringView.h>

namespace Unicode {

enum class NormalizationForm : u8 {
    NFD,
    NFC,
    NFKD,
    NFKC
};
NormalizationForm normalization_form_from_string(StringView);

String normalize(StringView string, NormalizationForm form);

}
