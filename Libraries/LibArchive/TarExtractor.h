/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/StringView.h>

namespace Archive {

class TarExtractor {
public:
    static ErrorOr<void> extract(StringView archive_path, StringView destination);
};

}
