/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#define MAGIC_ENUM_RANGE_MIN 0
#define MAGIC_ENUM_RANGE_MAX 256

#include <magic_enum/magic_enum.hpp>

#define AK_ENUM_IS_BITWISE(Enum)                     \
    template<>                                       \
    struct magic_enum::customize::enum_range<Enum> { \
        static constexpr bool is_flags = true;       \
    };
