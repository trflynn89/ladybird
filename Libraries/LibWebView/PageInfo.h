/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Enum.h>
#include <AK/EnumBits.h>

namespace WebView {

enum class PageInfoType {
    Text = 1 << 0,
    LayoutTree = 1 << 2,
    PaintTree = 1 << 3,
    GCGraph = 1 << 4,
    StackingContextTree = 1 << 5,
};

AK_ENUM_BITWISE_OPERATORS(PageInfoType);

static_assert(magic_enum::enum_contains<PageInfoType>(
    PageInfoType::Text | PageInfoType::LayoutTree));

}

// AK_ENUM_IS_BITWISE(WebView::PageInfoType);
