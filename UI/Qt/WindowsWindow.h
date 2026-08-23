/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Platform.h>

class QWidget;

namespace Ladybird {

#if defined(AK_OS_WINDOWS)
void apply_expanded_client_area_window_styles(QWidget&);
#endif

}
