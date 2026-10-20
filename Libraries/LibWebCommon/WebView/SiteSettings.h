/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Variant.h>
#include <LibWebCommon/Bindings/PermissionStatus.h>
#include <LibWebCommon/HTML/AutoplayPolicy.h>

namespace Web {

using SiteSettingValue = Variant<HTML::AutoplayPolicy, Bindings::PermissionState>;

}
