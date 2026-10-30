/*
 * Copyright (c) 2025, Luke Wilde <luke@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/StringView.h>
#include <AK/Types.h>
#include <LibWebCommon/Export.h>

namespace Web::HTML {

enum class AutoplayPolicy : u8 {
    AllowAudioAndVideo,
    BlockAudio,
    BlockAudioAndVideo,
};

WEBCOMMON_API Optional<AutoplayPolicy> autoplay_policy_from_string(StringView);
WEBCOMMON_API StringView autoplay_policy_to_string(AutoplayPolicy);

}
