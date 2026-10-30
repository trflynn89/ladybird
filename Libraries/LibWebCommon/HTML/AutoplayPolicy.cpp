/*
 * Copyright (c) 2025, Luke Wilde <luke@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWebCommon/HTML/AutoplayPolicy.h>

namespace Web::HTML {

Optional<AutoplayPolicy> autoplay_policy_from_string(StringView policy)
{
    if (policy == "allow-audio-and-video"sv)
        return AutoplayPolicy::AllowAudioAndVideo;
    if (policy == "block-audio"sv)
        return AutoplayPolicy::BlockAudio;
    if (policy == "block-audio-and-video"sv)
        return AutoplayPolicy::BlockAudioAndVideo;
    return {};
}

StringView autoplay_policy_to_string(AutoplayPolicy policy)
{
    switch (policy) {
    case AutoplayPolicy::AllowAudioAndVideo:
        return "allow-audio-and-video"sv;
    case AutoplayPolicy::BlockAudio:
        return "block-audio"sv;
    case AutoplayPolicy::BlockAudioAndVideo:
        return "block-audio-and-video"sv;
    }
    VERIFY_NOT_REACHED();
}

}
