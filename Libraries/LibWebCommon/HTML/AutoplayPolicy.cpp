/*
 * Copyright (c) 2025, Luke Wilde <luke@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWebCommon/HTML/AutoplayPolicy.h>

namespace Web::HTML {

Optional<AutoplayPolicy> autoplay_policy_from_string(StringView policy)
{
    if (policy == "allow"sv)
        return AutoplayPolicy::AllowAudioAndVideo;
    if (policy == "allow-if-inaudible"sv)
        return AutoplayPolicy::BlockAudio;
    if (policy == "block"sv)
        return AutoplayPolicy::BlockAudioAndVideo;
    return {};
}

StringView autoplay_policy_to_string(AutoplayPolicy policy)
{
    switch (policy) {
    case AutoplayPolicy::AllowAudioAndVideo:
        return "allow"sv;
    case AutoplayPolicy::BlockAudio:
        return "allow-if-inaudible"sv;
    case AutoplayPolicy::BlockAudioAndVideo:
        return "block"sv;
    }
    VERIFY_NOT_REACHED();
}

}
