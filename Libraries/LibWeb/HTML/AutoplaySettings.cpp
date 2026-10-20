/*
 * Copyright (c) 2023-2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibURL/Origin.h>
#include <LibURL/URL.h>
#include <LibWeb/DOM/Document.h>
#include <LibWeb/HTML/AutoplaySettings.h>
#include <LibWeb/Platform/SiteSettings.h>

namespace Web::HTML {

AutoplayDecision autoplay_decision_for_origin(DOM::Document const& document, URL::Origin const& origin)
{
    auto value = Platform::SiteSettings::the().value_for_origin("autoplay"_utf16, origin, Platform::SiteSettings::OriginMatching::SameOriginDomain);
    auto policy = value.has_value() && value->has<AutoplayPolicy>() ? value->get<AutoplayPolicy>() : AutoplayPolicy::BlockAudio;

    // AD-HOC: Allow autoplay for file:// URLs if the document is also from a file:// URL.
    if (origin.is_opaque_file_origin() && document.origin().is_opaque_file_origin())
        return AutoplayDecision::Allowed;

    switch (policy) {
    case AutoplayPolicy::AllowAudioAndVideo:
        return AutoplayDecision::Allowed;
    case AutoplayPolicy::BlockAudio:
        return AutoplayDecision::AllowedIfInaudible;
    case AutoplayPolicy::BlockAudioAndVideo:
        return AutoplayDecision::Blocked;
    }

    VERIFY_NOT_REACHED();
}

}
