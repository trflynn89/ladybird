/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/HashMap.h>
#include <AK/Optional.h>
#include <AK/Utf16String.h>
#include <AK/Vector.h>
#include <LibURL/Origin.h>
#include <LibWeb/Export.h>
#include <LibWebCommon/WebView/SiteSettings.h>

namespace Web::Platform {

// Browser-owned site settings replicated into this process at startup and when they change.
class WEB_API SiteSettings {
public:
    AK_ALLOC_WITH_KMALLOC;

    static SiteSettings& the();

    enum class OriginMatching {
        SameOrigin,
        SameOriginDomain,
    };

    void set(Utf16String name, SiteSettingValue default_value, SiteSettingValue allowlist_value, ReadonlySpan<Utf16String> allowlist);
    Optional<SiteSettingValue> value_for_origin(Utf16String const& name, URL::Origin const&, OriginMatching = OriginMatching::SameOrigin) const;

private:
    struct Setting {
        SiteSettingValue default_value;
        SiteSettingValue allowlist_value;
        Vector<URL::Origin> allowlist;
    };

    HashMap<Utf16String, Setting> m_settings;
};

}
