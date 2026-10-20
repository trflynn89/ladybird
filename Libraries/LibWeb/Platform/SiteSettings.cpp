/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibURL/Parser.h>
#include <LibURL/URL.h>
#include <LibWeb/Platform/SiteSettings.h>

namespace Web::Platform {

SiteSettings& SiteSettings::the()
{
    static auto& settings = *new SiteSettings;
    return settings;
}

void SiteSettings::set(Utf16String name, SiteSettingValue default_value, SiteSettingValue allowlist_value, ReadonlySpan<Utf16String> allowlist)
{
    Vector<URL::Origin> origins;
    origins.ensure_capacity(allowlist.size());

    for (auto const& entry : allowlist) {
        auto url = URL::Parser::basic_parse(entry.to_utf8());
        if (!url.has_value())
            url = URL::Parser::basic_parse(MUST(String::formatted("https://{}", entry)));
        if (!url.has_value() || url->origin().is_opaque()) {
            dbgln("Invalid origin for site setting {}: {}", name, entry);
            continue;
        }
        origins.append(url->origin());
    }

    m_settings.set(move(name), Setting { move(default_value), move(allowlist_value), move(origins) });
}

Optional<SiteSettingValue> SiteSettings::value_for_origin(Utf16String const& name, URL::Origin const& origin, OriginMatching matching) const
{
    auto setting = m_settings.get(name);
    if (!setting.has_value())
        return {};

    for (auto const& allowed : setting->allowlist) {
        if (matching == OriginMatching::SameOrigin ? allowed.is_same_origin(origin) : allowed.is_same_origin_domain(origin))
            return setting->allowlist_value;
    }
    return setting->default_value;
}

}
