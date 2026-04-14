/*
 * Copyright (c) 2021, Andreas Kling <andreas@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibGC/Ptr.h>
#include <LibURL/URL.h>
#include <LibWeb/Forward.h>
#include <LibWeb/HTML/UserNavigationInvolvement.h>

namespace Web::HTML {

class HTMLHyperlinkElementUtils {
public:
    virtual ~HTMLHyperlinkElementUtils();

    String origin() const;

    String href() const;
    void set_href(String);

    String protocol() const;
    void set_protocol(StringView);

    String username() const;
    void set_username(StringView);

    String password() const;
    void set_password(StringView);

    String host() const;
    void set_host(StringView);

    String hostname() const;
    void set_hostname(StringView);

    String port() const;
    void set_port(StringView);

    String pathname() const;
    void set_pathname(StringView);

    String search() const;
    void set_search(StringView);

    String hash() const;
    void set_hash(StringView);

protected:
    virtual DOM::Element& hyperlink_element_utils_element() = 0;
    virtual DOM::Element const& hyperlink_element_utils_element() const = 0;

    Optional<URL::Origin> hyperlink_element_utils_extract_an_origin() const;

    void download_the_hyperlink(Optional<String> hyperlink_suffix, UserNavigationInvolvement);

    void set_the_url();

private:
    void reinitialize_url() const;
    void update_href();

    Optional<URL::URL> m_url;
};

void handle_as_a_download(GC::Ptr<DOM::Document>, GC::Ref<Fetch::Infrastructure::Response>, GC::Ref<Navigable>, Optional<String> navigation_id, Optional<String> proposed_filename);
ByteString get_suggested_filename(GC::Ptr<DOM::Document>, GC::Ref<Fetch::Infrastructure::Response>, Optional<String> proposed_filename);

}
