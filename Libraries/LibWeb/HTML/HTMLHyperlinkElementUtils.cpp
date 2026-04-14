/*
 * Copyright (c) 2021, Andreas Kling <andreas@ladybird.org>
 * Copyright (c) 2024-2025, Shannon Booth <shannon@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Base64.h>
#include <AK/LexicalPath.h>
#include <AK/Random.h>
#include <LibCore/MimeData.h>
#include <LibURL/Parser.h>
#include <LibWeb/DOM/Document.h>
#include <LibWeb/Fetch/Fetching/Fetching.h>
#include <LibWeb/Fetch/Infrastructure/FetchAlgorithms.h>
#include <LibWeb/Fetch/Infrastructure/HTTP/MIME.h>
#include <LibWeb/Fetch/Infrastructure/HTTP/Requests.h>
#include <LibWeb/Fetch/Infrastructure/URL.h>
#include <LibWeb/HTML/AttributeNames.h>
#include <LibWeb/HTML/HTMLHyperlinkElementUtils.h>
#include <LibWeb/HTML/Navigable.h>
#include <LibWeb/HTML/Navigation.h>
#include <LibWeb/HTML/Window.h>
#include <LibWeb/Platform/EventLoopPlugin.h>

namespace Web::HTML {

HTMLHyperlinkElementUtils::~HTMLHyperlinkElementUtils() = default;

// https://html.spec.whatwg.org/multipage/links.html#reinitialise-url
void HTMLHyperlinkElementUtils::reinitialize_url() const
{
    // 1. If the element's url is non-null, its scheme is "blob", and it has an opaque path, then terminate these steps.
    if (m_url.has_value() && m_url->scheme() == "blob"sv && m_url->has_an_opaque_path())
        return;

    // 2. Set the url.
    const_cast<HTMLHyperlinkElementUtils*>(this)->set_the_url();
}

// https://html.spec.whatwg.org/multipage/links.html#concept-hyperlink-url-set
void HTMLHyperlinkElementUtils::set_the_url()
{
    ScopeGuard invalidate_style_if_needed = [old_url = m_url, this] {
        if (m_url != old_url) {
            hyperlink_element_utils_element().invalidate_style(
                DOM::StyleInvalidationReason::HTMLHyperlinkElementHrefChange,
                {
                    { .type = CSS::InvalidationSet::Property::Type::PseudoClass, .value = CSS::PseudoClass::AnyLink },
                    { .type = CSS::InvalidationSet::Property::Type::PseudoClass, .value = CSS::PseudoClass::Link },
                    { .type = CSS::InvalidationSet::Property::Type::PseudoClass, .value = CSS::PseudoClass::LocalLink },
                },
                {});
        }
    };

    auto& element = hyperlink_element_utils_element();

    // 1. Set this element's url to null.
    m_url = {};

    // 2. If this element's href content attribute is absent, then return.
    auto href_content_attribute = element.attribute(HTML::AttributeNames::href);
    if (!href_content_attribute.has_value()) {
        return;
    }

    // 3. Let url be the result of encoding-parsing a URL given this element's href content attribute's value, relative to this element's node document.
    auto url = element.document().encoding_parse_url(*href_content_attribute);

    // 4. If url is not failure, then set this element's url to url.
    if (url.has_value())
        m_url = url.release_value();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-origin
String HTMLHyperlinkElementUtils::origin() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. If this element's url is null, return the empty string.
    if (!m_url.has_value())
        return String {};

    // 3. Return the serialization of this element's url's origin.
    return m_url->origin().serialize();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-protocol
String HTMLHyperlinkElementUtils::protocol() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. If this element's url is null, return ":".
    if (!m_url.has_value())
        return ":"_string;

    // 3. Return this element's url's scheme, followed by ":".
    return MUST(String::formatted("{}:", m_url->scheme()));
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-protocol
void HTMLHyperlinkElementUtils::set_protocol(StringView protocol)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. If this element's url is null, terminate these steps.
    if (!m_url.has_value())
        return;

    // 3. Basic URL parse the given value, followed by ":", with this element's url as url and scheme start state as state override.
    (void)URL::Parser::basic_parse(MUST(String::formatted("{}:", protocol)), {}, &m_url.value(), URL::Parser::State::SchemeStart);

    // 4. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-username
String HTMLHyperlinkElementUtils::username() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. If this element's url is null, return the empty string.
    if (!m_url.has_value())
        return String {};

    // 3. Return this element's url's username.
    return m_url->username();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-username
void HTMLHyperlinkElementUtils::set_username(StringView username)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null or url cannot have a username/password/port, then return.
    if (!url.has_value() || url->cannot_have_a_username_or_password_or_port())
        return;

    // 4. Set the username given this’s URL and the given value.
    url->set_username(username);

    // 5. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-password
String HTMLHyperlinkElementUtils::password() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null, then return the empty string.
    if (!url.has_value())
        return String {};

    // 4. Return url's password.
    return url->password();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-password
void HTMLHyperlinkElementUtils::set_password(StringView password)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null or url cannot have a username/password/port, then return.
    if (!url.has_value() || url->cannot_have_a_username_or_password_or_port())
        return;

    // 4. Set the password, given url and the given value.
    url->set_password(password);

    // 5. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-host
String HTMLHyperlinkElementUtils::host() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto const& url = m_url;

    // 3. If url or url's host is null, return the empty string.
    if (!url.has_value() || !url->host().has_value())
        return String {};

    // 4. If url's port is null, return url's host, serialized.
    if (!url->port().has_value())
        return url->serialized_host();

    // 5. Return url's host, serialized, followed by ":" and url's port, serialized.
    return MUST(String::formatted("{}:{}", url->serialized_host(), url->port().value()));
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-host
void HTMLHyperlinkElementUtils::set_host(StringView host)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null or url has an opaque path, then return.
    if (!url.has_value() || url->has_an_opaque_path())
        return;

    // 4. Basic URL parse the given value, with url as url and host state as state override.
    (void)URL::Parser::basic_parse(host, {}, &url.value(), URL::Parser::State::Host);

    // 5. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-hostname
String HTMLHyperlinkElementUtils::hostname() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto url = m_url;

    // 3. If url or url's host is null, return the empty string.
    if (!url.has_value() || !url->host().has_value())
        return String {};

    // 4. Return url's host, serialized.
    return url->serialized_host();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-hostname
void HTMLHyperlinkElementUtils::set_hostname(StringView hostname)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null or url has an opaque path, then return.
    if (!url.has_value() || url->has_an_opaque_path())
        return;

    // 4. Basic URL parse the given value, with url as url and hostname state as state override.
    (void)URL::Parser::basic_parse(hostname, {}, &url.value(), URL::Parser::State::Hostname);

    // 5. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-port
String HTMLHyperlinkElementUtils::port() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url or url's port is null, return the empty string.
    if (!url.has_value() || !url->port().has_value())
        return String {};

    // 4. Return url's port, serialized.
    return String::number(url->port().value());
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-port
void HTMLHyperlinkElementUtils::set_port(StringView port)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null or url cannot have a username/password/port, then return.
    if (!m_url.has_value() || m_url->cannot_have_a_username_or_password_or_port())
        return;

    // 4. If the given value is the empty string, then set url's port to null.
    if (port.is_empty()) {
        m_url->set_port({});
    }
    // 5. Otherwise, basic URL parse the given value, with url as url and port state as state override.
    else {
        (void)URL::Parser::basic_parse(port, {}, &m_url.value(), URL::Parser::State::Port);
    }

    // 6. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-pathname
String HTMLHyperlinkElementUtils::pathname() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null, return the empty string.
    if (!m_url.has_value())
        return String {};

    // 4. Return the result of URL path serializing url.
    return m_url->serialize_path();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-pathname
void HTMLHyperlinkElementUtils::set_pathname(StringView pathname)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto& url = m_url;

    // 3. If url is null or url has an opaque path, then return.
    if (!url.has_value() || url->has_an_opaque_path())
        return;

    // 4. Set url's path to the empty list.
    url->set_paths({});

    // 5. Basic URL parse the given value, with url as url and path start state as state override.
    (void)URL::Parser::basic_parse(pathname, {}, &url.value(), URL::Parser::State::PathStart);

    // 6. Update href.
    update_href();
}

String HTMLHyperlinkElementUtils::search() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null, or url's query is either null or the empty string, return the empty string.
    if (!m_url.has_value() || !m_url->query().has_value() || m_url->query()->is_empty())
        return String {};

    // 4. Return "?", followed by url's query.
    return MUST(String::formatted("?{}", m_url->query()));
}

void HTMLHyperlinkElementUtils::set_search(StringView search)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null, terminate these steps.
    if (!m_url.has_value())
        return;

    // 4. If the given value is the empty string, set url's query to null.
    if (search.is_empty()) {
        m_url->set_query({});
    } else {
        // 5. Otherwise:
        //    1. Let input be the given value with a single leading "?" removed, if any.
        auto input = search.substring_view(search.starts_with('?'));

        //    2. Set url's query to the empty string.
        m_url->set_query(String {});

        //    3. Basic URL parse input, with null, this element's node document's document's character encoding, url as url, and query state as state override.
        (void)URL::Parser::basic_parse(input, {}, &m_url.value(), URL::Parser::State::Query);
    }

    // 6. Update href.
    update_href();
}

String HTMLHyperlinkElementUtils::hash() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null, or url's fragment is either null or the empty string, return the empty string.
    if (!m_url.has_value() || !m_url->fragment().has_value() || m_url->fragment()->is_empty())
        return String {};

    // 4. Return "#", followed by url's fragment.
    return MUST(String::formatted("#{}", *m_url->fragment()));
}

void HTMLHyperlinkElementUtils::set_hash(StringView hash)
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.

    // 3. If url is null, then return.
    if (!m_url.has_value())
        return;

    // 4. If the given value is the empty string, set url's fragment to null.
    if (hash.is_empty()) {
        m_url->set_fragment({});
    } else {
        // 5. Otherwise:
        //    1. Let input be the given value with a single leading "#" removed, if any.
        auto input = hash.substring_view(hash.starts_with('#'));

        //    2. Set url's fragment to the empty string.
        m_url->set_fragment(String {});

        //    3. Basic URL parse input, with url as url and fragment state as state override.
        (void)URL::Parser::basic_parse(input, {}, &m_url.value(), URL::Parser::State::Fragment);
    }

    // 6. Update href.
    update_href();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-href
String HTMLHyperlinkElementUtils::href() const
{
    // 1. Reinitialize url.
    reinitialize_url();

    // 2. Let url be this element's url.
    auto const& url = m_url;

    // 3. If url is null and this element has no href content attribute, return the empty string.
    auto href_content_attribute = hyperlink_element_utils_element().attribute(HTML::AttributeNames::href);
    if (!url.has_value() && !href_content_attribute.has_value())
        return String {};

    // 4. Otherwise, if url is null, return this element's href content attribute's value.
    if (!url.has_value())
        return href_content_attribute.release_value();

    // 5. Return url, serialized.
    return url->serialize();
}

// https://html.spec.whatwg.org/multipage/links.html#dom-hyperlink-href
void HTMLHyperlinkElementUtils::set_href(String href)
{
    // The href attribute's setter must set this element's href content attribute's value to the given value.
    hyperlink_element_utils_element().set_attribute_value(HTML::AttributeNames::href, move(href));
}

// https://html.spec.whatwg.org/multipage/links.html#update-href
void HTMLHyperlinkElementUtils::update_href()
{
    // To update href, set the element's href content attribute's value to the element's url, serialized.
    hyperlink_element_utils_element().set_attribute_value(HTML::AttributeNames::href, m_url->serialize());
}

// https://html.spec.whatwg.org/multipage/links.html#api-for-a-and-area-elements:extract-an-origin
Optional<URL::Origin> HTMLHyperlinkElementUtils::hyperlink_element_utils_extract_an_origin() const
{
    // 1. If this's url is null, then return null.
    if (!m_url.has_value())
        return {};

    // 2. Return this's url's origin.
    return m_url->origin();
}

// https://html.spec.whatwg.org/multipage/links.html#downloading-hyperlinks
void HTMLHyperlinkElementUtils::download_the_hyperlink(Optional<String> hyperlink_suffix, UserNavigationInvolvement user_involvement)
{
    auto& subject = hyperlink_element_utils_element();

    // 1. If subject cannot navigate, then return.
    if (subject.cannot_navigate())
        return;

    // 2. If subject's node document's active sandboxing flag set has the sandboxed downloads browsing context flag
    //    set, then return.
    if (has_flag(subject.document().active_sandboxing_flag_set(), SandboxingFlagSet::SandboxedDownloads))
        return;

    // 3. Let urlString be the result of encoding-parsing-and-serializing a URL given subject's href attribute value,
    //    relative to subject's node document.
    auto url_string = subject.document().encoding_parse_and_serialize_url(href());

    // 4. If urlString is failure, then return.
    if (!url_string.has_value())
        return;

    // 5. If hyperlinkSuffix is non-null, then append it to urlString.
    if (hyperlink_suffix.has_value())
        url_string = MUST(String::formatted("{}{}", *url_string, *hyperlink_suffix));

    auto url = URL::Parser::basic_parse(*url_string).release_value();
    auto filename = subject.attribute(AttributeNames::download);

    // 6. If userInvolvement is not "browser UI", then:
    if (user_involvement != UserNavigationInvolvement::BrowserUI) {
        // 1. Assert: subject has a download attribute.
        VERIFY(filename.has_value());

        // 2. Let navigation be subject's relevant global object's navigation API.
        auto navigation = as<Window>(relevant_global_object(subject)).navigation();

        // 3. Let filename be the value of subject's download attribute.

        // 4. Let continue be the result of firing a download request navigate event at navigation with destinationURL
        //    set to urlString, userInvolvement set to userInvolvement, sourceElement set to subject, and filename set
        //    to filename.
        auto continue_ = navigation->fire_a_download_request_navigate_event(url, user_involvement, subject, filename.release_value());

        // 5. If continue is false, then return.
        if (!continue_)
            return;

        // 6. Inform the navigation API about aborting navigation given subject's node navigable.
        if (auto navigable = subject.navigable())
            navigable->inform_the_navigation_api_about_aborting_navigation();
    }

    // 7. Run these steps in parallel:
    Platform::EventLoopPlugin::the().deferred_invoke(GC::create_function(subject.heap(), [subject = GC::Ref { subject }, url = move(url), filename = move(filename)]() mutable {
        auto& realm = subject->realm();
        auto& vm = realm.vm();

        // FIXME: 1. Optionally, the user agent may abort these steps, if it believes doing so would safeguard the user from a
        //           potentially hostile download.
        // https://github.com/whatwg/html/issues/2562
        // https://github.com/whatwg/html/issues/7718

        // 2. Let request be a new request whose URL is urlString, client is entry settings object, initiator is
        //    "download", destination is the empty string, and whose synchronous flag and use-URL-credentials flag are
        //    set.
        // FIXME: Spec issue: The synchronous flag does not exist anymore.
        //        https://github.com/whatwg/fetch/pull/1165
        auto request = Fetch::Infrastructure::Request::create(vm);
        request->set_url(move(url));
        request->set_client(&entry_settings_object());
        request->set_initiator(Fetch::Infrastructure::Request::Initiator::Download);
        request->set_destination({});
        request->set_use_url_credentials(true);

        // 3. Let response be the result of fetching request.
        Fetch::Infrastructure::FetchAlgorithms::Input fetch_algorithms_input {};
        fetch_algorithms_input.process_response = [subject, filename = move(filename)](GC::Ref<Fetch::Infrastructure::Response> response) mutable {
            // 4. Handle as a download response with subject's node navigable and null.
            if (auto navigable = subject->navigable())
                handle_as_a_download(subject->document(), response, *navigable, {}, move(filename));
        };

        Fetch::Fetching::fetch(realm, request, Fetch::Infrastructure::FetchAlgorithms::create(vm, move(fetch_algorithms_input)));
    }));
}

// https://html.spec.whatwg.org/multipage/links.html#handle-as-a-download
void handle_as_a_download(GC::Ptr<DOM::Document> document, GC::Ref<Fetch::Infrastructure::Response> response, GC::Ref<Navigable> navigable, Optional<String> navigation_id, Optional<String> proposed_filename)
{
    // 1. Let suggestedFilename be the result of getting the suggested filename for response.
    auto suggested_filename = get_suggested_filename(document, response, move(proposed_filename));

    // 2. Let download behavior be the result of WebDriver BiDi download will begin with navigable and a new WebDriver BiDi navigation status whose id is navigationId, status is "pending", url is response's URL, and suggestedFilename is suggestedFilename.

    // 3. If download behavior is not null and download behavior's allowed is false:

    //     Invoke WebDriver BiDi download end with navigable and a new WebDriver BiDi navigation status whose id is navigationId, status is "canceled", url is response's URL.

    //     Return.

    // 4. If download behavior is not null, let destinationFolder be download behavior's destinationFolder.

    // 5. Run these steps in parallel:

    //     Run implementation-defined steps to save response for later use. If destinationFolder is not null, the user agent should save the file to that path. If the user agent needs a filename, the user agent should use the suggestedFilename.

    //     If any of the following are true:

    //         the download is canceled by the user;

    //         the download is canceled by the user agent;

    //         an error occurs (for example, a network error, not enough storage, an unavailable destination folder);

    //     then:

    //         Invoke WebDriver BiDi download end with navigable and a new WebDriver BiDi navigation status whose id is navigationId, status is "canceled", url is response's URL.

    //         Return.

    //     When the download completes successfully, invoke WebDriver BiDi download end with navigable and a new WebDriver BiDi navigation status whose id is navigationId, status is "complete", downloadedFilepath is an absolute path of the downloaded file if available, otherwise null, url is response's URL.
}

// https://html.spec.whatwg.org/multipage/links.html#getting-the-suggested-filename
ByteString get_suggested_filename(GC::Ptr<DOM::Document> document, GC::Ref<Fetch::Infrastructure::Response> response, Optional<String> proposed_filename)
{
    // 1. Let filename be the undefined value.
    Optional<String> filename;

    // FIXME: 2. If response has a `Content-Disposition` header, that header specifies the attachment disposition type, and the
    //           header includes filename information, then let filename have the value specified by the header, and jump to
    //           the step labeled sanitize below. [RFC6266]

    // 3. Let interface origin be the origin of the Document in which the download or navigate action resulting in the
    //    download was initiated, if any.
    Optional<URL::Origin> interface_origin;

    if (document)
        interface_origin = document->origin();

    // 4. Let response origin be the origin of the URL of response, unless that URL's scheme component is data, in which
    //    case let response origin be the same as the interface origin, if any.
    Optional<URL::Origin> response_origin;

    if (auto url = response->url(); url.has_value() && url->scheme() != "data"sv)
        response_origin = url->origin();
    else
        response_origin = interface_origin;

    // 5. If there is no interface origin, then let trusted operation be true. Otherwise, let trusted operation be true
    //    if response origin is the same origin as interface origin, and false otherwise.
    auto trusted_operation = !interface_origin.has_value() || response_origin->is_same_origin(*interface_origin);

    // FIXME: 6. If trusted operation is true and response has a `Content-Disposition` header and that header includes filename
    //           information, then let filename have the value specified by the header, and jump to the step labeled sanitize
    //           below. [RFC6266]
    if (trusted_operation) {
    }

    // 7. If the download was not initiated from a hyperlink created by an a or area element, or if the element of the
    //    hyperlink from which it was initiated did not have a download attribute when the download was initiated, or if
    //    there was such an attribute but its value when the download was initiated was the empty string, then jump to
    //    the step labeled no proposed filename.
    if (proposed_filename.has_value() && !proposed_filename->is_empty()) {
        // 8. Let proposed filename have the value of the download attribute of the element of the hyperlink that
        //    initiated the download at the time the download was initiated.
        // 9. If trusted operation is true, let filename have the value of proposed filename, and jump to the step
        //    labeled sanitize below.
        if (trusted_operation)
            filename = move(proposed_filename);

        // FIXME: 10. If response has a `Content-Disposition` header and that header specifies the attachment disposition type,
        //            let filename have the value of proposed filename, and jump to the step labeled sanitize below. [RFC6266]
    }

    // 11. No proposed filename: If trusted operation is true, or if the user indicated a preference for having the
    //     response in question downloaded, let filename have a value derived from the URL of response in an
    //     implementation-defined manner, and jump to the step labeled sanitize below.
    if (!filename.has_value() && trusted_operation) {
        if (auto url = response->url(); url.has_value() && !url->paths().is_empty() && url->scheme().is_one_of("http"sv, "https"sv, "file"sv)) {
            if (auto last_segment = url->paths().last(); !last_segment.is_empty())
                filename = move(last_segment);
        }
    }

    // 12. Let filename be set to the user's preferred filename or to a filename selected by the user agent, and jump to
    //     the step labeled sanitize below.
    // Warning: If the algorithm reaches this step, then a download was begun from a different origin than response, and
    //          the origin did not mark the file as suitable for downloading, and the download was not initiated by the
    //          user. This could be because a download attribute was used to trigger the download, or because response
    //          is not of a type that the user agent supports.
    //
    //          This could be dangerous, because, for instance, a hostile server could be trying to get a user to
    //          unknowingly download private information and then re-upload it to the hostile server, by tricking the
    //          user into thinking the data is from the hostile server.
    //
    //          Thus, it is in the user's interests that the user be somehow notified that response comes from quite a
    //          different source, and to prevent confusion, any suggested filename from the potentially hostile
    //          interface origin should be ignored.
    if (!filename.has_value()) {
        Array<u8, 16> random;
        fill_with_random(random);

        filename = MUST(AK::encode_base64url(random, AK::OmitPadding::Yes));
    }

    // FIXME: 13. Sanitize: Optionally, allow the user to influence filename. For example, a user agent could prompt the user
    //            for a filename, potentially providing the value of filename as determined above as a default value.

    // 14. Adjust filename to be suitable for the local file system.
    // Example: For example, this could involve removing characters that are not legal in filenames, or trimming leading
    //          and trailing whitespace.
    filename = MUST(filename->trim_whitespace());

    // 15. If the platform conventions do not in any way use extensions to determine the types of file on the file
    //     system, then return filename as the filename.

    // 16. Let claimed type be the type given by response's Content-Type metadata, if any is known. Let named type be
    //     the type given by filename's extension, if any is known. For the purposes of this step, a type is a mapping
    //     of a MIME type to an extension.
    auto extension_from_mime_type = [](MimeSniff::MimeType const& mime_type) -> Optional<StringView> {
        if (auto data = Core::get_mime_type_data(mime_type.essence()); data.has_value() && !data->common_extensions.is_empty())
            return data->common_extensions.first();
        return {};
    };

    auto claimed_type = [&]() -> Optional<StringView> {
        if (auto mime_type = Fetch::Infrastructure::extract_mime_type(response->header_list()); mime_type.has_value())
            return extension_from_mime_type(*mime_type);
        return {};
    }();

    auto named_type = [&]() -> Optional<StringView> {
        LexicalPath lexical_path { filename->to_byte_string() };

        if (auto extension = lexical_path.extension(); !extension.is_empty())
            return extension;

        if (auto url = response->url(); url.has_value()) {
            if (url->scheme().is_one_of("http"sv, "https"sv) && !url->paths().is_empty()) {
                return MUST(String::from_utf8(Core::guess_mime_type_based_on_filename(url->paths().last())));
            }

            if (url->scheme() == "data"sv) {
                if (auto data_url = Fetch::Infrastructure::process_data_url(*url); !data_url.is_error())
                    return data_url.value().mime_type.essence();
            }
        }

        return {};
    }();

    // 17. If named type is consistent with the user's preferences (e.g., because the value of filename was determined
    //     by prompting the user), then return filename as the filename.

    // 18. If claimed type and named type are the same type (i.e., the type given by response's Content-Type metadata is
    //     consistent with the type given by filename's extension), then return filename as the filename.

    // 19. If the claimed type is known, then alter filename to add an extension corresponding to claimed type.

    //     Otherwise, if named type is known to be potentially dangerous (e.g. it will be treated by the platform
    //     conventions as a native executable, shell script, HTML application, or executable-macro-capable document),
    //     then optionally alter filename to add a known-safe extension (e.g. ".txt").

    // Note: This last step would make it impossible to download executables, which might not be desirable. As always,
    //       implementers are forced to balance security and usability in this matter.

    // 20. Return filename as the filename.
}

}
