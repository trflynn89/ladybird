/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibURL/Parser.h>
#include <LibWebView/URLConstants.h>

namespace WebView {

URL::URL devtools_help_url = URL::Parser::basic_parse("https://github.com/LadybirdBrowser/ladybird/blob/master/Documentation/DevTools.md"sv).release_value();

}
