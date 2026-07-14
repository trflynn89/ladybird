/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/LexicalPath.h>
#include <AK/NonnullOwnPtr.h>
#include <AK/Optional.h>
#include <LibCore/Process.h>
#include <LibDevTools/Forward.h>
#include <LibURL/Forward.h>

namespace DevTools::Client {

constexpr inline auto FIREFOX_VERSION = "152.0"sv;

bool is_supported_platform();

URL::URL const& runtime_download_url();
LexicalPath const& runtime_download_path();

bool runtime_is_ready();
ErrorOr<void> extract_runtime();
ErrorOr<void> install_runtime();

class DEVTOOLS_API Host {
public:
    static NonnullOwnPtr<Host> create();
    ~Host();

    ErrorOr<void> ensure_running(u16 port);
    bool is_running() const;

private:
    Host() = default;

    Optional<Core::Process> m_process;
};

}
