/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/String.h>
#include <AK/Types.h>

namespace DevTools::Client {

enum class Stage : u8 {
    CheckingRuntime,
    DownloadingRuntime,
    ExtractingRuntime,
    PreparingRuntime,
    Launching,
    Running,
    Failed,
};

struct Status {
    Stage stage { Stage::CheckingRuntime };
    Optional<String> error;
};

constexpr StringView pending_stage_to_string(Stage stage)
{
    switch (stage) {
    case Stage::CheckingRuntime:
        return "Preparing the DevTools client..."sv;
    case Stage::DownloadingRuntime:
        return "Downloading DevTools runtime..."sv;
    case Stage::ExtractingRuntime:
        return "Extracting DevTools runtime..."sv;
    case Stage::PreparingRuntime:
        return "Preparing DevTools runtime..."sv;
    case Stage::Launching:
        return "Launching DevTools client..."sv;
    case Stage::Running:
    case Stage::Failed:
        break;
    }
    VERIFY_NOT_REACHED();
}

}
