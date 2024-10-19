/*
 * Copyright (c) 2024, Lucas Chollet <lucas.chollet@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Error.h>
#include <AK/Noncopyable.h>
#include <AK/OwnPtr.h>
#include <LibIPC/Forward.h>

namespace Gfx {

struct SkiaColorSpace;

class ColorSpace {
public:
    ColorSpace();
    ColorSpace(OwnPtr<SkiaColorSpace>);
    ~ColorSpace();

    ColorSpace(ColorSpace const&);
    ColorSpace(ColorSpace&&);

    ColorSpace& operator=(ColorSpace const&);
    ColorSpace& operator=(ColorSpace&&);

    static ErrorOr<ColorSpace> load_from_icc_bytes(ReadonlyBytes);

    template<typename Type>
    Type color_space() const;

private:
    OwnPtr<SkiaColorSpace> m_skia_color_space;
};

}

namespace IPC {

template<>
ErrorOr<void> encode(Encoder&, Gfx::ColorSpace const&);

template<>
ErrorOr<Gfx::ColorSpace> decode(Decoder&);

}
