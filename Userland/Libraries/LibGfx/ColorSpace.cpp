/*
 * Copyright (c) 2024, Lucas Chollet <lucas.chollet@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ByteBuffer.h>
#include <LibGfx/ColorSpace.h>
#include <LibIPC/Decoder.h>
#include <LibIPC/Encoder.h>

#include <core/SkColorSpace.h>
#include <core/SkData.h>

namespace Gfx {

struct SkiaColorSpace {
    sk_sp<SkColorSpace> color_space { nullptr };
};

ColorSpace::ColorSpace() = default;
ColorSpace::~ColorSpace() = default;

ColorSpace::ColorSpace(OwnPtr<SkiaColorSpace> skia_color_space)
    : m_skia_color_space(move(skia_color_space))
{
}

ColorSpace::ColorSpace(ColorSpace const& other)
    : m_skia_color_space(make<SkiaColorSpace>(other.m_skia_color_space->color_space))
{
}

ColorSpace::ColorSpace(ColorSpace&& other)
    : m_skia_color_space(move(other.m_skia_color_space))
{
}

ColorSpace& ColorSpace::operator=(ColorSpace const& other)
{
    if (this == &other)
        return *this;

    m_skia_color_space = make<SkiaColorSpace>(other.m_skia_color_space->color_space);
    return *this;
}

ColorSpace& ColorSpace::operator=(ColorSpace&& other)
{
    if (this == &other)
        return *this;

    m_skia_color_space = move(other.m_skia_color_space);
    return *this;
}

ErrorOr<ColorSpace> ColorSpace::load_from_icc_bytes(ReadonlyBytes icc_bytes)
{
    ColorSpace color_space;

    if (icc_bytes.size() != 0) {
        skcms_ICCProfile icc_profile {};
        if (!skcms_Parse(icc_bytes.data(), icc_bytes.size(), &icc_profile))
            return Error::from_string_literal("Failed to parse the ICC profile");

        color_space.m_skia_color_space = make<SkiaColorSpace>(SkColorSpace::Make(icc_profile));
    }

    return color_space;
}

template<>
sk_sp<SkColorSpace> ColorSpace::color_space() const
{
    return m_skia_color_space->color_space;
}

}

namespace IPC {

template<>
ErrorOr<void> encode(Encoder& encoder, Gfx::ColorSpace const& color_space)
{
    auto skia_color_space = color_space.color_space<sk_sp<SkColorSpace>>();

    if (!skia_color_space) {
        TRY(encoder.encode<u64>(0));
        return {};
    }

    auto serialized = skia_color_space->serialize();
    TRY(encoder.encode<u64>(serialized->size()));
    TRY(encoder.append(serialized->bytes(), serialized->size()));
    return {};
}

template<>
ErrorOr<Gfx::ColorSpace> decode(Decoder& decoder)
{
    auto size = TRY(decoder.decode<u64>());
    if (size == 0)
        return Gfx::ColorSpace {};

    auto buffer = TRY(ByteBuffer::create_uninitialized(size));
    TRY(decoder.decode_into(buffer.bytes()));

    auto skia_color_space = SkColorSpace::Deserialize(buffer.data(), buffer.size());
    return Gfx::ColorSpace { make<Gfx::SkiaColorSpace>(move(skia_color_space)) };
}

}
