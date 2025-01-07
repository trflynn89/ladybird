/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Utf16View.h>
#include <LibGC/Ptr.h>
#include <LibJS/Forward.h>
#include <LibTextCodec/Encoder.h>
#include <LibWeb/Bindings/PlatformObject.h>
#include <LibWeb/Encoding/TextEncoderCommon.h>
#include <LibWeb/Streams/GenericTransformStream.h>
#include <LibWeb/WebIDL/ExceptionOr.h>

namespace Web::Encoding {

// https://encoding.spec.whatwg.org/#textencoderstream
class TextEncoderStream final
    : public Bindings::PlatformObject
    , public Streams::GenericTransformStreamMixin
    , public TextEncoderCommonMixin {
    WEB_PLATFORM_OBJECT(TextEncoderStream, Bindings::PlatformObject);
    GC_DECLARE_ALLOCATOR(TextEncoderStream);

public:
    static WebIDL::ExceptionOr<GC::Ref<TextEncoderStream>> construct_impl(JS::Realm&);
    virtual ~TextEncoderStream() override;

private:
    TextEncoderStream(JS::Realm&, GC::Ref<Streams::TransformStream>);

    virtual void initialize(JS::Realm&) override;
    virtual void visit_edges(Cell::Visitor&) override;

    WebIDL::ExceptionOr<void> encode_and_enqueue_chunk(JS::VM&, JS::Value);
    Optional<u32> convert_code_unit_to_scalar_value(Utf16Data& input, u16 item);
    WebIDL::ExceptionOr<void> encode_and_flush();

    // https://encoding.spec.whatwg.org/#textencoderstream-encoder
    TextCodec::UTF8Encoder m_encoder;

    Optional<u16> m_leading_surrogate;
};

}
