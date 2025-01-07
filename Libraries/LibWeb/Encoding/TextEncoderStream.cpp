/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibJS/Runtime/Realm.h>
#include <LibWeb/Bindings/ExceptionOrUtils.h>
#include <LibWeb/Bindings/Intrinsics.h>
#include <LibWeb/Bindings/TextEncoderStreamPrototype.h>
#include <LibWeb/Encoding/AbstractOperations.h>
#include <LibWeb/Encoding/TextEncoderStream.h>
#include <LibWeb/Streams/TransformStream.h>
#include <LibWeb/WebIDL/AbstractOperations.h>

namespace Web::Encoding {

// https://encoding.spec.whatwg.org/#dom-textencoderstream
WebIDL::ExceptionOr<GC::Ref<TextEncoderStream>> TextEncoderStream::construct_impl(JS::Realm& realm)
{
    // 1. Set this’s encoder to an instance of the UTF-8 encoder.

    // 4. Let transformStream be a new TransformStream.
    // 6. Set this’s transform to transformStream.
    // NOTE: We do these first so that we may store it as nonnull in the GenericTransformStream.
    auto stream = realm.create<TextEncoderStream>(realm, realm.create<Streams::TransformStream>(realm));

    // 2. Let transformAlgorithm be an algorithm which takes a chunk argument and runs the encode and enqueue a chunk
    //    algorithm with this and chunk.
    auto transform_algorithm = GC::create_function(realm.heap(), [stream](JS::Value chunk) -> GC::Ref<WebIDL::Promise> {
        auto& realm = stream->realm();
        auto& vm = realm.vm();

        if (auto result = stream->encode_and_enqueue_chunk(vm, chunk); result.is_error()) {
            auto throw_completion = Bindings::exception_to_throw_completion(vm, result.exception());
            return WebIDL::create_rejected_promise(realm, *throw_completion.release_value());
        }

        return WebIDL::create_resolved_promise(realm, JS::js_undefined());
    });

    // 3. Let flushAlgorithm be an algorithm which runs the encode and flush algorithm with this.
    auto flush_algorithm = GC::create_function(realm.heap(), [stream]() -> GC::Ref<WebIDL::Promise> {
        auto& realm = stream->realm();
        auto& vm = realm.vm();

        if (auto result = stream->encode_and_flush(); result.is_error()) {
            auto throw_completion = Bindings::exception_to_throw_completion(vm, result.exception());
            return WebIDL::create_rejected_promise(realm, *throw_completion.release_value());
        }

        return WebIDL::create_resolved_promise(realm, JS::js_undefined());
    });

    // 5. Set up transformStream with transformAlgorithm set to transformAlgorithm and flushAlgorithm set to flushAlgorithm.
    stream->m_transform->set_up(transform_algorithm, flush_algorithm);

    return stream;
}

TextEncoderStream::TextEncoderStream(JS::Realm& realm, GC::Ref<Streams::TransformStream> transform)
    : Bindings::PlatformObject(realm)
    , Streams::GenericTransformStreamMixin(transform)
{
}

TextEncoderStream::~TextEncoderStream() = default;

void TextEncoderStream::initialize(JS::Realm& realm)
{
    Base::initialize(realm);
    WEB_SET_PROTOTYPE_FOR_INTERFACE(TextEncoderStream);
}

void TextEncoderStream::visit_edges(JS::Cell::Visitor& visitor)
{
    Base::visit_edges(visitor);
    Streams::GenericTransformStreamMixin::visit_edges(visitor);
}

// https://encoding.spec.whatwg.org/#encode-and-enqueue-a-chunk
WebIDL::ExceptionOr<void> TextEncoderStream::encode_and_enqueue_chunk(JS::VM& vm, JS::Value chunk)
{
    // 1. Let input be the result of converting chunk to a DOMString.
    auto input = TRY(WebIDL::to_string(vm, chunk));

    // 2. Convert input to an I/O queue of code units.
    auto code_units = MUST(AK::utf8_to_utf16(input));

    // Note: DOMString, as well as an I/O queue of code units rather than scalar values, are used here so that a
    //       surrogate pair that is split between chunks can be reassembled into the appropriate scalar value. The
    //       behavior is otherwise identical to USVString. In particular, lone surrogates will be replaced with U+FFFD.

    // 3. Let output be the I/O queue of bytes « end-of-queue ».
    Vector<u8> output;

    // 4. While true:
    for (auto item = code_units.begin();; ++item) {
        // 1. Let item be the result of reading from input.

        // 2. If item is end-of-queue, then:
        if (item == code_units.end()) {
            // 1. Convert output into a byte sequence.

            // 2. If output is non-empty, then:

            //     Let chunk be a Uint8Array object wrapping an ArrayBuffer containing output.

            //     Enqueue chunk into encoder’s transform.

            // 3. Return.
            return {};
        }

        // 3. Let result be the result of executing the convert code unit to scalar value algorithm with encoder, item and input.
        auto result = convert_code_unit_to_scalar_value(code_units, *item);

        // 4. If result is not continue, then process an item with result, encoder’s encoder, input, output, and "fatal".
        if (result.has_value())
            process_item(*result, m_encoder, code_units, output, ProcessMode::Fatal);
    }
}

Optional<u32> TextEncoderStream::convert_code_unit_to_scalar_value(Utf16Data& input, u16 item)
{
    // 1. If encoder’s leading surrogate is non-null, then:
    if (m_leading_surrogate.has_value()) {
        // 1. Let leadingSurrogate be encoder’s leading surrogate.
        auto leading_surrogate = *m_leading_surrogate;

        // 2. Set encoder’s leading surrogate to null.
        m_leading_surrogate.clear();

        // 3. If item is a trailing surrogate, then return a scalar value from surrogates given leadingSurrogate and item.
        if (Utf16View::is_low_surrogate(item))
            return Utf16View::decode_surrogate_pair(leading_surrogate, item);

        // 4. Restore item to input.
        input.prepend(item);

        // 5. Return U+FFFD.
        return 0xFFFD;
    }

    // 2. If item is a leading surrogate, then set encoder’s leading surrogate to item and return continue.
    if (Utf16View::is_high_surrogate(item)) {
        m_leading_surrogate = item;
        return {};
    }

    // 3. If item is a trailing surrogate, then return U+FFFD.
    if (Utf16View::is_low_surrogate(item))
        return 0xFFFD;

    // 4. Return item.
    return item;
}

}
