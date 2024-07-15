/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Base64.h>
#include <AK/StringBuilder.h>
#include <LibJS/Runtime/Temporal/AbstractOperations.h>
#include <LibJS/Runtime/TypedArray.h>
#include <LibJS/Runtime/Uint8Array.h>
#include <LibJS/Runtime/VM.h>
#include <LibJS/Runtime/ValueInlines.h>

namespace JS {

void Uint8ArrayConstructorHelpers::initialize(Realm& realm, Object& constructor)
{
    auto& vm = constructor.vm();

    static constexpr u8 attr = Attribute::Writable | Attribute::Configurable;
    constructor.define_native_function(realm, vm.names.fromBase64, from_base64, 1, attr);
}

void Uint8ArrayPrototypeHelpers::initialize(Realm& realm, Object& prototype)
{
    auto& vm = prototype.vm();

    static constexpr u8 attr = Attribute::Writable | Attribute::Configurable;
    prototype.define_native_function(realm, vm.names.toBase64, to_base64, 0, attr);
    prototype.define_native_function(realm, vm.names.toHex, to_hex, 0, attr);
}

// 1 Uint8Array.prototype.toBase64 ( [ options ] ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-uint8array.prototype.tobase64
JS_DEFINE_NATIVE_FUNCTION(Uint8ArrayPrototypeHelpers::to_base64)
{
    auto options_value = vm.argument(0);

    // 1. Let O be the this value.
    // 2. Perform ? ValidateUint8Array(O).
    auto typed_array = TRY(validate_uint8_array(vm));

    // 3. Let opts be ? GetOptionsObject(options).
    auto* options = TRY(Temporal::get_options_object(vm, options_value));

    // 4. Let alphabet be ? Get(opts, "alphabet").
    auto alphabet = TRY(options->get(vm.names.alphabet));

    // 5. If alphabet is undefined, set alphabet to "base64".
    if (alphabet.is_undefined())
        alphabet = PrimitiveString::create(vm, "base64"sv);

    // 6. If alphabet is neither "base64" nor "base64url", throw a TypeError exception.
    if (!alphabet.is_string() || !alphabet.as_string().utf8_string_view().is_one_of("base64"sv, "base64url"sv))
        return vm.throw_completion<TypeError>(ErrorType::OptionIsNotValidValue, alphabet, "alphabet"sv);

    // 7. Let omitPadding be ToBoolean(? Get(opts, "omitPadding")).
    auto omit_padding_value = TRY(options->get(vm.names.omitPadding)).to_boolean();
    auto omit_padding = omit_padding_value ? AK::OmitPadding::Yes : AK::OmitPadding::No;

    // 8. Let toEncode be ? GetUint8ArrayBytes(O).
    auto to_encode = TRY(get_uint8_array_bytes(vm, typed_array));

    String out_ascii;

    // 9. If alphabet is "base64", then
    if (alphabet.as_string().utf8_string_view() == "base64"sv) {
        // a. Let outAscii be the sequence of code points which results from encoding toEncode according to the base64
        //    encoding specified in section 4 of RFC 4648. Padding is included if and only if omitPadding is false.
        out_ascii = MUST(encode_base64(to_encode, omit_padding));
    }
    // 10. Else,
    else {
        // a. Assert: alphabet is "base64url".
        // b. Let outAscii be the sequence of code points which results from encoding toEncode according to the base64url
        //    encoding specified in section 5 of RFC 4648. Padding is included if and only if omitPadding is false.
        out_ascii = MUST(encode_base64url(to_encode, omit_padding));
    }

    // 11. Return CodePointsToString(outAscii).
    return PrimitiveString::create(vm, move(out_ascii));
}

// 2 Uint8Array.prototype.toHex ( ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-uint8array.prototype.tobase64
JS_DEFINE_NATIVE_FUNCTION(Uint8ArrayPrototypeHelpers::to_hex)
{
    // 1. Let O be the this value.
    // 2. Perform ? ValidateUint8Array(O).
    auto typed_array = TRY(validate_uint8_array(vm));

    // 3. Let toEncode be ? GetUint8ArrayBytes(O).
    auto to_encode = TRY(get_uint8_array_bytes(vm, typed_array));

    // 4. Let out be the empty String.
    StringBuilder out;

    // 5. For each byte byte of toEncode, do
    for (auto byte : to_encode.bytes()) {
        // a. Let hex be Number::toString(𝔽(byte), 16).
        // b. Set hex to StringPad(hex, 2, "0", START).
        // c. Set out to the string-concatenation of out and hex.
        out.appendff("{:02x}", byte);
    }

    // 6. Return out.
    return PrimitiveString::create(vm, MUST(out.to_string()));
}

// 3 Uint8Array.fromBase64 ( string [ , options ] ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-uint8array.frombase64
JS_DEFINE_NATIVE_FUNCTION(Uint8ArrayConstructorHelpers::from_base64)
{
    auto string_value = vm.argument(0);
    auto options_value = vm.argument(1);

    // 1. If string is not a String, throw a TypeError exception.
    if (!string_value.is_string())
        return vm.throw_completion<TypeError>(ErrorType::NotAString, string_value);

    // 2. Let opts be ? GetOptionsObject(options).
    auto* options = TRY(Temporal::get_options_object(vm, options_value));

    // 3. Let alphabet be ? Get(opts, "alphabet").
    auto alphabet = TRY(options->get(vm.names.alphabet));

    // 4. If alphabet is undefined, set alphabet to "base64".
    if (alphabet.is_undefined())
        alphabet = PrimitiveString::create(vm, "base64"sv);

    // 5. If alphabet is neither "base64" nor "base64url", throw a TypeError exception.
    if (!alphabet.is_string() || !alphabet.as_string().utf8_string_view().is_one_of("base64"sv, "base64url"sv))
        return vm.throw_completion<TypeError>(ErrorType::OptionIsNotValidValue, alphabet, "alphabet"sv);

    // 6. Let lastChunkHandling be ? Get(opts, "lastChunkHandling").
    auto last_chunk_handling = TRY(options->get(vm.names.lastChunkHandling));

    // 7. If lastChunkHandling is undefined, set lastChunkHandling to "loose".
    if (last_chunk_handling.is_undefined())
        last_chunk_handling = PrimitiveString::create(vm, "loose"sv);

    // 8. If lastChunkHandling is not one of "loose", "strict", or "stop-before-partial", throw a TypeError exception.
    if (!last_chunk_handling.is_string() || !last_chunk_handling.as_string().utf8_string_view().is_one_of("loose"sv, "strict"sv, "stop-before-partial"sv))
        return vm.throw_completion<TypeError>(ErrorType::OptionIsNotValidValue, last_chunk_handling, "lastChunkHandling"sv);

    // 9. Let result be FromBase64(string, alphabet, lastChunkHandling).
    // 10. If result.[[Error]] is not none, then
    //     a. Throw result.[[Error]].
    // 11. Let resultLength be the length of result.[[Bytes]].
    // 12. Let ta be ? AllocateTypedArray("Uint8Array", %Uint8Array%, "%Uint8Array.prototype%", resultLength).
    // 13. Set the value at each index of ta.[[ViewedArrayBuffer]].[[ArrayBufferData]] to the value at the corresponding index of result.[[Bytes]].
    // 14. Return ta.
    return js_undefined();
}

// 7 ValidateUint8Array ( ta ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-validateuint8array
ThrowCompletionOr<NonnullGCPtr<TypedArrayBase>> validate_uint8_array(VM& vm)
{
    auto this_object = TRY(vm.this_value().to_object(vm));

    // 1. Perform ? RequireInternalSlot(ta, [[TypedArrayName]]).
    if (!this_object->is_typed_array())
        return vm.throw_completion<TypeError>(ErrorType::NotAnObjectOfType, "Uint8Array");

    auto& typed_array = static_cast<TypedArrayBase&>(*this_object.ptr());

    // 2. If ta.[[TypedArrayName]] is not "Uint8Array", throw a TypeError exception.
    if (typed_array.kind() != TypedArrayBase::Kind::Uint8Array)
        return vm.throw_completion<TypeError>(ErrorType::NotAnObjectOfType, "Uint8Array");

    // 3. Return UNUSED.
    return typed_array;
}

// 8 GetUint8ArrayBytes ( ta ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-getuint8arraybytes
ThrowCompletionOr<ByteBuffer> get_uint8_array_bytes(VM& vm, TypedArrayBase const& typed_array)
{
    // 1. Let buffer be ta.[[ViewedArrayBuffer]].
    // 2. Let taRecord be MakeTypedArrayWithBufferWitnessRecord(ta, SEQ-CST).
    auto typed_array_record = make_typed_array_with_buffer_witness_record(typed_array, ArrayBuffer::Order::SeqCst);

    // 3. If IsTypedArrayOutOfBounds(taRecord) is true, throw a TypeError exception.
    if (is_typed_array_out_of_bounds(typed_array_record))
        return vm.throw_completion<TypeError>(ErrorType::BufferOutOfBounds, "TypedArray"sv);

    // 4. Let len be TypedArrayLength(taRecord).
    auto length = typed_array_length(typed_array_record);

    // 5. Let byteOffset be ta.[[ByteOffset]].
    auto byte_offset = typed_array.byte_offset();

    // 6. Let bytes be a new empty List.
    ByteBuffer bytes;

    // 7. Let index be 0.
    // 8. Repeat, while index < len,
    for (u32 index = 0; index < length; ++index) {
        // a. Let byteIndex be byteOffset + index.
        auto byte_index = byte_offset + index;

        // b. Let byte be ℝ(GetValueFromBuffer(buffer, byteIndex, UINT8, true, UNORDERED)).
        auto byte = typed_array.get_value_from_buffer(byte_index, ArrayBuffer::Order::Unordered);

        // c. Append byte to bytes.
        bytes.append(MUST(byte.to_u8(vm)));

        // d. Set index to index + 1.
    }

    // 9. Return bytes.
    return bytes;
}

// 10.3 FromBase64 ( string, alphabet, lastChunkHandling [ , maxLength ] ), https://tc39.es/proposal-arraybuffer-base64/spec/#sec-frombase64
FromBase64Result from_base64(VM& vm, StringView string, StringView alphabet, [[maybe_unused]] StringView last_chunk_handling, [[maybe_unused]] size_t max_length)
{
    auto decoded = alphabet == "base64"sv ? decode_base64(string) : decode_base64url(string);

    if (decoded.is_error()) {
        auto error = vm.throw_completion<SyntaxError>(decoded.error().string_literal());
        return { .read = 0, .bytes = {}, .error = move(error) };
    }

    return { .read = 0, .bytes = decoded.release_value(), .error = {} };
}

}
