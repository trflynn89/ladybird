/*
 * Copyright (c) 2020, the SerenityOS developers.
 * Copyright (c) 2022, David Tuin <davidot@serenityos.org>
 * Copyright (c) 2025, Altomani Gianluca <altomanigianluca@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibCrypto/BigInt/UnsignedBigInteger.h>

namespace Crypto {

struct SignedDivisionResult;

class [[nodiscard]] SignedBigInteger {
public:
    template<Signed T>
    SignedBigInteger(T value)
        : SignedBigInteger(static_cast<i64>(value))
    {
    }
    SignedBigInteger(UnsignedBigInteger&& unsigned_data, bool sign);
    SignedBigInteger(ReadonlyBytes);

    explicit SignedBigInteger(UnsignedBigInteger const& unsigned_data);
    explicit SignedBigInteger(double value);
    explicit SignedBigInteger(i64 value);

    SignedBigInteger(SignedBigInteger const&);
    SignedBigInteger(SignedBigInteger&&);

    SignedBigInteger& operator=(SignedBigInteger const&);
    SignedBigInteger& operator=(SignedBigInteger&&);

    SignedBigInteger();
    ~SignedBigInteger();

    static SignedBigInteger import_data(ReadonlyBytes data) { return SignedBigInteger(data); }

    [[nodiscard]] Bytes export_data(Bytes) const;

    static ErrorOr<SignedBigInteger> from_base(u16 N, StringView str);
    [[nodiscard]] String to_base(u16 N) const;

    [[nodiscard]] i64 to_i64() const;
    [[nodiscard]] u64 to_u64() const;
    [[nodiscard]] double to_double(UnsignedBigInteger::RoundingMode rounding_mode = UnsignedBigInteger::RoundingMode::IEEERoundAndTiesToEvenMantissa) const;

    UnsignedBigInteger unsigned_value() const;
    [[nodiscard]] bool is_positive() const { return !is_negative() && !is_zero(); }
    [[nodiscard]] bool is_negative() const;
    [[nodiscard]] bool is_zero() const;

    void negate();
    void set_to_0();
    void set_to(i64 other);
    void set_to(SignedBigInteger const& other);

    [[nodiscard]] size_t byte_length() const;

    SignedBigInteger& add(i64);
    SignedBigInteger added_to(i64) const;

    SignedBigInteger& add(SignedBigInteger const&);
    SignedBigInteger added_to(SignedBigInteger const&) const;

    SignedBigInteger& add(UnsignedBigInteger const&);
    SignedBigInteger added_to(UnsignedBigInteger const&) const;

    SignedBigInteger& subtract(i64);
    SignedBigInteger subtracted_by(i64) const;

    SignedBigInteger& subtract(SignedBigInteger const&);
    SignedBigInteger subtracted_by(SignedBigInteger const&) const;

    SignedBigInteger& subtract(UnsignedBigInteger const&);
    SignedBigInteger subtracted_by(UnsignedBigInteger const&) const;

    SignedBigInteger& multiply(i64);
    SignedBigInteger multiplied_by(i64) const;

    SignedBigInteger& multiply(SignedBigInteger const&);
    SignedBigInteger multiplied_by(SignedBigInteger const&) const;

    SignedBigInteger& multiply(UnsignedBigInteger const&);
    SignedBigInteger multiplied_by(UnsignedBigInteger const&) const;

    SignedDivisionResult divided_by(SignedBigInteger const&) const;
    SignedDivisionResult divided_by(UnsignedBigInteger const&) const;

    SignedBigInteger& shift_left(size_t num_bits);
    SignedBigInteger shifted_left(size_t num_bits) const;

    SignedBigInteger& shift_right(size_t num_bits);
    SignedBigInteger shifted_right(size_t num_bits) const;

    SignedBigInteger& pow_assign(u32 exponent);
    SignedBigInteger pow(u32 exponent) const;

    SignedBigInteger bitwise_or(SignedBigInteger const&) const;
    SignedBigInteger bitwise_and(SignedBigInteger const&) const;
    SignedBigInteger bitwise_xor(SignedBigInteger const&) const;
    SignedBigInteger bitwise_not() const;

    ErrorOr<SignedBigInteger> mod_power_of_two(size_t power_of_two) const;

    SignedBigInteger negated_value() const;

    [[nodiscard]] u32 hash() const;

    [[nodiscard]] bool operator==(i64) const;
    [[nodiscard]] bool operator!=(i64) const;
    [[nodiscard]] bool operator<(i64) const;
    [[nodiscard]] bool operator<=(i64) const;
    [[nodiscard]] bool operator>(i64) const;
    [[nodiscard]] bool operator>=(i64) const;

    [[nodiscard]] bool operator==(SignedBigInteger const&) const;
    [[nodiscard]] bool operator!=(SignedBigInteger const&) const;
    [[nodiscard]] bool operator<(SignedBigInteger const&) const;
    [[nodiscard]] bool operator<=(SignedBigInteger const&) const;
    [[nodiscard]] bool operator>(SignedBigInteger const&) const;
    [[nodiscard]] bool operator>=(SignedBigInteger const&) const;

    [[nodiscard]] bool operator==(UnsignedBigInteger const&) const;
    [[nodiscard]] bool operator!=(UnsignedBigInteger const&) const;
    [[nodiscard]] bool operator<(UnsignedBigInteger const&) const;
    [[nodiscard]] bool operator<=(UnsignedBigInteger const&) const;
    [[nodiscard]] bool operator>(UnsignedBigInteger const&) const;
    [[nodiscard]] bool operator>=(UnsignedBigInteger const&) const;

    [[nodiscard]] UnsignedBigInteger::CompareResult compare_to_double(double) const;

private:
    mp_int m_mp {};
    mutable Optional<u32> m_hash {};
};

struct [[nodiscard]] SignedDivisionResult {
    Crypto::SignedBigInteger quotient;
    Crypto::SignedBigInteger remainder;
};

}

template<>
struct AK::Formatter<Crypto::SignedBigInteger> : AK::Formatter<Crypto::UnsignedBigInteger> {
    ErrorOr<void> format(FormatBuilder&, Crypto::SignedBigInteger const&);
};

inline Crypto::SignedBigInteger operator""_sbigint(char const* string, size_t length)
{
    return MUST(Crypto::SignedBigInteger::from_base(10, { string, length }));
}
