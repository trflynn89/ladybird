/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/ByteString.h>
#include <AK/Concepts.h>
#include <AK/NonnullOwnPtr.h>

namespace AK {

struct BigDivisionResult;

class [[nodiscard]] BigInteger {
public:
    BigInteger();

    template<Integral T>
    explicit BigInteger(T);
    explicit BigInteger(double);

    BigInteger(BigInteger const&);
    BigInteger(BigInteger&&);

    ~BigInteger();

    template<Integral T>
    BigInteger& operator=(T);
    BigInteger& operator=(double);
    BigInteger& operator=(BigInteger const&);
    BigInteger& operator=(BigInteger&&);

    // We use ByteString here because the string must be NUL-terminated.
    static ErrorOr<BigInteger> from_base(ByteString const&, int base);
    ByteString to_base(int base) const;

    template<Integral T>
    T to_integer() const;
    double to_double() const;

    bool is_zero() const;
    bool is_negative() const;
    bool is_positive() const;

    void negate();
    BigInteger absolute() const;

    template<Integral T>
    bool operator==(T) const;
    bool operator==(double) const;
    bool operator==(BigInteger const&) const;

    template<Integral T>
    int operator<=>(T) const;
    int operator<=>(double) const;
    int operator<=>(BigInteger const&) const;

    template<Integral T>
    void add(T);
    void add(BigInteger const&);

    template<Integral T>
    void subtract(T);
    void subtract(BigInteger const&);

    template<Integral T>
    void multiply(T);
    void multiply(BigInteger const&);

    template<Integral T>
    BigDivisionResult divide(T);
    BigDivisionResult divide(BigInteger const&);

private:
    struct Value;
    NonnullOwnPtr<Value> m_value;
};

struct [[nodiscard]] BigDivisionResult {
    BigInteger quotient;
    BigInteger remainder;
};

}
