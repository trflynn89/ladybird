/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/BigInteger.h>
#include <AK/Vector.h>

#include <gmp.h>

namespace AK {

// GMP doesn't have support for explicit 64-bit values; its methods accept (unsigned) long ints, which could be either
// 32- or 64-bit.
template<Integral T>
static constexpr inline bool GMPSupportsIntegerType = sizeof(T) < sizeof(long int);

//
template<Integral T>
static inline MakeUnsigned<T> safe_absolute(T value)
{
    VERIFY(value < 0);
    return static_cast<MakeUnsigned<T>>(-(value + 1)) + 1;
}

struct BigInteger::Value {
    Value()
    {
        mpz_init(value);
    }

    ~Value()
    {
        mpz_clear(value);
    }

    mpz_t value;
};

BigInteger::BigInteger()
    : m_value(make<BigInteger::Value>())
{
}

template<Integral T>
BigInteger::BigInteger(T value)
    : BigInteger()
{
    *this = value;
}

template BigInteger::BigInteger(i8);
template BigInteger::BigInteger(i16);
template BigInteger::BigInteger(i32);
template BigInteger::BigInteger(i64);
template BigInteger::BigInteger(u8);
template BigInteger::BigInteger(u16);
template BigInteger::BigInteger(u32);
template BigInteger::BigInteger(u64);

BigInteger::BigInteger(double value)
    : BigInteger()
{
    *this = value;
}

BigInteger::BigInteger(BigInteger const& other)
    : m_value(make<BigInteger::Value>())
{
    mpz_set(m_value->value, other.m_value->value);
}

BigInteger::BigInteger(BigInteger&& other)
    : m_value(move(other.m_value))
{
}

BigInteger::~BigInteger() = default;

template<Integral T>
BigInteger& BigInteger::operator=(T other)
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            mpz_set_si(m_value->value, other);
        } else {
            mpz_set_ui(m_value->value, other);
        }
    } else {
        // On systems
        if constexpr (IsSigned<T>) {
            mpz_set_si(m_value->value, static_cast<i32>(other >> 32));
            mpz_mul_2exp(m_value->value, m_value->value, 32);
            mpz_add_ui(m_value->value, m_value->value, static_cast<u32>(other));
        } else {
            mpz_set_ui(m_value->value, static_cast<u32>(other >> 32));
            mpz_mul_2exp(m_value->value, m_value->value, 32);
            mpz_add_ui(m_value->value, m_value->value, static_cast<u32>(other));
        }
    }

    return *this;
}

BigInteger& BigInteger::operator=(double other)
{
    mpz_set_d(m_value->value, other);
    return *this;
}

BigInteger& BigInteger::operator=(BigInteger const& other)
{
    if (this != &other)
        mpz_set(m_value->value, other.m_value->value);
    return *this;
}

BigInteger& BigInteger::operator=(BigInteger&& other)
{
    if (this != &other)
        m_value = move(other.m_value);
    return *this;
}

template BigInteger& BigInteger::operator=(i8);
template BigInteger& BigInteger::operator=(i16);
template BigInteger& BigInteger::operator=(i32);
template BigInteger& BigInteger::operator=(i64);
template BigInteger& BigInteger::operator=(u8);
template BigInteger& BigInteger::operator=(u16);
template BigInteger& BigInteger::operator=(u32);
template BigInteger& BigInteger::operator=(u64);

ErrorOr<BigInteger> BigInteger::from_base(ByteString const& value, int base)
{
    BigInteger result;

    if (mpz_set_str(result.m_value->value, value.characters(), base) == -1)
        return Error::from_string_literal("Input contains invalid character for desired base");

    return result;
}

ByteString BigInteger::to_base(int base) const
{
    // We must include space for the NUL-terminator and negative sign (if needed).
    auto capacity = mpz_sizeinbase(m_value->value, base) + static_cast<size_t>(is_negative()) + 1;

    // FIXME: It would be nicer if ByteString had an API to adopt an already-allocated buffer. We can't just use the
    //        result of the above capacity computation to create a StringImpl, because GMP notes that computation may
    //        result in more than the minimum number of bytes actually needed.
    Vector<char, 256> buffer;
    buffer.ensure_capacity(capacity);

    mpz_get_str(buffer.data(), base, m_value->value);
    return ByteString { buffer.data() };
}

template<Integral T>
T BigInteger::to_integer() const
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            return static_cast<T>(mpz_get_si(m_value->value));
        } else {
            return static_cast<T>(mpz_get_ui(m_value->value));
        }
    } else {
        BigInteger::Value tmp;
        mpz_mod_2exp(tmp.value, m_value->value, 64);

        T high = 0;
        T low = mpz_get_ui(tmp.value) & 0xffff'ffff;
        mpz_div_2exp(tmp.value, tmp.value, 32);

        if constexpr (IsSigned<T>) {
            high = mpz_get_si(tmp.value);
        } else {
            high = mpz_get_ui(tmp.value);
        }

        return (high << 32) + low;
    }
}

template i8 BigInteger::to_integer() const;
template i16 BigInteger::to_integer() const;
template i32 BigInteger::to_integer() const;
template i64 BigInteger::to_integer() const;
template u8 BigInteger::to_integer() const;
template u16 BigInteger::to_integer() const;
template u32 BigInteger::to_integer() const;
template u64 BigInteger::to_integer() const;

double BigInteger::to_double() const
{
    return mpz_get_d(m_value->value);
}

bool BigInteger::is_zero() const
{
    return mpz_sgn(m_value->value) == 0;
}

bool BigInteger::is_negative() const
{
    return mpz_sgn(m_value->value) == -1;
}

bool BigInteger::is_positive() const
{
    return mpz_sgn(m_value->value) == 1;
}

void BigInteger::negate()
{
    mpz_neg(m_value->value, m_value->value);
}

BigInteger BigInteger::absolute() const
{
    BigInteger result;
    mpz_abs(result.m_value->value, m_value->value);
    return result;
}

template<Integral T>
bool BigInteger::operator==(T other) const
{
    return this->operator<=>(other) == 0;
}

bool BigInteger::operator==(double other) const
{
    return this->operator<=>(other) == 0;
}

bool BigInteger::operator==(BigInteger const& other) const
{
    return this->operator<=>(other) == 0;
}

template bool BigInteger::operator==(i8) const;
template bool BigInteger::operator==(i16) const;
template bool BigInteger::operator==(i32) const;
template bool BigInteger::operator==(i64) const;
template bool BigInteger::operator==(u8) const;
template bool BigInteger::operator==(u16) const;
template bool BigInteger::operator==(u32) const;
template bool BigInteger::operator==(u64) const;

template<Integral T>
int BigInteger::operator<=>(T other) const
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            return mpz_cmp_si(m_value->value, other);
        } else {
            return mpz_cmp_ui(m_value->value, other);
        }
    } else {
        return this->operator<=>(BigInteger { other });
    }
}

int BigInteger::operator<=>(double other) const
{
    return mpz_cmp_d(m_value->value, other);
}

int BigInteger::operator<=>(BigInteger const& other) const
{
    return mpz_cmp(m_value->value, other.m_value->value);
}

template int BigInteger::operator<=>(i8) const;
template int BigInteger::operator<=>(i16) const;
template int BigInteger::operator<=>(i32) const;
template int BigInteger::operator<=>(i64) const;
template int BigInteger::operator<=>(u8) const;
template int BigInteger::operator<=>(u16) const;
template int BigInteger::operator<=>(u32) const;
template int BigInteger::operator<=>(u64) const;

template<Integral T>
void BigInteger::add(T value)
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            if (value < 0) {
                subtract(safe_absolute(value));
                return;
            }
        }

        mpz_add_ui(m_value->value, m_value->value, static_cast<unsigned long int>(value));
    } else {
        add(BigInteger { value });
    }
}

void BigInteger::add(BigInteger const& value)
{
    mpz_add(m_value->value, m_value->value, value.m_value->value);
}

template void BigInteger::add(i8);
template void BigInteger::add(i16);
template void BigInteger::add(i32);
template void BigInteger::add(i64);
template void BigInteger::add(u8);
template void BigInteger::add(u16);
template void BigInteger::add(u32);
template void BigInteger::add(u64);

template<Integral T>
void BigInteger::subtract(T value)
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            if (value < 0) {
                add(safe_absolute(value));
                return;
            }
        }

        mpz_sub_ui(m_value->value, m_value->value, static_cast<unsigned long int>(value));
    } else {
        subtract(BigInteger { value });
    }
}

void BigInteger::subtract(BigInteger const& value)
{
    mpz_sub(m_value->value, m_value->value, value.m_value->value);
}

template void BigInteger::subtract(i8);
template void BigInteger::subtract(i16);
template void BigInteger::subtract(i32);
template void BigInteger::subtract(i64);
template void BigInteger::subtract(u8);
template void BigInteger::subtract(u16);
template void BigInteger::subtract(u32);
template void BigInteger::subtract(u64);

template<Integral T>
void BigInteger::multiply(T value)
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            mpz_mul_si(m_value->value, m_value->value, value);
        } else {
            mpz_mul_ui(m_value->value, m_value->value, value);
        }
    } else {
        multiply(BigInteger { value });
    }
}

void BigInteger::multiply(BigInteger const& value)
{
    mpz_mul(m_value->value, m_value->value, value.m_value->value);
}

template void BigInteger::multiply(i8);
template void BigInteger::multiply(i16);
template void BigInteger::multiply(i32);
template void BigInteger::multiply(i64);
template void BigInteger::multiply(u8);
template void BigInteger::multiply(u16);
template void BigInteger::multiply(u32);
template void BigInteger::multiply(u64);

template<Integral T>
BigDivisionResult BigInteger::divide(T value)
{
    if constexpr (GMPSupportsIntegerType<T>) {
        if constexpr (IsSigned<T>) {
            if (value < 0) {
                auto result = divide(safe_absolute(value));
                result.remainder.negate();
                return result;
            }
        }

        BigInteger quotient;
        BigInteger remainder;

        mpz_tdiv_qr_ui(quotient.m_value->value, remainder.m_value->value, m_value->value, value);
        return { .quotient = move(quotient), .remainder = move(remainder) };
    } else {
        return divide(BigInteger { value });
    }
}

BigDivisionResult BigInteger::divide(BigInteger const& value)
{
    BigInteger quotient;
    BigInteger remainder;

    mpz_tdiv_qr(quotient.m_value->value, remainder.m_value->value, m_value->value, value.m_value->value);
    return { .quotient = move(quotient), .remainder = move(remainder) };
}

template BigDivisionResult BigInteger::divide(i8);
template BigDivisionResult BigInteger::divide(i16);
template BigDivisionResult BigInteger::divide(i32);
template BigDivisionResult BigInteger::divide(i64);
template BigDivisionResult BigInteger::divide(u8);
template BigDivisionResult BigInteger::divide(u16);
template BigDivisionResult BigInteger::divide(u32);
template BigDivisionResult BigInteger::divide(u64);

}
