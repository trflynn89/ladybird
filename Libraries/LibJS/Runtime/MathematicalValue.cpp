/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibJS/Runtime/MathematicalValue.h>

#include <gmp.h>
#include <mpfr.h>

namespace JS {

struct MathematicalValue::Value {
    Value()
    {
        static constexpr mpfr_prec_t PRECISION = 128;
        mpfr_init2(value, PRECISION);
    }

    ~Value()
    {
        mpfr_clear(value);
    }

    mpfr_t value;
};

MathematicalValue::MathematicalValue()
    : m_value(make<MathematicalValue::Value>())
{
}

MathematicalValue::MathematicalValue(double value)
    : m_value(make<MathematicalValue::Value>())
{
    mpfr_set_d(m_value->value, value, MPFR_RNDN);
}

MathematicalValue::MathematicalValue(MathematicalValue const& other)
    : m_value(make<MathematicalValue::Value>())
{
    mpfr_set(m_value->value, other.m_value->value, MPFR_RNDN);
}

MathematicalValue::MathematicalValue(MathematicalValue&& other)
    : m_value(move(other.m_value))
{
}

MathematicalValue& MathematicalValue::operator=(MathematicalValue const& other)
{
    if (this != &other)
        mpfr_set(m_value->value, other.m_value->value, MPFR_RNDN);
    return *this;
}

MathematicalValue& MathematicalValue::operator=(MathematicalValue&& other)
{
    if (this != &other)
        m_value = move(other.m_value);
    return *this;
}

MathematicalValue::~MathematicalValue() = default;

double MathematicalValue::to_double() const
{
    return mpfr_get_d(m_value->value, MPFR_RNDN);
}

MathematicalValue MathematicalValue::add(double other) const
{
    MathematicalValue result;
    mpfr_add_d(result.m_value->value, m_value->value, other, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::add(MathematicalValue const& other) const
{
    MathematicalValue result;
    mpfr_add(result.m_value->value, m_value->value, other.m_value->value, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::subtract(double other) const
{
    MathematicalValue result;
    mpfr_sub_d(result.m_value->value, m_value->value, other, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::subtract(MathematicalValue const& other) const
{
    MathematicalValue result;
    mpfr_sub(result.m_value->value, m_value->value, other.m_value->value, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::multiply(double other) const
{
    MathematicalValue result;
    mpfr_mul_d(result.m_value->value, m_value->value, other, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::multiply(MathematicalValue const& other) const
{
    MathematicalValue result;
    mpfr_mul(result.m_value->value, m_value->value, other.m_value->value, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::divide(double other) const
{
    MathematicalValue result;
    mpfr_div_d(result.m_value->value, m_value->value, other, MPFR_RNDN);
    return result;
}

MathematicalValue MathematicalValue::divide(MathematicalValue const& other) const
{
    MathematicalValue result;
    mpfr_div(result.m_value->value, m_value->value, other.m_value->value, MPFR_RNDN);
    return result;
}

}
