/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibTest/TestCase.h>

#include <AK/BigInteger.h>
#include <AK/NumericLimits.h>

static AK::BigInteger bigint_fibonacci(size_t n)
{
    AK::BigInteger num1 { 0 };
    AK::BigInteger num2 { 1 };

    for (size_t i = 0; i < n; ++i) {
        auto tmp = num1;
        num1.add(num2);
        num2 = tmp;
    }

    return num1;
}

TEST_CASE(zero)
{
    {
        AK::BigInteger value;
        EXPECT(value.is_zero());
        EXPECT(!value.is_negative());
        EXPECT(!value.is_positive());
    }
    {
        AK::BigInteger value { 0 };
        EXPECT(value.is_zero());
        EXPECT(!value.is_negative());
        EXPECT(!value.is_positive());
    }
}

TEST_CASE(integral_limits)
{
    {
        AK::BigInteger min { NumericLimits<i8>::min() };
        EXPECT_EQ(min.to_integer<i8>(), NumericLimits<i8>::min());
        EXPECT_EQ(min.to_base(10), "-128"sv);

        AK::BigInteger max { NumericLimits<i8>::max() };
        EXPECT_EQ(max.to_integer<i8>(), NumericLimits<i8>::max());
        EXPECT_EQ(max.to_base(10), "127"sv);
    }
    {
        AK::BigInteger min { NumericLimits<i16>::min() };
        EXPECT_EQ(min.to_integer<i16>(), NumericLimits<i16>::min());
        EXPECT_EQ(min.to_base(10), "-32768"sv);

        AK::BigInteger max { NumericLimits<i16>::max() };
        EXPECT_EQ(max.to_integer<i16>(), NumericLimits<i16>::max());
        EXPECT_EQ(max.to_base(10), "32767"sv);
    }
    {
        AK::BigInteger min { NumericLimits<i32>::min() };
        EXPECT_EQ(min.to_integer<i32>(), NumericLimits<i32>::min());
        EXPECT_EQ(min.to_base(10), "-2147483648"sv);

        AK::BigInteger max { NumericLimits<i32>::max() };
        EXPECT_EQ(max.to_integer<i32>(), NumericLimits<i32>::max());
        EXPECT_EQ(max.to_base(10), "2147483647"sv);
    }
    {
        AK::BigInteger min { NumericLimits<i64>::min() };
        EXPECT_EQ(min.to_integer<i64>(), NumericLimits<i64>::min());
        EXPECT_EQ(min.to_base(10), "-9223372036854775808"sv);

        AK::BigInteger max { NumericLimits<i64>::max() };
        EXPECT_EQ(max.to_integer<i64>(), NumericLimits<i64>::max());
        EXPECT_EQ(max.to_base(10), "9223372036854775807"sv);
    }
    {
        AK::BigInteger min { NumericLimits<u8>::min() };
        EXPECT_EQ(min.to_integer<u8>(), NumericLimits<u8>::min());
        EXPECT_EQ(min.to_base(10), "0"sv);

        AK::BigInteger max { NumericLimits<u8>::max() };
        EXPECT_EQ(max.to_integer<u8>(), NumericLimits<u8>::max());
        EXPECT_EQ(max.to_base(10), "255"sv);
    }
    {
        AK::BigInteger min { NumericLimits<u16>::min() };
        EXPECT_EQ(min.to_integer<u16>(), NumericLimits<u16>::min());
        EXPECT_EQ(min.to_base(10), "0"sv);

        AK::BigInteger max { NumericLimits<u16>::max() };
        EXPECT_EQ(max.to_integer<u16>(), NumericLimits<u16>::max());
        EXPECT_EQ(max.to_base(10), "65535"sv);
    }
    {
        AK::BigInteger min { NumericLimits<u32>::min() };
        EXPECT_EQ(min.to_integer<u32>(), NumericLimits<u32>::min());
        EXPECT_EQ(min.to_base(10), "0"sv);

        AK::BigInteger max { NumericLimits<u32>::max() };
        EXPECT_EQ(max.to_integer<u32>(), NumericLimits<u32>::max());
        EXPECT_EQ(max.to_base(10), "4294967295"sv);
    }
    {
        AK::BigInteger min { NumericLimits<u64>::min() };
        EXPECT_EQ(min.to_integer<u64>(), NumericLimits<u64>::min());
        EXPECT_EQ(min.to_base(10), "0"sv);

        AK::BigInteger max { NumericLimits<u64>::max() };
        EXPECT_EQ(max.to_integer<u64>(), NumericLimits<u64>::max());
        EXPECT_EQ(max.to_base(10), "18446744073709551615"sv);
    }
}

TEST_CASE(absolute)
{
    {
        AK::BigInteger value { 0 };

        EXPECT(value.is_zero());
        EXPECT(!value.is_negative());
        EXPECT(!value.is_positive());
    }
}

TEST_CASE(division)
{
    {
        AK::BigInteger num1 { 27194 };
        AK::BigInteger num2 { 251 };
        auto result = num1.divide(num2);

        AK::BigDivisionResult expected { AK::BigInteger { 108 }, AK::BigInteger { 86 } };
        EXPECT_EQ(result.quotient, expected.quotient);
        EXPECT_EQ(result.remainder, expected.remainder);
    }
    {
        auto num1 = bigint_fibonacci(497);
        auto num2 = bigint_fibonacci(238);

        auto result = num1.divide(num2);
        result.quotient.multiply(num2);
        result.quotient.add(result.remainder);

        EXPECT_EQ(result.quotient, num1);
    }
}
