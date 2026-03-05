/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/String.h>

namespace Unicode {

// 3.5.1 ISO Date Records, https://tc39.es/proposal-temporal/#sec-temporal-iso-date-records
struct ISODate {
    i32 year { 0 };
    u8 month { 0 };
    u8 day { 0 };
};

// 14.3 The Year-Week Record Specification Type, https://tc39.es/proposal-temporal/#sec-year-week-record-specification-type
struct YearWeek {
    Optional<u8> week;
    Optional<i32> year;
};

// 12.3.1 Calendar Date Records, https://tc39.es/proposal-temporal/#sec-temporal-calendar-date-records
struct CalendarDate {
    Optional<String> era;
    Optional<i32> era_year;
    i32 year { 0 };
    u8 month { 0 };
    String month_code;
    u8 day { 0 };
    u8 day_of_week { 0 };
    u16 day_of_year { 0 };
    YearWeek week_of_year;
    u8 days_in_week { 0 };
    u8 days_in_month { 0 };
    u16 days_in_year { 0 };
    u8 months_in_year { 0 };
    bool in_leap_year { false };
};

constexpr inline auto HEBREW_ADAR_I_MONTH_CODE = "M05L"sv;
constexpr inline auto HEBREW_ADAR_I_MONTH_NUMBER = 5u;

CalendarDate calendar_date_from_iso(String const& calendar, ISODate);
Optional<ISODate> iso_date_from_calendar(String const& calendar, i32 year, u8 month, u8 day);
bool calendar_supports_era(String const& calendar);
u8 calendar_months_in_year(String const& calendar, i32 arithmetic_year);
u8 calendar_days_in_month(String const& calendar, i32 arithmetic_year, u8 ordinal_month);

// For Chinese/Dangi: get the month number and leap flag for a given ordinal month in a year.
// Returns (month_number, is_leap). E.g., for ordinal 5 in a year with leap month 4:
// ordinals 1-4 → (1-4, false), ordinal 5 → (4, true), ordinals 6-13 → (5-12, false).
struct OrdinalMonthInfo {
    u8 month_number { 0 };
    bool is_leap { false };
};
Optional<OrdinalMonthInfo> chinese_ordinal_month_info(String const& calendar, i32 arithmetic_year, u8 ordinal_month);

}
