/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/String.h>
#include <AK/StringView.h>
#include <AK/Types.h>

namespace Unicode {

struct ISODate {
    i32 year { 0 };
    u8 month { 0 };
    u8 day { 0 };
};

struct NonISODate {
    Optional<i32> era_year;
    Optional<i32> year;
    Optional<i8> month;
    Optional<u8> day;
};

struct YearWeek {
    Optional<u8> week;
    Optional<i32> year;
};

struct DateDuration {
    double years { 0 };
    double months { 0 };
    double weeks { 0 };
    double days { 0 };
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

enum class Unit {
    Year,
    Month,
    Week,
    Day,
};

enum class Lenient {
    No,
    Yes,
};

Optional<ISODate> add_duration_to_iso_date_in_calendar(StringView calendar, ISODate iso_date, DateDuration const&, Lenient);
DateDuration calendar_until(StringView calendar, ISODate one, ISODate two, Unit largest_unit);

CalendarDate iso_date_to_calendar_date(StringView calendar, ISODate iso_date);
Optional<ISODate> calendar_date_to_iso_date(StringView calendar, NonISODate const& iso_date, Lenient);

Optional<String> calendar_era(StringView calendar, ISODate date);
Optional<i32> calendar_era_year(StringView calendar, ISODate date);
Optional<i32> calendar_year(StringView calendar, ISODate date);
bool calendar_year_contains_month_code(StringView calendar, i32 year, StringView month_code);
u8 calendar_months_in_year(StringView calendar, i32 year);
u8 calendar_days_in_month(StringView calendar, i32 year, i8 month);

}
