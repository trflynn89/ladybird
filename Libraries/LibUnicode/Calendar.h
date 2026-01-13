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
    Optional<u32> month;
    Optional<u32> day;
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

Optional<ISODate> add_duration_to_iso_date_in_calendar(StringView calendar, ISODate iso_date, DateDuration const&);
DateDuration calendar_until(StringView calendar, ISODate one, ISODate two, Unit largest_unit);

CalendarDate iso_date_to_calendar_date(StringView calendar, ISODate iso_date);
ISODate calendar_date_to_iso_date(StringView calendar, NonISODate const& iso_date);

}
