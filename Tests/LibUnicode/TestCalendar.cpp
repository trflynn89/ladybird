/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibTest/TestCase.h>

#include <LibUnicode/Calendar.h>
#include <LibUnicode/UnicodeKeywords.h>

TEST_CASE(foo)
{
    Unicode::ISODate date {
        .year = 1989,
        .month = 0,
        .day = 23,
    };

    // for (auto const& calendar : Unicode::available_calendars())
    //     (void)Unicode::iso_date_to_calendar_date(calendar, date);

    for (auto const& calendar : Unicode::available_calendars())
        (void)Unicode::add_duration_to_iso_date_in_calendar(calendar, date, { .years = 2, .days = 1 });
}
