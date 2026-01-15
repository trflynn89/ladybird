/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ByteString.h>
#include <AK/OwnPtr.h>
#include <LibUnicode/Calendar.h>
#include <LibUnicode/ICU.h>

#include <unicode/calendar.h>
#include <unicode/dtfmtsym.h>

namespace Unicode {

// NOTE OVERFLOW == setLenient

#define ICU_TRY(expression)                                                      \
    ({                                                                           \
        /* Ignore -Wshadow to allow nesting the macro. */                        \
        AK_IGNORE_DIAGNOSTIC("-Wshadow", auto _temporary_result = (expression)); \
        if (icu_failure(status)) [[unlikely]]                                    \
            return {};                                                           \
        _temporary_result;                                                       \
    })

#define ICU_TRY_VOID(expression)                          \
    ({                                                    \
        /* Ignore -Wshadow to allow nesting the macro. */ \
        AK_IGNORE_DIAGNOSTIC("-Wshadow", (expression));   \
        if (icu_failure(status)) [[unlikely]]             \
            return {};                                    \
    })

static CalendarDate create_calendar_date(icu::Calendar const& calendar)
{
    UErrorCode status = U_ZERO_ERROR;
    CalendarDate result;

    auto store_calendar_field = [&](auto& storage, UCalendarDateFields field) {
        if (auto result = calendar.get(field, status); icu_success(status))
            storage = result;
    };

    auto store_calendar_maximum = [&](auto& storage, UCalendarDateFields field) {
        if (auto result = calendar.getActualMaximum(field, status); icu_success(status))
            storage = result;
    };

    if (auto era = calendar.get(UCAL_ERA, status); icu_success(status)) {
        if (icu::DateFormatSymbols symbols { calendar.getType(), status }; icu_success(status)) {
            i32 count = 0;
            auto const* era_names = symbols.getEras(count);

            if (era >= 0 && era < count) {
                result.era = MUST(icu_string_to_string(era_names[era]).to_lowercase());
                store_calendar_field(result.era_year, UCAL_YEAR);
            }
        }
    }

    store_calendar_field(result.year, UCAL_EXTENDED_YEAR);
    store_calendar_field(result.month, UCAL_MONTH);
    store_calendar_field(result.day, UCAL_DATE);
    store_calendar_field(result.day_of_week, UCAL_DAY_OF_WEEK);
    store_calendar_field(result.day_of_year, UCAL_DAY_OF_YEAR);
    store_calendar_field(result.week_of_year.week, UCAL_WEEK_OF_YEAR);
    store_calendar_field(result.week_of_year.year, UCAL_YEAR_WOY);

    if (auto const* month_code = calendar.getTemporalMonthCode(status); icu_success(status))
        result.month_code = MUST(String::from_utf8({ month_code, strlen(month_code) }));

    store_calendar_maximum(result.days_in_week, UCAL_DAY_OF_WEEK);
    store_calendar_maximum(result.days_in_month, UCAL_DAY_OF_MONTH);
    store_calendar_maximum(result.days_in_year, UCAL_DAY_OF_YEAR);
    store_calendar_maximum(result.months_in_year, UCAL_MONTH);
    ++result.months_in_year; // Increment by 1 because UCAL_MONTH is zero-based.

    result.in_leap_year = calendar.inTemporalLeapYear(status);

    return result;
}

static icu::Calendar& iso_calendar_at_iso_date(ISODate iso_date)
{
    auto iso8601_calendar = CalendarData::for_calendar("iso8601"sv);
    VERIFY(iso8601_calendar.has_value());

    iso8601_calendar->calendar().set(UCAL_EXTENDED_YEAR, iso_date.year);
    iso8601_calendar->calendar().set(UCAL_MONTH, iso_date.month);
    iso8601_calendar->calendar().set(UCAL_DATE, iso_date.day);
    // iso8601_calendar->calendar().set(iso_date.year, iso_date.month, iso_date.day);

    return iso8601_calendar->calendar();
}

static Optional<icu::Calendar&> calendar_at_iso_date(StringView calendar, ISODate iso_date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto& iso8601_calendar = iso_calendar_at_iso_date(iso_date);
    auto time = ICU_TRY(iso8601_calendar.getTime(status));

    dbgln("{}:\t{}/{}/{}",
        iso8601_calendar.getType(),
        iso8601_calendar.get(UCAL_MONTH, status) + 1,
        iso8601_calendar.get(UCAL_DATE, status),
        iso8601_calendar.get(UCAL_EXTENDED_YEAR, status));

    auto target_calendar = CalendarData::for_calendar(calendar);
    if (!target_calendar.has_value())
        return {};

    ICU_TRY_VOID(target_calendar->calendar().setTime(time, status));
    return target_calendar->calendar();
}

Optional<ISODate> add_duration_to_iso_date_in_calendar(StringView calendar, ISODate iso_date, DateDuration const& duration, Lenient lenient)
{
    dbgln("!!! {} years={} months={} weeks={} days={}", calendar, duration.years, duration.months, duration.weeks, duration.days);
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, iso_date);
    if (!target_calendar.has_value())
        return {};

    target_calendar->setLenient(static_cast<UBool>(lenient == Lenient::Yes));

    dbgln("{}:\t{}/{}/{}",
        target_calendar->getType(),
        target_calendar->get(UCAL_MONTH, status) + 1,
        target_calendar->get(UCAL_DATE, status),
        target_calendar->get(UCAL_EXTENDED_YEAR, status));

    if (duration.years != 0)
        ICU_TRY_VOID(target_calendar->add(UCAL_EXTENDED_YEAR, static_cast<i32>(duration.years), status));
    if (duration.months != 0)
        ICU_TRY_VOID(target_calendar->add(UCAL_MONTH, static_cast<i32>(duration.months), status));

    auto days = duration.days;

    if (duration.weeks != 0) {
        auto days_in_week = ICU_TRY(target_calendar->getActualMaximum(UCAL_DAY_OF_WEEK, status));
        days += days_in_week * duration.weeks;
    }

    if (days != 0)
        ICU_TRY_VOID(target_calendar->add(UCAL_DATE, static_cast<i32>(days), status));

    dbgln("{}:\t{}/{}/{}",
        target_calendar->getType(),
        target_calendar->get(UCAL_MONTH, status) + 1,
        target_calendar->get(UCAL_DATE, status),
        target_calendar->get(UCAL_EXTENDED_YEAR, status));

    auto time = ICU_TRY(target_calendar->getTime(status));

    auto iso8601_calendar = CalendarData::for_calendar("iso8601"sv);
    ICU_TRY_VOID(iso8601_calendar->calendar().setTime(time, status));

    auto year = ICU_TRY(iso8601_calendar->calendar().get(UCAL_EXTENDED_YEAR, status));
    auto month = ICU_TRY(iso8601_calendar->calendar().get(UCAL_MONTH, status));
    auto day = ICU_TRY(iso8601_calendar->calendar().get(UCAL_DATE, status));

    dbgln("iso8601:\t{}/{}/{}\n\n", month + 1, day, year);
    return ISODate { .year = year, .month = static_cast<u8>(month), .day = static_cast<u8>(day) };
}

DateDuration calendar_until(StringView calendar, ISODate one, ISODate two, Unit largest_unit)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, two);
    if (!target_calendar.has_value())
        return {};

    auto second_time = ICU_TRY(target_calendar->getTime(status));

    target_calendar = calendar_at_iso_date(calendar, one);
    if (!target_calendar.has_value())
        return {};

    i32 years { 0 };
    i32 months { 0 };
    i32 weeks { 0 };
    i32 days { 0 };

    if (largest_unit == Unit::Year)
        years = ICU_TRY(target_calendar->fieldDifference(second_time, UCAL_EXTENDED_YEAR, status));
    if (largest_unit <= Unit::Month)
        months = ICU_TRY(target_calendar->fieldDifference(second_time, UCAL_MONTH, status));

    days = ICU_TRY(target_calendar->fieldDifference(second_time, UCAL_DATE, status));

    if (largest_unit == Unit::Week) {
        auto days_in_week = ICU_TRY(target_calendar->getActualMaximum(UCAL_DAY_OF_WEEK, status));
        weeks = days / days_in_week;
        days = days % days_in_week;
    }

    return {
        .years = static_cast<double>(years),
        .months = static_cast<double>(months),
        .weeks = static_cast<double>(weeks),
        .days = static_cast<double>(days),
    };
}

CalendarDate iso_date_to_calendar_date(StringView calendar, ISODate iso_date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, iso_date);
    if (!target_calendar.has_value())
        return {};

    auto result = create_calendar_date(*target_calendar);

    dbgln("{}:\tera={} era_year={} year={} ({}) month={} month_code={} day={} day_of_week={} day_of_year={} week_of_year.week={} week_of_year.year={} days_in_week={} days_in_month={} days_in_year={} months_in_year={} in_leap_year={}",
        target_calendar->getType(),
        result.era,
        result.era_year,
        result.year,
        target_calendar->get(UCAL_YEAR, status),
        result.month,
        result.month_code,
        result.day,
        result.day_of_week,
        result.day_of_year,
        result.week_of_year.week,
        result.week_of_year.year,
        result.days_in_week,
        result.days_in_month,
        result.days_in_year,
        result.months_in_year,
        result.in_leap_year);
    dbgln();

    return result;
}

Optional<ISODate> calendar_date_to_iso_date(StringView calendar, NonISODate const& date, Lenient lenient)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = CalendarData::for_calendar(calendar);
    if (!target_calendar.has_value())
        return {};

    target_calendar->calendar().setLenient(static_cast<UBool>(lenient == Lenient::Yes));

    if (date.year.has_value())
        target_calendar->calendar().set(UCAL_EXTENDED_YEAR, *date.year);
    else if (date.era_year.has_value())
        target_calendar->calendar().set(UCAL_YEAR, *date.era_year);
    else
        VERIFY_NOT_REACHED();

    target_calendar->calendar().set(UCAL_ORDINAL_MONTH, date.month.value_or(0));
    target_calendar->calendar().set(UCAL_DATE, date.day.value_or(1));

    dbgln("{}:\t{}/{}/{}",
        target_calendar->calendar().getType(),
        target_calendar->calendar().get(UCAL_MONTH, status) + 1,
        target_calendar->calendar().get(UCAL_DATE, status),
        target_calendar->calendar().get(UCAL_EXTENDED_YEAR, status));

    auto time = ICU_TRY(target_calendar->calendar().getTime(status));

    auto iso8601_calendar = CalendarData::for_calendar("iso8601"sv);
    ICU_TRY_VOID(iso8601_calendar->calendar().setTime(time, status));

    auto store_calendar_field = [&](auto& storage, UCalendarDateFields field) {
        if (auto result = iso8601_calendar->calendar().get(field, status); icu_success(status))
            storage = result;
    };

    ISODate result;
    store_calendar_field(result.year, UCAL_EXTENDED_YEAR);
    store_calendar_field(result.month, UCAL_MONTH);
    store_calendar_field(result.day, UCAL_DATE);

    dbgln("{}:\t{}/{}/{}\n",
        iso8601_calendar->calendar().getType(),
        iso8601_calendar->calendar().get(UCAL_MONTH, status) + 1,
        iso8601_calendar->calendar().get(UCAL_DATE, status),
        iso8601_calendar->calendar().get(UCAL_EXTENDED_YEAR, status));

    return result;
}

Optional<String> calendar_era(StringView calendar, ISODate date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, date);
    if (!target_calendar.has_value())
        return {};

    auto era = ICU_TRY(target_calendar->get(UCAL_ERA, status));
    auto symbols = ICU_TRY(icu::DateFormatSymbols(target_calendar->getType(), status));

    i32 count = 0;
    auto const* era_names = symbols.getEras(count);

    if (era < 0 || era >= count)
        return {};

    return MUST(icu_string_to_string(era_names[era]).to_lowercase());
}

Optional<i32> calendar_era_year(StringView calendar, ISODate date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, date);
    if (!target_calendar.has_value())
        return {};

    return ICU_TRY(target_calendar->get(UCAL_ERA, status));
}

Optional<i32> calendar_year(StringView calendar, ISODate date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = calendar_at_iso_date(calendar, date);
    if (!target_calendar.has_value())
        return {};

    return ICU_TRY(target_calendar->get(UCAL_EXTENDED_YEAR, status));
}

bool calendar_year_contains_month_code(StringView calendar, i32 year, StringView month_code)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = CalendarData::for_calendar(calendar);
    if (!target_calendar.has_value())
        return false;

    target_calendar->calendar().setLenient(0);
    target_calendar->calendar().set(UCAL_EXTENDED_YEAR, year);

    ICU_TRY_VOID(target_calendar->calendar().setTemporalMonthCode(ByteString { month_code }.characters(), status));
    char const* actual_month_code = ICU_TRY(target_calendar->calendar().getTemporalMonthCode(status));

    return month_code == actual_month_code;
}

u8 calendar_months_in_year(StringView calendar, i32 year)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = CalendarData::for_calendar(calendar);
    if (!target_calendar.has_value())
        return false;

    target_calendar->calendar().set(UCAL_EXTENDED_YEAR, year);

    return ICU_TRY(target_calendar->calendar().getActualMaximum(UCAL_MONTH, status));
}

u8 calendar_days_in_month(StringView calendar, i32 year, i8 month)
{
    UErrorCode status = U_ZERO_ERROR;

    auto target_calendar = CalendarData::for_calendar(calendar);
    if (!target_calendar.has_value())
        return false;

    target_calendar->calendar().set(UCAL_EXTENDED_YEAR, year);
    target_calendar->calendar().set(UCAL_ORDINAL_MONTH, month);

    return ICU_TRY(target_calendar->calendar().getActualMaximum(UCAL_DATE, status));
}

}
