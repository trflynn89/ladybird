/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/HashMap.h>
#include <LibUnicode/Calendar.h>
#include <LibUnicode/ICU.h>

#include <unicode/calendar.h>

namespace Unicode {

static icu::Calendar& proleptic_gregorian_calendar()
{
    static auto GREGORIAN_CALENDAR = "gregory"_string;

    auto calendar_data = CalendarData::for_calendar(GREGORIAN_CALENDAR);
    VERIFY(calendar_data.has_value());

    return calendar_data->calendar();
}

static void set_icu_calendar_to_iso_date(icu::Calendar& calendar, ISODate iso_date)
{
    UErrorCode status = U_ZERO_ERROR;

    auto& gregorian = proleptic_gregorian_calendar();
    gregorian.clear();
    gregorian.set(UCAL_EXTENDED_YEAR, iso_date.year);
    gregorian.set(UCAL_MONTH, iso_date.month - 1);
    gregorian.set(UCAL_DATE, iso_date.day);

    auto epoch_ms = gregorian.getTime(status);
    if (icu_failure(status))
        return;

    calendar.setTime(epoch_ms, status);
}

// Check if a Hebrew year is a leap year (has Adar I / 13 months).
static bool is_hebrew_leap_year(icu::Calendar& calendar)
{
    static constexpr auto ADAR_I = 5;
    UErrorCode status = U_ZERO_ERROR;

    auto cloned = adopt_own(*calendar.clone());
    cloned->set(UCAL_MONTH, ADAR_I);
    cloned->set(UCAL_DATE, 1);

    auto month = cloned->get(UCAL_MONTH, status);
    return icu_success(status) && month == ADAR_I;
}

// Cached month layout for a Chinese/Dangi calendar year.
// Maps ordinal month (1-based) to (ICU month index, is_leap_month flag, days_in_month).
struct ChineseMonthEntry {
    i32 month_index { 0 };
    i32 is_leap { 0 };
    u8 days_in_month { 0 };
};

struct ChineseYearLayout {
    u8 month_count { 0 };
    ChineseMonthEntry months[14] {}; // months[1..month_count] are valid (1-indexed)
};

// Cache key: (calendar_type, extended_year). We use a combined key.
struct ChineseYearKey {
    String calendar;
    i32 extended_year { 0 };

    bool operator==(ChineseYearKey const& other) const = default;
};

}

namespace AK {

template<>
struct Traits<Unicode::ChineseYearKey> : public DefaultTraits<Unicode::ChineseYearKey> {
    static unsigned hash(Unicode::ChineseYearKey const& key) { return pair_int_hash(key.calendar.hash(), key.extended_year); }
};

}

namespace Unicode {

static HashMap<ChineseYearKey, ChineseYearLayout> s_chinese_year_cache;

// Cache: arithmetic_year → ICU EXTENDED_YEAR for Chinese/Dangi calendars.
struct ChineseExtYearKey {
    String calendar;
    i32 arithmetic_year { 0 };
    bool operator==(ChineseExtYearKey const&) const = default;
};

}

namespace AK {

template<>
struct Traits<Unicode::ChineseExtYearKey> : public DefaultTraits<Unicode::ChineseExtYearKey> {
    static unsigned hash(Unicode::ChineseExtYearKey const& key) { return pair_int_hash(key.calendar.hash(), key.arithmetic_year); }
};

}

namespace Unicode {

static HashMap<ChineseExtYearKey, i32> s_chinese_ext_year_cache;

static i32 chinese_arithmetic_to_extended(String const& calendar_name, icu::Calendar& calendar, i32 arithmetic_year)
{
    ChineseExtYearKey key { calendar_name, arithmetic_year };
    auto it = s_chinese_ext_year_cache.find(key);
    if (it != s_chinese_ext_year_cache.end())
        return it->value;

    UErrorCode status = U_ZERO_ERROR;
    ISODate approx_iso { arithmetic_year, 6, 15 };
    set_icu_calendar_to_iso_date(calendar, approx_iso);
    auto ext_year = calendar.get(UCAL_EXTENDED_YEAR, status);
    s_chinese_ext_year_cache.set(move(key), ext_year);
    return ext_year;
}

// Build and cache the month layout for a Chinese/Dangi year given the extended_year.
static ChineseYearLayout const& chinese_year_layout(String const& calendar_name, icu::Calendar& calendar)
{
    UErrorCode status = U_ZERO_ERROR;
    auto extended_year = calendar.get(UCAL_EXTENDED_YEAR, status);

    ChineseYearKey key { calendar_name, extended_year };
    auto it = s_chinese_year_cache.find(key);
    if (it != s_chinese_year_cache.end())
        return it->value;

    // Build the layout by walking from month 0 to end of year.
    auto cloned = adopt_own_if_nonnull(calendar.clone());
    cloned->set(UCAL_MONTH, 0);
    cloned->set(UCAL_IS_LEAP_MONTH, 0);
    cloned->set(UCAL_DATE, 1);
    auto resolved = cloned->getTime(status);
    cloned->setTime(resolved, status);

    auto start_year = cloned->get(UCAL_EXTENDED_YEAR, status);

    ChineseYearLayout layout;
    layout.month_count = 1;
    layout.months[1] = {
        cloned->get(UCAL_MONTH, status),
        cloned->get(UCAL_IS_LEAP_MONTH, status),
        static_cast<u8>(cloned->getActualMaximum(UCAL_DATE, status)),
    };

    for (u8 i = 0; i < 13; ++i) {
        cloned->add(UCAL_MONTH, 1, status);
        if (icu_failure(status))
            break;
        auto cur_year = cloned->get(UCAL_EXTENDED_YEAR, status);
        if (icu_failure(status) || cur_year != start_year)
            break;
        layout.month_count++;
        layout.months[layout.month_count] = {
            cloned->get(UCAL_MONTH, status),
            cloned->get(UCAL_IS_LEAP_MONTH, status),
            static_cast<u8>(cloned->getActualMaximum(UCAL_DATE, status)),
        };
    }

    return s_chinese_year_cache.ensure(move(key), [&] { return layout; });
}

// Map ordinal month to ICU month index for Hebrew calendar.
static i32 hebrew_ordinal_to_icu_month(bool is_leap, u8 ordinal_month)
{
    if (is_leap)
        return ordinal_month - 1; // 1-13 → 0-12
    // Common year: ordinals 1-5 → indices 0-4, ordinals 6-12 → indices 6-12.
    return (ordinal_month <= 5) ? (ordinal_month - 1) : ordinal_month;
}

// Compute the ordinal month (1-based position within the year).
// For Chinese/Dangi, leap months share the same UCAL_MONTH index with IS_LEAP_MONTH=1,
// so we need to count from the beginning of the year.
static u8 compute_ordinal_month(icu::Calendar& calendar, String const& calendar_name)
{
    UErrorCode status = U_ZERO_ERROR;
    auto month_index = calendar.get(UCAL_MONTH, status);
    if (icu_failure(status))
        return 1;

    if (calendar_name.is_one_of("chinese"sv, "dangi"sv)) {
        auto is_leap = calendar.get(UCAL_IS_LEAP_MONTH, status);
        if (icu_failure(status))
            is_leap = 0;

        auto const& layout = chinese_year_layout(calendar_name, calendar);
        for (u8 ord = 1; ord <= layout.month_count; ++ord) {
            if (layout.months[ord].month_index == month_index && layout.months[ord].is_leap == is_leap)
                return ord;
        }
        return 1;
    }

    if (calendar_name == "hebrew"sv) {
        // ICU Hebrew calendar always uses 13 month slots (0-12). In common years,
        // month index 5 (Adar I) is skipped: indices go 0-4, then 6-12.
        if (is_hebrew_leap_year(calendar)) {
            // Leap year: all 13 month slots are used, ordinal = index + 1.
            return static_cast<u8>(month_index + 1);
        }
        // Common year: index 5 is skipped, so indices >= 6 need adjustment.
        if (month_index < 6)
            return static_cast<u8>(month_index + 1);
        return static_cast<u8>(month_index); // 6 -> 6, 7 -> 7, ..., 12 -> 12
    }

    return static_cast<u8>(month_index + 1);
}

// Determine a month code for the current date in an ICU calendar.
static String compute_month_code(icu::Calendar& calendar, String const& calendar_name)
{
    UErrorCode status = U_ZERO_ERROR;

    auto month_index = calendar.get(UCAL_MONTH, status);
    if (icu_failure(status))
        return "M01"_string;

    // Check if this is a leap month (ICU provides IS_LEAP_MONTH field).
    auto is_leap_month = calendar.get(UCAL_IS_LEAP_MONTH, status);
    if (icu_failure(status))
        is_leap_month = 0;

    // For lunisolar calendars (chinese, dangi, hebrew), leap months have special handling.
    if (is_leap_month != 0) {
        // The month_index is the ordinal position. For a leap month, the "base" month number
        // is the same as the preceding regular month.
        // For Hebrew: the leap month is Adar I (inserted before Adar), which is month index 5
        // in leap years (0-based), and its month code is M05L.
        // For Chinese/Dangi: ICU gives the leap month an IS_LEAP_MONTH flag.

        if (calendar_name.is_one_of("hebrew"sv)) {
            // Hebrew leap month: Adar I has month code M05L.
            return "M05L"_string;
        }

        // Chinese/Dangi: The month code for a leap month is M##L where ## is the ordinal month
        // number in a common year. ICU's MONTH field for a leap month is the same as the
        // preceding non-leap month.
        auto month_number = static_cast<u8>(month_index + 1);
        // The ordinal month for a leap month in Chinese/Dangi is the same as the previous month.
        // ICU reports month_index as the 0-based index including the leap month position.
        // We need to find what "base" month this is a leap of. For Chinese/Dangi, the month
        // number in UCAL_MONTH for a leap month is the same as the base month.
        return MUST(String::formatted("M{:02}L", month_number));
    }

    // For non-leap months, we need the "standard" month number.
    // For calendars with 13 months (coptic, ethioaa, ethiopic): month 13 has code M13.
    // For Hebrew: months after a potential leap month have shifted indices in leap years.
    if (calendar_name == "hebrew"sv) {
        // ICU Hebrew calendar always uses 13 month slots (0-12). In common years,
        // month index 5 (Adar I) is skipped. ICU doesn't set IS_LEAP_MONTH for Hebrew,
        // so we detect Adar I by checking month_index == 5 in a leap year.
        if (month_index <= 4)
            return MUST(String::formatted("M{:02}", month_index + 1));
        if (month_index == 5 && is_hebrew_leap_year(calendar))
            return "M05L"_string;
        // month_index 6-12 -> M06-M12
        return MUST(String::formatted("M{:02}", month_index));
    }

    if (calendar_name.is_one_of("coptic"sv, "ethioaa"sv, "ethiopic"sv)) {
        // These calendars have 13 months. Month index 12 = M13.
        return MUST(String::formatted("M{:02}", month_index + 1));
    }

    // For most calendars: month_index + 1 = month number = month code number.
    return MUST(String::formatted("M{:02}", month_index + 1));
}

// Compute era and era year from the arithmetic year and ISO date.
// We derive these from the arithmetic year rather than relying on ICU's ERA/YEAR fields,
// because ICU's era numbering and year-within-era semantics differ from the spec.
struct EraAndYear {
    Optional<String> era;
    Optional<i32> era_year;
};

static EraAndYear compute_era_and_era_year(String const& calendar_name, i32 arithmetic_year, ISODate iso_date)
{
    if (!calendar_supports_era(calendar_name))
        return {};

    // Single-era calendars: eraYear = arithmeticYear.
    if (calendar_name == "buddhist"sv)
        return { "be"_string, arithmetic_year };
    if (calendar_name == "coptic"sv)
        return { "am"_string, arithmetic_year };
    if (calendar_name == "ethioaa"sv)
        return { "aa"_string, arithmetic_year };
    if (calendar_name == "hebrew"sv)
        return { "am"_string, arithmetic_year };
    if (calendar_name == "indian"sv)
        return { "shaka"_string, arithmetic_year };
    if (calendar_name == "persian"sv)
        return { "ap"_string, arithmetic_year };

    // Two-era calendars with epoch/negative pattern.
    if (calendar_name == "ethiopic"sv) {
        if (arithmetic_year >= 1)
            return { "am"_string, arithmetic_year };
        return { "aa"_string, arithmetic_year + 5500 };
    }

    if (calendar_name == "gregory"sv) {
        if (arithmetic_year >= 1)
            return { "ce"_string, arithmetic_year };
        return { "bce"_string, 1 - arithmetic_year };
    }

    if (calendar_name.is_one_of("islamic-civil"sv, "islamic-tbla"sv, "islamic-umalqura"sv)) {
        if (arithmetic_year >= 1)
            return { "ah"_string, arithmetic_year };
        return { "bh"_string, 1 - arithmetic_year };
    }

    if (calendar_name == "roc"sv) {
        if (arithmetic_year >= 1)
            return { "roc"_string, arithmetic_year };
        return { "broc"_string, 1 - arithmetic_year };
    }

    if (calendar_name == "japanese"sv) {
        // Japanese: arithmetic year = ISO year. Use the ISO date to determine regnal eras.
        if (arithmetic_year <= 0)
            return { "bce"_string, 1 - arithmetic_year };

        // Check regnal eras by ISO date (year/month/day).
        auto y = iso_date.year;
        auto m = iso_date.month;
        auto d = iso_date.day;

        if (y > 2019 || (y == 2019 && (m > 5 || (m == 5 && d >= 1))))
            return { "reiwa"_string, y - 2019 + 1 };
        if (y > 1989 || (y == 1989 && (m > 1 || (m == 1 && d >= 8))))
            return { "heisei"_string, y - 1989 + 1 };
        if (y > 1926 || (y == 1926 && (m == 12 && d >= 25)))
            return { "showa"_string, y - 1926 + 1 };
        if (y > 1912 || (y == 1912 && (m > 7 || (m == 7 && d >= 30))))
            return { "taisho"_string, y - 1912 + 1 };
        if (y >= 1873)
            return { "meiji"_string, y - 1868 + 1 };

        return { "ce"_string, arithmetic_year };
    }

    return {};
}

// Convert arithmetic year to ICU EXTENDED_YEAR, accounting for calendars where
// EXTENDED_YEAR is the Gregorian year rather than the native calendar year.
static i32 arithmetic_year_to_extended_year(String const& calendar, i32 arithmetic_year)
{
    if (calendar == "buddhist"sv)
        return arithmetic_year - 543;
    if (calendar == "roc"sv)
        return arithmetic_year + 1911;
    return arithmetic_year;
}

CalendarDate calendar_date_from_iso(String const& calendar, ISODate iso_date)
{
    auto calendar_data = CalendarData::for_calendar(calendar);
    if (!calendar_data.has_value())
        return {};

    auto& icu_calendar = calendar_data->calendar();
    set_icu_calendar_to_iso_date(icu_calendar, iso_date);

    UErrorCode status = U_ZERO_ERROR;

    auto ordinal_month = compute_ordinal_month(icu_calendar, calendar);
    auto month_code = compute_month_code(icu_calendar, calendar);

    auto day = static_cast<u8>(icu_calendar.get(UCAL_DATE, status));
    auto day_of_week_raw = icu_calendar.get(UCAL_DAY_OF_WEEK, status);
    // ICU: SUNDAY=1, MONDAY=2, ..., SATURDAY=7
    // Spec: MONDAY=1, ..., SUNDAY=7
    u8 day_of_week = (day_of_week_raw == UCAL_SUNDAY) ? 7 : static_cast<u8>(day_of_week_raw - 1);

    auto day_of_year = static_cast<u16>(icu_calendar.get(UCAL_DAY_OF_YEAR, status));

    auto days_in_month = static_cast<u8>(icu_calendar.getActualMaximum(UCAL_DATE, status));
    auto days_in_year = static_cast<u16>(icu_calendar.getActualMaximum(UCAL_DAY_OF_YEAR, status));
    u8 months_in_year;
    if (calendar.is_one_of("chinese"sv, "dangi"sv)) {
        months_in_year = chinese_year_layout(calendar, icu_calendar).month_count;
    } else if (calendar == "hebrew"sv) {
        months_in_year = is_hebrew_leap_year(icu_calendar) ? 13 : 12;
    } else {
        months_in_year = static_cast<u8>(icu_calendar.getActualMaximum(UCAL_MONTH, status) + 1);
    }

    // Determine leap year: a year with more months or days than a common year.
    bool in_leap_year = false;
    if (calendar.is_one_of("chinese"sv, "dangi"sv, "hebrew"sv)) {
        // For lunisolar calendars, a leap year has more months.
        in_leap_year = months_in_year > 12;
    } else if (calendar.is_one_of("coptic"sv, "ethioaa"sv, "ethiopic"sv)) {
        // These always have 13 months; leap year has 366 days (6 days in month 13 vs 5).
        in_leap_year = days_in_year > 365;
    } else if (calendar.starts_with_bytes("islamic"sv)) {
        // Islamic calendars have 354 days in a common year, 355 in a leap year.
        in_leap_year = days_in_year > 354;
    } else {
        // For solar calendars, leap year has more days.
        in_leap_year = days_in_year > 365;
    }

    // Compute arithmetic year per the era/monthCode spec.
    // The arithmetic year is the proleptic calendar year, with 0 at the epoch.
    i32 arithmetic_year = 0;
    if (calendar.is_one_of("chinese"sv, "dangi"sv)) {
        // Chinese/Dangi: arithmetic year = ISO year of the start of the current calendar year.
        // Chinese New Year falls in Jan-Feb, so for ISO dates before it, the arithmetic year
        // is the previous ISO year.
        auto year_start = adopt_own_if_nonnull(icu_calendar.clone());
        year_start->set(UCAL_MONTH, 0);
        year_start->set(UCAL_DATE, 1);
        UErrorCode status2 = U_ZERO_ERROR;
        auto epoch_ms = year_start->getTime(status2);
        if (!icu_failure(status2)) {
            auto& greg = proleptic_gregorian_calendar();
            greg.setTime(epoch_ms, status2);
            arithmetic_year = static_cast<i32>(greg.get(UCAL_EXTENDED_YEAR, status2));
        }
        if (icu_failure(status2))
            arithmetic_year = iso_date.year;
    } else {
        // For most calendars, EXTENDED_YEAR gives the calendar's proleptic year.
        // For Buddhist and ROC (which subclass GregorianCalendar in ICU), EXTENDED_YEAR
        // is the Gregorian year, so we need to apply the epoch offset.
        auto extended_year = icu_calendar.get(UCAL_EXTENDED_YEAR, status);
        if (icu_failure(status))
            extended_year = 0;

        // Reverse the EXTENDED_YEAR → arithmetic year mapping.
        if (calendar == "buddhist"sv)
            arithmetic_year = extended_year + 543;
        else if (calendar == "roc"sv)
            arithmetic_year = extended_year - 1911;
        else
            arithmetic_year = extended_year;
    }

    // Compute era and eraYear from the arithmetic year and ISO date.
    auto [era, era_year] = compute_era_and_era_year(calendar, arithmetic_year, iso_date);

    return CalendarDate {
        .era = move(era),
        .era_year = era_year,
        .year = arithmetic_year,
        .month = ordinal_month,
        .month_code = move(month_code),
        .day = day,
        .day_of_week = day_of_week,
        .day_of_year = day_of_year,
        .week_of_year = {},
        .days_in_week = 7,
        .days_in_month = days_in_month,
        .days_in_year = days_in_year,
        .months_in_year = months_in_year,
        .in_leap_year = in_leap_year,
    };
}

// Set an ICU calendar to the given arithmetic year. For Chinese/Dangi, this requires
// converting through an ISO date since EXTENDED_YEAR is cycle-based, not ISO-year-based.
static void set_calendar_to_arithmetic_year(icu::Calendar& calendar, String const& calendar_name, i32 arithmetic_year)
{
    if (calendar_name.is_one_of("chinese"sv, "dangi"sv)) {
        auto extended_year = chinese_arithmetic_to_extended(calendar_name, calendar, arithmetic_year);
        calendar.clear();
        calendar.set(UCAL_EXTENDED_YEAR, extended_year);
    } else {
        calendar.set(UCAL_EXTENDED_YEAR, arithmetic_year_to_extended_year(calendar_name, arithmetic_year));
    }
}

Optional<ISODate> iso_date_from_calendar(String const& calendar, i32 arithmetic_year, u8 ordinal_month, u8 day)
{
    auto calendar_data = CalendarData::for_calendar(calendar);
    if (!calendar_data.has_value())
        return {};

    auto& icu_calendar = calendar_data->calendar();
    UErrorCode status = U_ZERO_ERROR;

    icu_calendar.clear();

    // Convert arithmetic year to ICU's EXTENDED_YEAR and set the calendar.
    set_calendar_to_arithmetic_year(icu_calendar, calendar, arithmetic_year);

    if (calendar.is_one_of("chinese"sv, "dangi"sv)) {
        // Use the cached month layout to directly set month index and leap flag.
        auto const& layout = chinese_year_layout(calendar, icu_calendar);
        if (ordinal_month >= 1 && ordinal_month <= layout.month_count) {
            icu_calendar.set(UCAL_MONTH, layout.months[ordinal_month].month_index);
            icu_calendar.set(UCAL_IS_LEAP_MONTH, layout.months[ordinal_month].is_leap);
        }
        icu_calendar.set(UCAL_DATE, day);
    } else if (calendar == "hebrew"sv) {
        auto icu_month = hebrew_ordinal_to_icu_month(is_hebrew_leap_year(icu_calendar), ordinal_month);
        icu_calendar.set(UCAL_MONTH, icu_month);
        icu_calendar.set(UCAL_DATE, day);
    } else {
        icu_calendar.set(UCAL_MONTH, ordinal_month - 1); // 0-based
        icu_calendar.set(UCAL_DATE, day);
    }
    icu_calendar.set(UCAL_HOUR_OF_DAY, 12);

    auto epoch_ms = icu_calendar.getTime(status);
    if (icu_failure(status))
        return {};

    // Now convert epoch_ms to ISO date using a proleptic Gregorian calendar.
    auto& gregorian = proleptic_gregorian_calendar();
    gregorian.setTime(epoch_ms, status);
    if (icu_failure(status))
        return {};

    auto iso_year = static_cast<i32>(gregorian.get(UCAL_EXTENDED_YEAR, status));
    auto iso_month = static_cast<u8>(gregorian.get(UCAL_MONTH, status) + 1);
    auto iso_day = static_cast<u8>(gregorian.get(UCAL_DATE, status));

    if (icu_failure(status))
        return {};

    return ISODate { iso_year, iso_month, iso_day };
}

// https://tc39.es/proposal-intl-era-monthcode/#table-eras
bool calendar_supports_era(String const& calendar)
{
    // From the spec's Table of Eras: calendars that have entries in the era table.
    return calendar.is_one_of(
        "buddhist"sv,
        "coptic"sv,
        "ethioaa"sv,
        "ethiopic"sv,
        "gregory"sv,
        "hebrew"sv,
        "indian"sv,
        "islamic-civil"sv,
        "islamic-tbla"sv,
        "islamic-umalqura"sv,
        "japanese"sv,
        "persian"sv,
        "roc"sv);
}

// Look up a Chinese/Dangi year layout by extended year, without touching ICU if cached.
static ChineseYearLayout const& chinese_year_layout_by_extended(String const& calendar_name, i32 extended_year, icu::Calendar& calendar)
{
    ChineseYearKey key { calendar_name, extended_year };
    auto it = s_chinese_year_cache.find(key);
    if (it != s_chinese_year_cache.end())
        return it->value;

    // Need to set up calendar and build layout.
    calendar.clear();
    calendar.set(UCAL_EXTENDED_YEAR, extended_year);
    calendar.set(UCAL_MONTH, 0);
    calendar.set(UCAL_IS_LEAP_MONTH, 0);
    calendar.set(UCAL_DATE, 1);
    return chinese_year_layout(calendar_name, calendar);
}

u8 calendar_months_in_year(String const& calendar, i32 arithmetic_year)
{
    auto calendar_data = CalendarData::for_calendar(calendar);
    if (!calendar_data.has_value())
        return 12;

    auto& icu_calendar = calendar_data->calendar();
    UErrorCode status = U_ZERO_ERROR;

    if (calendar.is_one_of("chinese"sv, "dangi"sv)) {
        auto ext_year = chinese_arithmetic_to_extended(calendar, icu_calendar, arithmetic_year);
        return chinese_year_layout_by_extended(calendar, ext_year, icu_calendar).month_count;
    }

    icu_calendar.clear();
    set_calendar_to_arithmetic_year(icu_calendar, calendar, arithmetic_year);
    icu_calendar.set(UCAL_MONTH, 0);
    icu_calendar.set(UCAL_IS_LEAP_MONTH, 0);
    icu_calendar.set(UCAL_DATE, 1);

    if (calendar == "hebrew"sv)
        return is_hebrew_leap_year(icu_calendar) ? 13 : 12;

    return static_cast<u8>(icu_calendar.getActualMaximum(UCAL_MONTH, status) + 1);
}

u8 calendar_days_in_month(String const& calendar, i32 arithmetic_year, u8 ordinal_month)
{
    auto calendar_data = CalendarData::for_calendar(calendar);
    if (!calendar_data.has_value())
        return 30;

    auto& icu_calendar = calendar_data->calendar();
    UErrorCode status = U_ZERO_ERROR;

    if (calendar.is_one_of("chinese"sv, "dangi"sv)) {
        auto ext_year = chinese_arithmetic_to_extended(calendar, icu_calendar, arithmetic_year);
        auto const& layout = chinese_year_layout_by_extended(calendar, ext_year, icu_calendar);
        if (ordinal_month >= 1 && ordinal_month <= layout.month_count)
            return layout.months[ordinal_month].days_in_month;
        return 30;
    }

    icu_calendar.clear();
    set_calendar_to_arithmetic_year(icu_calendar, calendar, arithmetic_year);

    if (calendar == "hebrew"sv) {
        auto icu_month = hebrew_ordinal_to_icu_month(is_hebrew_leap_year(icu_calendar), ordinal_month);
        icu_calendar.set(UCAL_MONTH, icu_month);
    } else {
        icu_calendar.set(UCAL_MONTH, ordinal_month - 1);
    }

    icu_calendar.set(UCAL_DATE, 1);

    return static_cast<u8>(icu_calendar.getActualMaximum(UCAL_DATE, status));
}

Optional<OrdinalMonthInfo> chinese_ordinal_month_info(String const& calendar, i32 arithmetic_year, u8 ordinal_month)
{
    ASSERT(calendar.is_one_of("chinese"sv, "dangi"sv));

    auto calendar_data = CalendarData::for_calendar(calendar);
    if (!calendar_data.has_value())
        return {};

    auto& icu_calendar = calendar_data->calendar();

    auto ext_year = chinese_arithmetic_to_extended(calendar, icu_calendar, arithmetic_year);
    auto const& layout = chinese_year_layout_by_extended(calendar, ext_year, icu_calendar);

    if (ordinal_month < 1 || ordinal_month > layout.month_count)
        return {};

    auto const& entry = layout.months[ordinal_month];
    // month_index is the 0-based ICU month index. For Chinese, the month number is month_index + 1.
    return OrdinalMonthInfo { static_cast<u8>(entry.month_index + 1), entry.is_leap != 0 };
}

}
