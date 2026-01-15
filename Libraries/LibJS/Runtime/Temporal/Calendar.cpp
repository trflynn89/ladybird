/*
 * Copyright (c) 2021, Idan Horowitz <idan.horowitz@serenityos.org>
 * Copyright (c) 2021-2023, Linus Groh <linusg@serenityos.org>
 * Copyright (c) 2023-2024, Shannon Booth <shannon@serenityos.org>
 * Copyright (c) 2024-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/NonnullRawPtr.h>
#include <AK/QuickSort.h>
#include <LibJS/Runtime/AbstractOperations.h>
#include <LibJS/Runtime/Temporal/Calendar.h>
#include <LibJS/Runtime/Temporal/DateEquations.h>
#include <LibJS/Runtime/Temporal/Duration.h>
#include <LibJS/Runtime/Temporal/ISO8601.h>
#include <LibJS/Runtime/Temporal/PlainDate.h>
#include <LibJS/Runtime/Temporal/PlainDateTime.h>
#include <LibJS/Runtime/Temporal/PlainMonthDay.h>
#include <LibJS/Runtime/Temporal/PlainYearMonth.h>
#include <LibJS/Runtime/Temporal/TimeZone.h>
#include <LibJS/Runtime/Temporal/ZonedDateTime.h>
#include <LibJS/Runtime/VM.h>
#include <LibUnicode/Calendar.h>
#include <LibUnicode/Locale.h>
#include <LibUnicode/UnicodeKeywords.h>

namespace JS::Temporal {

enum class CalendarFieldConversion {
    ToIntegerWithTruncation,
    ToMonthCode,
    ToOffsetString,
    ToPositiveIntegerWithTruncation,
    ToString,
    ToTemporalTimeZoneIdentifier,
};

// https://tc39.es/proposal-temporal/#table-temporal-calendar-fields-record-fields
#define JS_ENUMERATE_CALENDAR_FIELDS                                                                                                \
    __JS_ENUMERATE(CalendarField::Era, era, vm.names.era, CalendarFieldConversion::ToString)                                        \
    __JS_ENUMERATE(CalendarField::EraYear, era_year, vm.names.eraYear, CalendarFieldConversion::ToIntegerWithTruncation)            \
    __JS_ENUMERATE(CalendarField::Year, year, vm.names.year, CalendarFieldConversion::ToIntegerWithTruncation)                      \
    __JS_ENUMERATE(CalendarField::Month, month, vm.names.month, CalendarFieldConversion::ToPositiveIntegerWithTruncation)           \
    __JS_ENUMERATE(CalendarField::MonthCode, month_code, vm.names.monthCode, CalendarFieldConversion::ToMonthCode)                  \
    __JS_ENUMERATE(CalendarField::Day, day, vm.names.day, CalendarFieldConversion::ToPositiveIntegerWithTruncation)                 \
    __JS_ENUMERATE(CalendarField::Hour, hour, vm.names.hour, CalendarFieldConversion::ToIntegerWithTruncation)                      \
    __JS_ENUMERATE(CalendarField::Minute, minute, vm.names.minute, CalendarFieldConversion::ToIntegerWithTruncation)                \
    __JS_ENUMERATE(CalendarField::Second, second, vm.names.second, CalendarFieldConversion::ToIntegerWithTruncation)                \
    __JS_ENUMERATE(CalendarField::Millisecond, millisecond, vm.names.millisecond, CalendarFieldConversion::ToIntegerWithTruncation) \
    __JS_ENUMERATE(CalendarField::Microsecond, microsecond, vm.names.microsecond, CalendarFieldConversion::ToIntegerWithTruncation) \
    __JS_ENUMERATE(CalendarField::Nanosecond, nanosecond, vm.names.nanosecond, CalendarFieldConversion::ToIntegerWithTruncation)    \
    __JS_ENUMERATE(CalendarField::Offset, offset_string, vm.names.offset, CalendarFieldConversion::ToOffsetString)                  \
    __JS_ENUMERATE(CalendarField::TimeZone, time_zone, vm.names.timeZone, CalendarFieldConversion::ToTemporalTimeZoneIdentifier)

struct CalendarFieldData {
    CalendarField key;
    NonnullRawPtr<PropertyKey> property;
    CalendarFieldConversion conversion;
};
static Vector<CalendarFieldData> sorted_calendar_fields(VM& vm, CalendarFieldList fields)
{
    auto data_for_field = [&](auto field) -> CalendarFieldData {
        switch (field) {
#define __JS_ENUMERATE(enumeration, field_name, property_key, conversion) \
    case enumeration:                                                     \
        return { enumeration, property_key, conversion };
            JS_ENUMERATE_CALENDAR_FIELDS
#undef __JS_ENUMERATE
        }

        VERIFY_NOT_REACHED();
    };

    Vector<CalendarFieldData> result;
    result.ensure_capacity(fields.size());

    for (auto field : fields)
        result.unchecked_append(data_for_field(field));

    quick_sort(result, [](auto const& lhs, auto const& rhs) {
        return lhs.property->as_string() < rhs.property->as_string();
    });

    return result;
}

template<typename T>
static void set_field_value(CalendarField field, CalendarFields& fields, T&& value)
{
    switch (field) {
#define __JS_ENUMERATE(enumeration, field_name, property_key, conversion)              \
    case enumeration:                                                                  \
        if constexpr (IsAssignable<decltype(fields.field_name), RemoveCVReference<T>>) \
            fields.field_name = value;                                                 \
        return;
        JS_ENUMERATE_CALENDAR_FIELDS
#undef __JS_ENUMERATE
    }

    VERIFY_NOT_REACHED();
}

static void set_default_field_value(CalendarField field, CalendarFields& fields)
{
    CalendarFields default_ {};

    switch (field) {
#define __JS_ENUMERATE(enumeration, field_name, property_key, conversion) \
    case enumeration:                                                     \
        fields.field_name = default_.field_name;                          \
        return;
        JS_ENUMERATE_CALENDAR_FIELDS
#undef __JS_ENUMERATE
    }

    VERIFY_NOT_REACHED();
}

[[maybe_unused]] static constexpr Unicode::ISODate to_unicode_iso_date(ISODate iso_date)
{
    return { .year = iso_date.year, .month = static_cast<u8>(iso_date.month - 1), .day = iso_date.day };
}

[[maybe_unused]] static constexpr ISODate from_unicode_iso_date(Unicode::ISODate iso_date)
{
    return { .year = iso_date.year, .month = static_cast<u8>(iso_date.month + 1), .day = iso_date.day };
}

[[maybe_unused]] static constexpr Unicode::DateDuration to_unicode_date_duration(DateDuration const& duration)
{
    return { .years = duration.years, .months = duration.months, .weeks = duration.weeks, .days = duration.days };
}

[[maybe_unused]] static constexpr DateDuration from_unicode_date_duration(Unicode::DateDuration const& duration)
{
    return { .years = duration.years, .months = duration.months, .weeks = duration.weeks, .days = duration.days };
}

// Table 1: Calendar types described in CLDR, https://tc39.es/proposal-intl-era-monthcode/#table-calendar-types
static constexpr auto CLDR_CALENDAR_TYPES = to_array<StringView>({
    "buddhist"sv,
    "chinese"sv,
    "coptic"sv,
    "dangi"sv,
    "ethioaa"sv,
    "ethiopic"sv,
    "ethiopic-amete-alem"sv,
    "gregory"sv,
    "hebrew"sv,
    "indian"sv,
    "islamic-civil"sv,
    "islamic-tbla"sv,
    "islamic-umalqura"sv,
    "islamicc"sv,
    "iso8601"sv,
    "japanese"sv,
    "persian"sv,
    "roc"sv,
});

// Table 2: Eras, https://tc39.es/proposal-intl-era-monthcode/#table-eras
struct CalendarEraData {
    enum class Kind {
        Epoch,
        Offset,
        Negative,
    };

    StringView calendar;
    StringView era;
    Vector<StringView> alias;
    Optional<i32> minimumEraYear;
    Optional<i32> maximumEraYear;
    Optional<Kind> kind;
    Optional<i32> offset;
};
static auto CALENDAR_ERA_DATA = to_array<CalendarEraData>({
    // clang-format off
    { "buddhist"sv,         "be"sv,     {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "coptic"sv,           "am"sv,     {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "ethioaa"sv,          "aa"sv,     {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "ethiopic"sv,         "am"sv,     {},         1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "ethiopic"sv,         "aa"sv,     {},         {}, 5500, CalendarEraData::Kind::Offset,   -5499 },
    { "gregory"sv,          "ce"sv,     { "ad"sv }, 1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "gregory"sv,          "bce"sv,    { "bc"sv }, 1,  {},   CalendarEraData::Kind::Negative, {}    },
    { "hebrew"sv,           "am"sv,     {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "indian"sv,           "shaka"sv,  {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "islamic-civil"sv,    "ah"sv,     {},         1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "islamic-civil"sv,    "bh"sv,     {},         1,  {},   CalendarEraData::Kind::Negative, {}    },
    { "islamic-tbla"sv,     "ah"sv,     {},         1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "islamic-tbla"sv,     "bh"sv,     {},         1,  {},   CalendarEraData::Kind::Negative, {}    },
    { "islamic-umalqura"sv, "ah"sv,     {},         1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "islamic-umalqura"sv, "bh"sv,     {},         1,  {},   CalendarEraData::Kind::Negative, {}    },
    { "japanese"sv,         "reiwa"sv,  {},         1,  {},   CalendarEraData::Kind::Offset,   2019  },
    { "japanese"sv,         "heisei"sv, {},         1,  31,   CalendarEraData::Kind::Offset,   1989  },
    { "japanese"sv,         "showa"sv,  {},         1,  64,   CalendarEraData::Kind::Offset,   1926  },
    { "japanese"sv,         "taisho"sv, {},         1,  15,   CalendarEraData::Kind::Offset,   1912  },
    { "japanese"sv,         "meiji"sv,  {},         1,  45,   CalendarEraData::Kind::Offset,   1868  },
    { "japanese"sv,         "ce"sv,     { "ad"sv }, 1,  1868, CalendarEraData::Kind::Epoch,    {}    },
    { "japanese"sv,         "bce"sv,    { "bc"sv }, 1,  {},   CalendarEraData::Kind::Negative, {}    },
    { "persian"sv,          "ap"sv,     {},         {}, {},   CalendarEraData::Kind::Epoch,    {}    },
    { "roc"sv,              "roc"sv,    {},         1,  {},   CalendarEraData::Kind::Epoch,    {}    },
    { "roc"sv,              "broc"sv,   {},         1,  {},   CalendarEraData::Kind::Negative, {}    },
    // clang-format on
});

// Table 3: Additional Month Codes in Calendars, https://tc39.es/proposal-intl-era-monthcode/#table-additional-month-codes
struct AdditionalMonthCodes {
    enum class Leap {
        SkipBackward,
        SkipForward,
    };

    StringView calendar;
    Vector<StringView> additional_month_codes;
    Optional<Leap> leap_to_common_month_transformation;
};
static auto ADDITIONAL_MONTH_CODES = to_array<AdditionalMonthCodes>({
    { "chinese"sv, { "M01L"sv, "M02L"sv, "M03L"sv, "M04L"sv, "M05L"sv, "M06L"sv, "M07L"sv, "M08L"sv, "M09L"sv, "M10L"sv, "M11L"sv, "M12L"sv }, AdditionalMonthCodes::Leap::SkipBackward },
    { "coptic"sv, { "M13"sv }, {} },
    { "dangi"sv, { "M01L"sv, "M02L"sv, "M03L"sv, "M04L"sv, "M05L"sv, "M06L"sv, "M07L"sv, "M08L"sv, "M09L"sv, "M10L"sv, "M11L"sv, "M12L"sv }, AdditionalMonthCodes::Leap::SkipBackward },
    { "ethioaa"sv, { "M13"sv }, {} },
    { "ethiopic"sv, { "M13"sv }, {} },
    { "hebrew"sv, { "M05L"sv }, AdditionalMonthCodes::Leap::SkipForward },
});

// Table 4: Epoch years, https://tc39.es/proposal-intl-era-monthcode/#table-epoch-years
// struct EpochYears {
//     StringView calendar;
//     i32 epoch_year { 0 };
// };
// static auto EPOCH_YEARS = to_array<EpochYears>({
//     { "buddhist"sv, -543 },
//     { "chinese"sv, 0 },
//     { "coptic"sv, 283 },
//     { "dangi"sv, 0 },
//     { "ethioaa"sv, -5493 },
//     { "ethiopic"sv, 7 },
//     { "gregory"sv, 0 },
//     { "hebrew"sv, -3761 },
//     { "indian"sv, 78 },
//     { "islamic-civil"sv, 621 },
//     { "islamic-tbla"sv, 621 },
//     { "islamic-umalqura"sv, 621 },
//     { "japanese"sv, 0 },
//     { "persian"sv, 621 },
//     { "roc"sv, 1911 },
// });

static constexpr bool is_lunisolar_calendar(StringView calendar)
{
    return calendar.is_one_of("chinese", "dangi", "hebrew"sv);
}

// static constexpr u8 months_per_year_in_non_lunisolar_calendar(StringView calendar)
// {
//     return 12u + calendar.is_one_of("coptic"sv, "ethioaa"sv, "ethiopic"sv);
// }

// 12.1.1 CanonicalizeCalendar ( id ), https://tc39.es/proposal-temporal/#sec-temporal-canonicalizecalendar
ThrowCompletionOr<String> canonicalize_calendar(VM& vm, StringView id)
{
    // 1. Let calendars be AvailableCalendars().
    auto const& calendars = available_calendars();

    // 2. If calendars does not contain the ASCII-lowercase of id, throw a RangeError exception.
    for (auto const& calendar : calendars) {
        if (calendar.equals_ignoring_ascii_case(id)) {
            // 3. Return CanonicalizeUValue("ca", id).
            return Unicode::canonicalize_unicode_extension_values("ca"sv, id);
        }
    }

    return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarIdentifier, id);
}

// 12.1.2 AvailableCalendars ( ), https://tc39.es/proposal-temporal/#sec-availablecalendars
Vector<String> const& available_calendars()
{
    // The implementation-defined abstract operation AvailableCalendars takes no arguments and returns a List of calendar
    // types. The returned List is sorted according to lexicographic code unit order, and contains unique calendar types
    // in canonical form (12.1) identifying the calendars for which the implementation provides the functionality of
    // Intl.DateTimeFormat objects, including their aliases (e.g., either both or neither of "islamicc" and
    // "islamic-civil"). The List must include "iso8601".
    return Unicode::available_calendars();
}

// https://tc39.es/proposal-temporal/#prod-MonthCode
static constexpr bool is_valid_month_code_string(StringView month_code)
{
    // MonthCode :::
    //     M00L
    //     M0 NonZeroDigit L[opt]
    //     M NonZeroDigit DecimalDigit L[opt]
    auto length = month_code.length();

    if (length != 3 && length != 4)
        return false;

    if (month_code[0] != 'M')
        return false;

    if (!is_ascii_digit(month_code[1]) || !is_ascii_digit(month_code[2]))
        return false;

    if (length == 3 && month_code[1] == '0' && month_code[2] == '0')
        return false;
    if (length == 4 && month_code[3] != 'L')
        return false;

    return true;
}

// 12.2.1 ParseMonthCode ( argument ), https://tc39.es/proposal-temporal/#sec-temporal-parsemonthcode
ThrowCompletionOr<MonthCode> parse_month_code(VM& vm, Value argument)
{
    // 1. Let monthCode be ? ToPrimitive(argument, STRING).
    auto month_code = TRY(argument.to_primitive(vm, Value::PreferredType::String));

    // 2. If monthCode is not a String, throw a TypeError exception.
    if (!month_code.is_string())
        return vm.throw_completion<TypeError>(ErrorType::NotAString, month_code);

    return parse_month_code(vm, month_code.as_string().utf8_string_view());
}

// 12.2.1 ParseMonthCode ( argument ), https://tc39.es/proposal-temporal/#sec-temporal-parsemonthcode
ThrowCompletionOr<MonthCode> parse_month_code(VM& vm, StringView month_code)
{
    // 3. If ParseText(StringToCodePoints(monthCode), MonthCode) is a List of errors, throw a RangeError exception.
    if (!is_valid_month_code_string(month_code))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidMonthCode);

    // 4. Let isLeapMonth be false.
    auto is_leap_month = false;

    // 5. If the length of monthCode is 4, then
    if (month_code.length() == 4) {
        // a. Assert: The fourth code unit of monthCode is 0x004C (LATIN CAPITAL LETTER L).
        VERIFY(month_code[3] == 'L');

        // b. Set isLeapMonth to true.
        is_leap_month = true;
    }

    // 6. Let monthCodeDigits be the substring of monthCode from 1 to 3.
    auto month_code_digits = month_code.substring_view(1, 2);

    // 7. Let monthNumber be ℝ(StringToNumber(monthCodeDigits)).
    auto month_number = month_code_digits.to_number<u8>().value();

    // 8. Return the Record { [[MonthNumber]]: monthNumber, [[IsLeapMonth]]: isLeapMonth }.
    return MonthCode { month_number, is_leap_month };
}

// 12.2.2 CreateMonthCode ( monthNumber, isLeapMonth ), https://tc39.es/proposal-temporal/#sec-temporal-createmonthcode
String create_month_code(u8 month_number, bool is_leap_month)
{
    // 1. Assert: If isLeapMonth is false, monthNumber > 0.
    if (!is_leap_month)
        VERIFY(month_number > 0);

    // 2. Let numberPart be ToZeroPaddedDecimalString(monthNumber, 2).

    // 3. If isLeapMonth is true, then
    if (is_leap_month) {
        // a. Return the string-concatenation of the code unit 0x004D (LATIN CAPITAL LETTER M), numberPart, and the
        //    code unit 0x004C (LATIN CAPITAL LETTER L).
        return MUST(String::formatted("M{:02}L", month_number));
    }

    // 4. Else,
    //     a. Return the string-concatenation of the code unit 0x004D (LATIN CAPITAL LETTER M) and numberPart.
    return MUST(String::formatted("M{:02}", month_number));
}

// 12.3.3 PrepareCalendarFields ( calendar, fields, calendarFieldNames, nonCalendarFieldNames, requiredFieldNames ), https://tc39.es/proposal-temporal/#sec-temporal-preparecalendarfields
ThrowCompletionOr<CalendarFields> prepare_calendar_fields(VM& vm, StringView calendar, Object const& fields, CalendarFieldList calendar_field_names, CalendarFieldList non_calendar_field_names, CalendarFieldListOrPartial required_field_names)
{
    // 1. Assert: If requiredFieldNames is a List, requiredFieldNames contains zero or one of each of the elements of
    //    calendarFieldNames and nonCalendarFieldNames.

    // 2. Let fieldNames be the list-concatenation of calendarFieldNames and nonCalendarFieldNames.
    Vector<CalendarField> field_names;
    field_names.append(calendar_field_names.data(), calendar_field_names.size());
    field_names.append(non_calendar_field_names.data(), non_calendar_field_names.size());

    // 3. Let extraFieldNames be CalendarExtraFields(calendar, calendarFieldNames).
    auto extra_field_names = calendar_extra_fields(calendar, calendar_field_names);

    // 4. Set fieldNames to the list-concatenation of fieldNames and extraFieldNames.
    field_names.extend(move(extra_field_names));

    // 5. Assert: fieldNames contains no duplicate elements.

    // 6. Let result be a Calendar Fields Record with all fields equal to UNSET.
    auto result = CalendarFields::unset();

    // 7. Let any be false.
    auto any = false;

    // 8. Let sortedPropertyNames be a List whose elements are the values in the Property Key column of Table 19
    //    corresponding to the elements of fieldNames, sorted according to lexicographic code unit order.
    auto sorted_property_names = sorted_calendar_fields(vm, field_names);

    // 9. For each property name property of sortedPropertyNames, do
    for (auto const& [key, property, conversion] : sorted_property_names) {
        // a. Let key be the value in the Enumeration Key column of Table 19 corresponding to the row whose Property Key value is property.

        // b. Let value be ? Get(fields, property).
        auto value = TRY(fields.get(property));

        // c. If value is not undefined, then
        if (!value.is_undefined()) {
            // i. Set any to true.
            any = true;

            // ii. Let Conversion be the Conversion value of the same row.
            switch (conversion) {
            // iii. If Conversion is TO-INTEGER-WITH-TRUNCATION, then
            case CalendarFieldConversion::ToIntegerWithTruncation:
                // 1. Set value to ? ToIntegerWithTruncation(value).
                // 2. Set value to 𝔽(value).
                set_field_value(key, result, TRY(to_integer_with_truncation(vm, value, ErrorType::TemporalInvalidCalendarFieldName, *property)));
                break;
            // iv. Else if Conversion is TO-POSITIVE-INTEGER-WITH-TRUNCATION, then
            case CalendarFieldConversion::ToPositiveIntegerWithTruncation:
                // 1. Set value to ? ToPositiveIntegerWithTruncation(value).
                // 2. Set value to 𝔽(value).
                set_field_value(key, result, TRY(to_positive_integer_with_truncation(vm, value, ErrorType::TemporalInvalidCalendarFieldName, *property)));
                break;
            // v. Else if Conversion is TO-STRING, then
            case CalendarFieldConversion::ToString:
                // 1. Set value to ? ToString(value).
                set_field_value(key, result, TRY(value.to_string(vm)));
                break;
            // vi. Else if Conversion is TO-TEMPORAL-TIME-ZONE-IDENTIFIER, then
            case CalendarFieldConversion::ToTemporalTimeZoneIdentifier:
                // 1. Set value to ? ToTemporalTimeZoneIdentifier(value).
                set_field_value(key, result, TRY(to_temporal_time_zone_identifier(vm, value)));
                break;
            // vii. Else if Conversion is TO-MONTH-CODE, then
            case CalendarFieldConversion::ToMonthCode: {
                // 1. Let parsed be ? ParseMonthCode(value).
                auto parsed = TRY(parse_month_code(vm, value));

                // 2. Set value to CreateMonthCode(parsed.[[MonthNumber]], parsed.[[IsLeapMonth]]).
                set_field_value(key, result, create_month_code(parsed.month_number, parsed.is_leap_month));

                break;
            }
            // viii. Else,
            case CalendarFieldConversion::ToOffsetString:
                // 1. Assert: Conversion is TO-OFFSET-STRING.
                // 2. Set value to ? ToOffsetString(value).
                set_field_value(key, result, TRY(to_offset_string(vm, value)));
                break;
            }

            // ix. Set result's field whose name is given in the Field Name column of the same row to value.
        }
        // d. Else if requiredFieldNames is a List, then
        else if (auto const* required = required_field_names.get_pointer<CalendarFieldList>()) {
            // i. If requiredFieldNames contains key, then
            if (required->contains_slow(key)) {
                // 1. Throw a TypeError exception.
                return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, *property);
            }

            // ii. Set result's field whose name is given in the Field Name column of the same row to the corresponding
            //     Default value of the same row.
            set_default_field_value(key, result);
        }
    }

    // 10. If requiredFieldNames is PARTIAL and any is false, then
    if (required_field_names.has<Partial>() && !any) {
        // a. Throw a TypeError exception.
        return vm.throw_completion<TypeError>(ErrorType::TemporalObjectMustBePartialTemporalObject);
    }

    // 11. Return result.
    return result;
}

// 12.3.4 CalendarFieldKeysPresent ( fields ), https://tc39.es/proposal-temporal/#sec-temporal-calendarfieldkeyspresent
Vector<CalendarField> calendar_field_keys_present(CalendarFields const& fields)
{
    // 1. Let list be « ».
    Vector<CalendarField> list;

    auto handle_field = [&](auto enumeration_key, auto const& value) {
        // a. Let value be fields' field whose name is given in the Field Name column of the row.
        // b. Let enumerationKey be the value in the Enumeration Key column of the row.
        // c. If value is not unset, append enumerationKey to list.
        if (value.has_value())
            list.append(enumeration_key);
    };

    // 2. For each row of Table 19, except the header row, do
#define __JS_ENUMERATE(enumeration, field_name, property_key, conversion) \
    handle_field(enumeration, fields.field_name);
    JS_ENUMERATE_CALENDAR_FIELDS
#undef __JS_ENUMERATE

    // 3. Return list.
    return list;
}

// 12.3.5 CalendarMergeFields ( calendar, fields, additionalFields ), https://tc39.es/proposal-temporal/#sec-temporal-calendarmergefields
CalendarFields calendar_merge_fields(StringView calendar, CalendarFields const& fields, CalendarFields const& additional_fields)
{
    // 1. Let additionalKeys be CalendarFieldKeysPresent(additionalFields).
    auto additional_keys = calendar_field_keys_present(additional_fields);

    // 2. Let overriddenKeys be CalendarFieldKeysToIgnore(calendar, additionalKeys).
    auto overridden_keys = calendar_field_keys_to_ignore(calendar, additional_keys);

    // 3. Let merged be a Calendar Fields Record with all fields set to unset.
    auto merged = CalendarFields::unset();

    // 4. Let fieldsKeys be CalendarFieldKeysPresent(fields).
    auto fields_keys = calendar_field_keys_present(fields);

    auto merge_field = [&](auto key, auto& merged_field, auto const& fields_field, auto const& additional_fields_field) {
        // a. Let key be the value in the Enumeration Key column of the row.

        // b. If fieldsKeys contains key and overriddenKeys does not contain key, then
        if (fields_keys.contains_slow(key) && !overridden_keys.contains_slow(key)) {
            // i. Let propValue be fields' field whose name is given in the Field Name column of the row.
            // ii. Set merged's field whose name is given in the Field Name column of the row to propValue.
            merged_field = fields_field;
        }

        // c. If additionalKeys contains key, then
        if (additional_keys.contains_slow(key)) {
            // i. Let propValue be additionalFields' field whose name is given in the Field Name column of the row.
            // ii. Set merged's field whose name is given in the Field Name column of the row to propValue.
            merged_field = additional_fields_field;
        }
    };

    // 5. For each row of Table 19, except the header row, do
#define __JS_ENUMERATE(enumeration, field_name, property_key, conversion) \
    merge_field(enumeration, merged.field_name, fields.field_name, additional_fields.field_name);
    JS_ENUMERATE_CALENDAR_FIELDS
#undef __JS_ENUMERATE

    // 6. Return merged.
    return merged;
}

// 12.3.6 NonISODateAdd ( calendar, isoDate, duration, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-nonisodateadd
// 4.1.18 NonISODateAdd ( calendar, isoDate, duration, overflow ), https://tc39.es/proposal-intl-era-monthcode/#sup-temporal-nonisodateadd
ThrowCompletionOr<ISODate> non_iso_date_add(VM& vm, StringView calendar, ISODate iso_date, DateDuration const& duration, Overflow overflow)
{
    // 1. Let parts be CalendarISOToDate(calendar, isoDate).
    auto parts = calendar_iso_to_date(calendar, iso_date);

    // 2. Let y0 be parts.[[Year]] + duration.[[Years]].
    auto y0 = parts.year + duration.years;

    // 3. Let m0 be MonthCodeToOrdinal(calendar, y0, ? ConstrainMonthCode(calendar, y0, parts.[[MonthCode]], overflow)).
    auto m0 = month_code_to_ordinal(vm, calendar, y0, TRY(constrain_month_code(vm, calendar, y0, parts.month_code, overflow)));

    // 4. Let endOfMonth be BalanceNonISODate(calendar, y0, m0 + duration.[[Months]] + 1, 0).
    auto end_of_month = balance_non_iso_date(calendar, y0, m0 + duration.months + 1, 0);

    // 5. Let baseDay be parts.[[Day]].
    auto base_day = parts.day;

    u8 regulated_day = 0;

    // 6. If baseDay < endOfMonth.[[Day]], then
    if (base_day < end_of_month.day) {
        // a. Let regulatedDay be baseDay.
        regulated_day = base_day;
    }
    // 7. Else,
    else {
        // a. If overflow is REJECT, throw a RangeError exception.
        if (overflow == Overflow::Reject)
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

        // b. Let regulatedDay be endOfMonth.[[Day]].
        regulated_day = end_of_month.day;
    }

    // 8. Let balancedDate be BalanceNonISODate(calendar, endOfMonth.[[Year]], endOfMonth.[[Month]], regulatedDay + 7 * duration.[[Weeks]] + duration.[[Days]]).
    auto balanced_date = balance_non_iso_date(calendar, end_of_month.year, end_of_month.month, regulated_day + (7 * duration.weeks) + duration.days);

    // 9. Let result be ? CalendarIntegersToISO(calendar, balancedDate.[[Year]], balancedDate.[[Month]], balancedDate.[[Day]]).
    auto result = TRY(calendar_integers_to_iso(vm, calendar, balanced_date.year, balanced_date.month, balanced_date.day));

    // 10. If ISODateWithinLimits(result) is false, throw a RangeError exception.
    if (!iso_date_within_limits(result))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 11. Return result.
    return result;
}

// 12.3.7 CalendarDateAdd ( calendar, isoDate, duration, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendardateadd
ThrowCompletionOr<ISODate> calendar_date_add(VM& vm, StringView calendar, ISODate iso_date, DateDuration const& duration, Overflow overflow)
{
    ISODate result;

    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. Let intermediate be BalanceISOYearMonth(isoDate.[[Year]] + duration.[[Years]], isoDate.[[Month]] + duration.[[Months]]).
        auto intermediate = balance_iso_year_month(static_cast<double>(iso_date.year) + duration.years, static_cast<double>(iso_date.month) + duration.months);

        // b. Set intermediate to ? RegulateISODate(intermediate.[[Year]], intermediate.[[Month]], isoDate.[[Day]], overflow).
        auto intermediate_date = TRY(regulate_iso_date(vm, intermediate.year, intermediate.month, iso_date.day, overflow));

        // c. Let days be duration.[[Days]] + 7 × duration.[[Weeks]].
        auto days = duration.days + (7 * duration.weeks);

        // d. Let result be AddDaysToISODate(intermediate, days).
        result = add_days_to_iso_date(intermediate_date, days);
    }
    // 2. Else,
    else {
        // a. Let result be ? NonISODateAdd(calendar, isoDate, duration, overflow).
        return non_iso_date_add(vm, calendar, iso_date, duration, overflow);
    }

    // 3. If ISODateWithinLimits(result) is false, throw a RangeError exception.
    if (!iso_date_within_limits(result))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 4. Return result.
    return result;
}

// 12.3.8 NonISODateUntil ( calendar, one, two, largestUnit ), https://tc39.es/proposal-temporal/#sec-temporal-nonisodateuntil
// 4.1.19 NonISODateUntil ( calendar, one, two, largestUnit ), https://tc39.es/proposal-intl-era-monthcode/#sup-temporal-nonisodateuntil
DateDuration non_iso_date_until(VM&, StringView calendar, ISODate one, ISODate two, Unit largest_unit)
{
    // 1. Let sign be -1 × CompareISODate(one, two).
    // 2. If sign = 0, return ZeroDateDuration().
    // 3. Let years be 0.
    // 4. If largestUnit is year, then
    //     a. Let candidateYears be sign.
    //     b. Repeat, while NonISODateSurpasses(calendar, sign, one, two, candidateYears, 0, 0, 0) is false,
    //         i. Set years to candidateYears.
    //         ii. Set candidateYears to candidateYears + sign.
    // 5. Let months be 0.
    // 6. If largestUnit is year or largestUnit is month, then
    //     a. Let candidateMonths be sign.
    //     b. Repeat, while NonISODateSurpasses(calendar, sign, one, two, years, candidateMonths, 0, 0) is false,
    //         i. Set months to candidateMonths.
    //         ii. Set candidateMonths to candidateMonths + sign.
    // 7. Let weeks be 0.
    // 8. If largestUnit is week, then
    //     a. Let candidateWeeks be sign.
    //     b. Repeat, while NonISODateSurpasses(calendar, sign, one, two, years, months, candidateWeeks, 0) is false,
    //         i. Set weeks to candidateWeeks.
    //         ii. Set candidateWeeks to candidateWeeks + sign.
    // 9. Let days be 0.
    // 10. Let candidateDays be sign.
    // 11. Repeat, while NonISODateSurpasses(calendar, sign, one, two, years, months, weeks, candidateDays) is false,
    //     a. Set days to candidateDays.
    //     b. Set candidateDays to candidateDays + sign.
    // 12. Return ! CreateDateDurationRecord(years, months, weeks, days).
    auto result = Unicode::calendar_until(calendar, to_unicode_iso_date(one), to_unicode_iso_date(two), static_cast<Unicode::Unit>(largest_unit));
    return from_unicode_date_duration(result);
}

// 12.3.9 CalendarDateUntil ( calendar, one, two, largestUnit ), https://tc39.es/proposal-temporal/#sec-temporal-calendardateuntil
DateDuration calendar_date_until(VM& vm, StringView calendar, ISODate one, ISODate two, Unit largest_unit)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. Let sign be -CompareISODate(one, two).
        auto sign = compare_iso_date(one, two);
        sign *= -1;

        // b. If sign = 0, return ZeroDateDuration().
        if (sign == 0)
            return zero_date_duration(vm);

        // c. Let years be 0.
        double years = 0;

        // e. Let months be 0.
        double months = 0;

        // OPTIMIZATION: If the largestUnit is MONTH, we want to skip ahead to the correct year. If implemented in exact
        //               accordance with the spec, we could enter the second ISODateSurpasses loop below with a very large
        //               number of months to traverse.

        // d. If largestUnit is YEAR, then
        // f. If largestUnit is YEAR or largestUnit is MONTH, then
        if (largest_unit == Unit::Year || largest_unit == Unit::Month) {
            // d.i. Let candidateYears be sign.
            auto candidate_years = two.year - one.year;
            if (candidate_years != 0)
                candidate_years -= sign;

            // d.ii. Repeat, while ISODateSurpasses(sign, one, two, candidateYears, 0, 0, 0) is false,
            while (!iso_date_surpasses(vm, sign, one, two, candidate_years, 0, 0, 0)) {
                // 1. Set years to candidateYears.
                years = candidate_years;

                // 2. Set candidateYears to candidateYears + sign.
                candidate_years += sign;
            }

            // f.i. Let candidateMonths be sign.
            double candidate_months = sign;

            // f.ii. Repeat, while ISODateSurpasses(sign, one, two, years, candidateMonths, 0, 0) is false,
            while (!iso_date_surpasses(vm, sign, one, two, years, candidate_months, 0, 0)) {
                // 1. Set months to candidateMonths.
                months = candidate_months;

                // 2. Set candidateMonths to candidateMonths + sign.
                candidate_months += sign;
            }

            if (largest_unit == Unit::Month) {
                months += years * 12.0;
                years = 0.0;
            }
        }

        // g. Let weeks be 0.
        double weeks = 0;

        // OPTIMIZATION: If the largestUnit is DAY, we do not want to enter an ISODateSurpasses loop. The loop would have
        //               us increment the intermediate ISOYearMonth one day at time, which will take an extremely long
        //               time if the difference is a large number of years. Instead, we can compute the day difference,
        //               and convert to weeks if needed.
        auto year_month = balance_iso_year_month(static_cast<double>(one.year) + years, static_cast<double>(one.month) + months);
        auto regulated_date = MUST(regulate_iso_date(vm, year_month.year, year_month.month, one.day, Overflow::Constrain));

        auto days = iso_date_to_epoch_days(two.year, two.month - 1, two.day) - iso_date_to_epoch_days(regulated_date.year, regulated_date.month - 1, regulated_date.day);

        if (largest_unit == Unit::Week) {
            weeks = trunc(days / 7.0);
            days = fmod(days, 7.0);
        }

        // l. Return ! CreateDateDurationRecord(years, months, weeks, days).
        return MUST(create_date_duration_record(vm, years, months, weeks, days));
    }

    // 2. Return NonISODateUntil(calendar, one, two, largestUnit).
    return non_iso_date_until(vm, calendar, one, two, largest_unit);
}

// 12.3.10 ToTemporalCalendarIdentifier ( temporalCalendarLike ), https://tc39.es/proposal-temporal/#sec-temporal-totemporalcalendaridentifier
ThrowCompletionOr<String> to_temporal_calendar_identifier(VM& vm, Value temporal_calendar_like)
{
    // 1. If temporalCalendarLike is an Object, then
    if (temporal_calendar_like.is_object()) {
        auto const& temporal_calendar_object = temporal_calendar_like.as_object();

        // a. If temporalCalendarLike has an [[InitializedTemporalDate]], [[InitializedTemporalDateTime]],
        //    [[InitializedTemporalMonthDay]], [[InitializedTemporalYearMonth]], or [[InitializedTemporalZonedDateTime]]
        //    internal slot, then
        //     i. Return temporalCalendarLike.[[Calendar]].
        if (is<PlainDate>(temporal_calendar_object))
            return static_cast<PlainDate const&>(temporal_calendar_object).calendar();
        if (is<PlainDateTime>(temporal_calendar_object))
            return static_cast<PlainDateTime const&>(temporal_calendar_object).calendar();
        if (is<PlainMonthDay>(temporal_calendar_object))
            return static_cast<PlainMonthDay const&>(temporal_calendar_object).calendar();
        if (is<PlainYearMonth>(temporal_calendar_object))
            return static_cast<PlainYearMonth const&>(temporal_calendar_object).calendar();
        if (is<ZonedDateTime>(temporal_calendar_object))
            return static_cast<ZonedDateTime const&>(temporal_calendar_object).calendar();
    }

    // 2. If temporalCalendarLike is not a String, throw a TypeError exception.
    if (!temporal_calendar_like.is_string())
        return vm.throw_completion<TypeError>(ErrorType::TemporalInvalidCalendar);

    // 3. Let identifier be ? ParseTemporalCalendarString(temporalCalendarLike).
    auto identifier = TRY(parse_temporal_calendar_string(vm, temporal_calendar_like.as_string().utf8_string()));

    // 4. Return ? CanonicalizeCalendar(identifier).
    return TRY(canonicalize_calendar(vm, identifier));
}

// 12.3.11 GetTemporalCalendarIdentifierWithISODefault ( item ), https://tc39.es/proposal-temporal/#sec-temporal-gettemporalcalendarslotvaluewithisodefault
ThrowCompletionOr<String> get_temporal_calendar_identifier_with_iso_default(VM& vm, Object const& item)
{
    // 1. If item has an [[InitializedTemporalDate]], [[InitializedTemporalDateTime]], [[InitializedTemporalMonthDay]],
    //    [[InitializedTemporalYearMonth]], or [[InitializedTemporalZonedDateTime]] internal slot, then
    //     a. Return item.[[Calendar]].
    if (is<PlainDate>(item))
        return static_cast<PlainDate const&>(item).calendar();
    if (is<PlainDateTime>(item))
        return static_cast<PlainDateTime const&>(item).calendar();
    if (is<PlainMonthDay>(item))
        return static_cast<PlainMonthDay const&>(item).calendar();
    if (is<PlainYearMonth>(item))
        return static_cast<PlainYearMonth const&>(item).calendar();
    if (is<ZonedDateTime>(item))
        return static_cast<PlainYearMonth const&>(item).calendar();

    // 2. Let calendarLike be ? Get(item, "calendar").
    auto calendar_like = TRY(item.get(vm.names.calendar));

    // 3. If calendarLike is undefined, then
    if (calendar_like.is_undefined()) {
        // a. Return "iso8601".
        return "iso8601"_string;
    }

    // 4. Return ? ToTemporalCalendarIdentifier(calendarLike).
    return TRY(to_temporal_calendar_identifier(vm, calendar_like));
}

// 12.3.12 CalendarDateFromFields ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendardatefromfields
ThrowCompletionOr<ISODate> calendar_date_from_fields(VM& vm, StringView calendar, CalendarFields& fields, Overflow overflow)
{
    // 1. Perform ? CalendarResolveFields(calendar, fields, DATE).
    TRY(calendar_resolve_fields(vm, calendar, fields, DateType::Date));

    // 2. Let result be ? CalendarDateToISO(calendar, fields, overflow).
    auto result = TRY(calendar_date_to_iso(vm, calendar, fields, overflow));

    // 3. If ISODateWithinLimits(result) is false, throw a RangeError exception.
    if (!iso_date_within_limits(result))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 4. Return result.
    return result;
}

// 12.3.13 CalendarYearMonthFromFields ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendaryearmonthfromfields
ThrowCompletionOr<ISODate> calendar_year_month_from_fields(VM& vm, StringView calendar, CalendarFields& fields, Overflow overflow)
{
    // 1. Set fields.[[Day]] to 1.
    fields.day = 1;

    // 2. Perform ? CalendarResolveFields(calendar, fields, YEAR-MONTH).
    TRY(calendar_resolve_fields(vm, calendar, fields, DateType::YearMonth));

    // 3. Let result be ? CalendarDateToISO(calendar, fields, overflow).
    auto result = TRY(calendar_date_to_iso(vm, calendar, fields, overflow));

    // 4. If ISOYearMonthWithinLimits(result) is false, throw a RangeError exception.
    if (!iso_year_month_within_limits(result))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 5. Return result.
    return result;
}

// 12.3.14 CalendarMonthDayFromFields ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendarmonthdayfromfields
ThrowCompletionOr<ISODate> calendar_month_day_from_fields(VM& vm, StringView calendar, CalendarFields& fields, Overflow overflow)
{
    // 1. Perform ? CalendarResolveFields(calendar, fields, MONTH-DAY).
    TRY(calendar_resolve_fields(vm, calendar, fields, DateType::MonthDay));

    // 2. Let result be ? CalendarMonthDayToISOReferenceDate(calendar, fields, overflow).
    auto result = TRY(calendar_month_day_to_iso_reference_date(vm, calendar, fields, overflow));

    // 3. If ISODateWithinLimits(result) is false, throw a RangeError exception.
    if (!iso_date_within_limits(result))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 4. Return result.
    return result;
}

// 12.3.15 FormatCalendarAnnotation ( id, showCalendar ), https://tc39.es/proposal-temporal/#sec-temporal-formatcalendarannotation
String format_calendar_annotation(StringView id, ShowCalendar show_calendar)
{
    // 1. If showCalendar is NEVER, return the empty String.
    if (show_calendar == ShowCalendar::Never)
        return String {};

    // 2. If showCalendar is AUTO and id is "iso8601", return the empty String.
    if (show_calendar == ShowCalendar::Auto && id == "iso8601"sv)
        return String {};

    // 3. If showCalendar is CRITICAL, let flag be "!"; else, let flag be the empty String.
    auto flag = show_calendar == ShowCalendar::Critical ? "!"sv : ""sv;

    // 4. Return the string-concatenation of "[", flag, "u-ca=", id, and "]".
    return MUST(String::formatted("[{}u-ca={}]", flag, id));
}

// 12.3.16 CalendarEquals ( one, two ), https://tc39.es/proposal-temporal/#sec-temporal-calendarequals
bool calendar_equals(StringView one, StringView two)
{
    // 1. If CanonicalizeUValue("ca", one) is CanonicalizeUValue("ca", two), return true.
    // 2. Return false.
    return Unicode::canonicalize_unicode_extension_values("ca"sv, one)
        == Unicode::canonicalize_unicode_extension_values("ca"sv, two);
}

// 12.3.17 ISODaysInMonth ( year, month ), https://tc39.es/proposal-temporal/#sec-temporal-isodaysinmonth
u8 iso_days_in_month(double year, double month)
{
    // 1. If month is 1, 3, 5, 7, 8, 10, or 12, return 31.
    if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12)
        return 31;

    // 2. If month is 4, 6, 9, or 11, return 30.
    if (month == 4 || month == 6 || month == 9 || month == 11)
        return 30;

    // 3. Assert: month is 2.
    VERIFY(month == 2);

    // 4. Return 28 + MathematicalInLeapYear(EpochTimeForYear(year)).
    return 28 + mathematical_in_leap_year(epoch_time_for_year(year));
}

// 12.3.18 ISOWeekOfYear ( isoDate ), https://tc39.es/proposal-temporal/#sec-temporal-isoweekofyear
YearWeek iso_week_of_year(ISODate iso_date)
{
    // 1. Let year be isoDate.[[Year]].
    auto year = iso_date.year;

    // 2. Let wednesday be 3.
    static constexpr auto wednesday = 3;

    // 3. Let thursday be 4.
    static constexpr auto thursday = 4;

    // 4. Let friday be 5.
    static constexpr auto friday = 5;

    // 5. Let saturday be 6.
    static constexpr auto saturday = 6;

    // 6. Let daysInWeek be 7.
    static constexpr auto days_in_week = 7;

    // 7. Let maxWeekNumber be 53.
    static constexpr auto max_week_number = 53;

    // 8. Let dayOfYear be ISODayOfYear(isoDate).
    auto day_of_year = iso_day_of_year(iso_date);

    // 9. Let dayOfWeek be ISODayOfWeek(isoDate).
    auto day_of_week = iso_day_of_week(iso_date);

    // 10. Let week be floor((dayOfYear + daysInWeek - dayOfWeek + wednesday) / daysInWeek).
    auto week = floor(static_cast<double>(day_of_year + days_in_week - day_of_week + wednesday) / static_cast<double>(days_in_week));

    // 11. If week < 1, then
    if (week < 1) {
        // a. NOTE: This is the last week of the previous year.

        // b. Let jan1st be CreateISODateRecord(year, 1, 1).
        auto jan1st = create_iso_date_record(year, 1, 1);

        // c. Let dayOfJan1st be ISODayOfWeek(jan1st).
        auto day_of_jan1st = iso_day_of_week(jan1st);

        // d. If dayOfJan1st = friday, then
        if (day_of_jan1st == friday) {
            // i. Return Year-Week Record { [[Week]]: maxWeekNumber, [[Year]]: year - 1 }.
            return { .week = max_week_number, .year = year - 1 };
        }

        // e. If dayOfJan1st = saturday, and MathematicalInLeapYear(EpochTimeForYear(year - 1)) = 1, then
        if (day_of_jan1st == saturday && mathematical_in_leap_year(epoch_time_for_year(year - 1)) == 1) {
            // i. Return Year-Week Record { [[Week]]: maxWeekNumber. [[Year]]: year - 1 }.
            return { .week = max_week_number, .year = year - 1 };
        }

        // f. Return Year-Week Record { [[Week]]: maxWeekNumber - 1, [[Year]]: year - 1 }.
        return { .week = max_week_number - 1, .year = year - 1 };
    }

    // 12. If week = maxWeekNumber, then
    if (week == max_week_number) {
        // a. Let daysInYear be MathematicalDaysInYear(year).
        auto days_in_year = mathematical_days_in_year(year);

        // b. Let daysLaterInYear be daysInYear - dayOfYear.
        auto days_later_in_year = days_in_year - day_of_year;

        // c. Let daysAfterThursday be thursday - dayOfWeek.
        auto days_after_thursday = thursday - day_of_week;

        // d. If daysLaterInYear < daysAfterThursday, then
        if (days_later_in_year < days_after_thursday) {
            // i. Return Year-Week Record { [[Week]]: 1, [[Year]]: year + 1 }.
            return { .week = 1, .year = year + 1 };
        }
    }

    // 13. Return Year-Week Record { [[Week]]: week, [[Year]]: year }.
    return { .week = week, .year = year };
}

// 12.3.19 ISODayOfYear ( isoDate ), https://tc39.es/proposal-temporal/#sec-temporal-isodayofyear
u16 iso_day_of_year(ISODate iso_date)
{
    // 1. Let epochDays be ISODateToEpochDays(isoDate.[[Year]], isoDate.[[Month]] - 1, isoDate.[[Day]]).
    auto epoch_days = iso_date_to_epoch_days(iso_date.year, iso_date.month - 1, iso_date.day);

    // 2. Return EpochTimeToDayInYear(EpochDaysToEpochMs(epochDays, 0)) + 1.
    return epoch_time_to_day_in_year(epoch_days_to_epoch_ms(epoch_days, 0)) + 1;
}

// 12.3.20 ISODayOfWeek ( isoDate ), https://tc39.es/proposal-temporal/#sec-temporal-isodayofweek
u8 iso_day_of_week(ISODate iso_date)
{
    // 1. Let epochDays be ISODateToEpochDays(isoDate.[[Year]], isoDate.[[Month]] - 1, isoDate.[[Day]]).
    auto epoch_days = iso_date_to_epoch_days(iso_date.year, iso_date.month - 1, iso_date.day);

    // 2. Let dayOfWeek be EpochTimeToWeekDay(EpochDaysToEpochMs(epochDays, 0)).
    auto day_of_week = epoch_time_to_week_day(epoch_days_to_epoch_ms(epoch_days, 0));

    // 3. If dayOfWeek = 0, return 7.
    if (day_of_week == 0)
        return 7;

    // 4. Return dayOfWeek.
    return day_of_week;
}

// 12.3.21 NonISOCalendarDateToISO ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-nonisocalendardatetoiso
// 4.1.20 NonISOCalendarDateToISO ( calendar, fields, overflow ), https://tc39.es/proposal-intl-era-monthcode/#sup-temporal-nonisocalendardatetoiso
ThrowCompletionOr<ISODate> non_iso_calendar_date_to_iso(VM& vm, StringView calendar, CalendarFields const& fields, Overflow overflow)
{
    dbgln("!!! non_iso_calendar_date_to_iso: {} {} {} {} {}", calendar, fields.year, fields.month_code, fields.month, fields.day);

    i32 arithmetic_year = 0;

    // 1. If fields.[[Era]] and fields.[[EraYear]] are not UNSET, then
    if (fields.era.has_value() && fields.era_year.has_value()) {
        // a. If CanonicalizeEraInCalendar(calendar, fields.[[Era]]) is undefined, throw a RangeError exception.
        if (!canonicalize_era_in_calendar(calendar, *fields.era).has_value())
            return vm.throw_completion<RangeError>(ErrorType::IntlTemporalInvalidEra, *fields.era, calendar);

        // b. Let arithmeticYear be CalendarDateArithmeticYearForEraYear(calendar, fields.[[Era]], fields.[[EraYear]]).
        arithmetic_year = calendar_date_arithmetic_year_for_era_year(calendar, *fields.era, *fields.era_year);
    }
    // 2. Else,
    else {
        // a. Assert: fields.[[Year]] is not UNSET.
        VERIFY(fields.year.has_value());

        // b. Let arithmeticYear be fields.[[Year]].
        arithmetic_year = *fields.year;
    }

    i8 ordinal_month = 0;

    // 3. If fields.[[MonthCode]] is not UNSET, then
    if (fields.month_code.has_value()) {
        // a. If IsValidMonthCodeForCalendar(calendar, fields.[[MonthCode]]) is false, throw a RangeError exception.
        if (!is_valid_month_code_for_calendar(calendar, *fields.month_code))
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidMonthCode);

        // b. Let constrainedMonthCode be ? ConstrainMonthCode(calendar, arithmeticYear, fields.[[MonthCode]], overflow).
        auto constrained_month_code = TRY(constrain_month_code(vm, calendar, arithmetic_year, *fields.month_code, overflow));
        dbgln("    {}", constrained_month_code);

        // c. Let ordinalMonth be MonthCodeToOrdinal(calendar, arithmeticYear, constrainedMonthCode).
        ordinal_month = month_code_to_ordinal(vm, calendar, arithmetic_year, constrained_month_code);
        dbgln("    {}", ordinal_month);
    }
    // 4. Else,
    else {
        // a. Assert: fields.[[Month]] is not UNSET.
        VERIFY(fields.month.has_value());

        // b. Let ordinalMonth be fields.[[Month]].
        ordinal_month = *fields.month;
    }

    // 5. Let day be fields.[[Day]].
    // 6. Assert: day is not UNSET.
    VERIFY(fields.day.has_value());
    auto day = *fields.day;

    // 7. Let daysInMonth be CalendarDaysInMonth(calendar, arithmeticYear, ordinalMonth).
    auto days_in_month = calendar_days_in_month(calendar, arithmetic_year, ordinal_month);

    u8 regulated_day = 0;

    // 8. If daysInMonth < day, then
    if (days_in_month < day) {
        // a. If overflow is REJECT, throw a RangeError exception.
        if (overflow == Overflow::Reject)
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

        // b. Let regulatedDay be daysInMonth.
        regulated_day = days_in_month;
    }
    // 9. Else,
    else {
        // a. Assert: day ≥ 1.
        VERIFY(day >= 1);

        // b. Let regulatedDay be day.
        regulated_day = day;
    }

    // 10. Return ? CalendarIntegersToISO(calendar, arithmeticYear, ordinalMonth, regulatedDay).
    return TRY(calendar_integers_to_iso(vm, calendar, arithmetic_year, ordinal_month, regulated_day));
}

// 12.3.22 CalendarDateToISO ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendardatetoiso
ThrowCompletionOr<ISODate> calendar_date_to_iso(VM& vm, StringView calendar, CalendarFields const& fields, Overflow overflow)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. Assert: fields.[[Year]], fields.[[Month]], and fields.[[Day]] are not UNSET.
        VERIFY(fields.year.has_value());
        VERIFY(fields.month.has_value());
        VERIFY(fields.day.has_value());

        // b. Return ? RegulateISODate(fields.[[Year]], fields.[[Month]], fields.[[Day]], overflow).
        return TRY(regulate_iso_date(vm, *fields.year, *fields.month, *fields.day, overflow));
    }

    // 2. Return ? NonISOCalendarDateToISO(calendar, fields, overflow).
    return non_iso_calendar_date_to_iso(vm, calendar, fields, overflow);
}

[[maybe_unused]] static constexpr i32 month_day_search_start_year(StringView month_code, u32 day)
{
    // Note that ICU4C actually has _no_ years in which leap months M01L and
    // M09L through M12L have 30 days. The values marked with (*) here are years
    // in which the leap month occurs with 29 days. ICU4C disagrees with ICU4X
    // here and it is not clear which is correct.
    if (month_code == "M01L"sv)
        return 1651; // *
    if (month_code == "M02L"sv)
        return day < 30 ? 1947 : 1765;
    if (month_code == "M03L"sv)
        return day < 30 ? 1966 : 1955;
    if (month_code == "M04L"sv)
        return day < 30 ? 1963 : 1944;
    if (month_code == "M05L"sv)
        return day < 30 ? 1971 : 1952;
    if (month_code == "M06L"sv)
        return day < 30 ? 1960 : 1941;
    if (month_code == "M07L"sv)
        return day < 30 ? 1968 : 1938;
    if (month_code == "M08L"sv)
        return day < 30 ? 1957 : 1718;
    if (month_code == "M09L"sv)
        return 1832; // *
    if (month_code == "M10L"sv)
        return 1870; // *
    if (month_code == "M11L"sv)
        return 1814; // *
    if (month_code == "M12L"sv)
        return 1890; // *
    return 1972;
}

// 12.3.23 NonISOMonthDayToISOReferenceDate ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-nonisomonthdaytoisoreferencedate
ThrowCompletionOr<ISODate> non_iso_month_day_to_iso_reference_date(VM& vm, StringView calendar, CalendarFields const& fields, Overflow overflow)
{
    dbgln("!!! non_iso_month_day_to_iso_reference_date: {}: {} {} {} {} {}", calendar, fields.era_year, fields.year, fields.month, fields.month_code, fields.day);

    auto supports_eras = calendar_supports_era(calendar);
    auto month = fields.month;
    auto month_code = fields.month_code;
    auto day = fields.day;

    if (!month_code.has_value() || fields.year.has_value() || (supports_eras && fields.era_year.has_value())) {
        // Apply overflow behaviour to year/month/day, to get correct monthCode/day
        auto iso_date = TRY(calendar_date_to_iso(vm, calendar, fields, overflow));
        auto calendar_date = calendar_iso_to_date(calendar, iso_date);

        month = calendar_date.month;
        month_code = calendar_date.month_code;
        day = calendar_date.day;
    }

    if (!is_valid_month_code_for_calendar(calendar, *month_code))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "monthCode"sv);

    // FIXME: Apply `overflow` to day in month. i.e. make sure the day does not exceed the month's number of days.

    struct ISODateRange {
        ISODate start;
        ISODate end;
    };
    static constexpr auto iso_date_ranges = to_array<ISODateRange>({
        // The reference date is the latest ISO 8601 date corresponding to the calendar date that is between
        // January 1, 1900 and December 31, 1972 inclusive.
        { ISODate { .year = 1972, .month = 12, .day = 31 }, ISODate { .year = 1900, .month = 1, .day = 1 } },

        // If there is no such date, it is the earliest ISO 8601 date corresponding to the calendar date between
        // January 1, 1973 and December 31, 2035.
        { ISODate { .year = 1973, .month = 1, .day = 1 }, ISODate { .year = 2035, .month = 12, .day = 31 } },

        // If there is still no such date, it is the latest ISO 8601 date corresponding to the calendar date on or
        // before December 31, 1899.
        { ISODate { .year = 1899, .month = 12, .day = 31 }, ISODate { .year = -8000, .month = 1, .day = 1 } },
    });

    CalendarFields new_fields;
    new_fields.month = month;
    new_fields.month_code = month_code;
    new_fields.day = day;

    for (auto const& [start, end] : iso_date_ranges) {
        dbgln("##############################");
        auto sign = start.year > end.year ? -1 : 1;

        auto start_calendar_date = calendar_iso_to_date(calendar, start);
        auto end_calendar_date = calendar_iso_to_date(calendar, end);

        for (auto calendar_year = start_calendar_date.year;
            sign < 0 ? calendar_year >= end_calendar_date.year : calendar_year <= end_calendar_date.year;
            calendar_year += sign) {
            dbgln("==============================");
            new_fields.year = calendar_year;

            auto iso_date = calendar_date_to_iso(vm, calendar, new_fields, overflow);
            if (iso_date.is_error())
                continue;

            if (compare_iso_date(iso_date.value(), start) != sign)
                continue;
            if (compare_iso_date(iso_date.value(), end) == sign)
                continue;

            // auto round_trip_calendar_date = calendar_iso_to_date(calendar, iso_date);
            // if (round_trip_calendar_date.month_code == month_code && round_trip_calendar_date.day == day)
            return iso_date.release_value();
        }

        dbgln("##############################");
        break;
    }

    return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // auto calendar_start_date = calendar_iso_to_date(calendar, iso_start_date);

    // auto calendar_year = calendar_start_date.month_code > *month_code || (calendar_start_date.month_code == month_code && calendar_start_date.day >= *day)
    //     ? calendar_start_date.year
    //     : calendar_start_date.year - 1;

    // for (i32 i = 0; i < 20; ++i) {
    //     CalendarFields new_fields;
    //     // new_fields.era = calendar_start_date.era;
    //     // new_fields.era_year = calendar_start_date.era_year;
    //     new_fields.year = calendar_year - i;
    //     // new_fields.month = calendar_start_date.month;
    //     new_fields.month_code = month_code;
    //     new_fields.day = day;

    //     auto iso_date = TRY(calendar_date_to_iso(vm, calendar, new_fields, overflow));
    //     auto round_trip_calendar_date = calendar_iso_to_date(calendar, iso_date);

    //     dbgln("!!! {} {} {}, {} {} {}, {} {} {}",
    //         new_fields.year, new_fields.month_code, new_fields.day,
    //         iso_date.year, iso_date.month, iso_date.day,
    //         round_trip_calendar_date.year, round_trip_calendar_date.month_code, round_trip_calendar_date.day);

    //     if (round_trip_calendar_date.month_code == month_code && round_trip_calendar_date.day == day)
    //         return iso_date;
    // }

    VERIFY_NOT_REACHED();

    // Look backwards starting from one of the calendar years spanning ISO year
    // 1972, up to 20 calendar years prior, to find a year that has this month
    // and day. Normal months and days will match immediately, but for leap days
    // and leap months we may have to look for a while. For searches longer than
    // 20 years, override the start date in monthDaySearchStartYear.
    // const startDateIso = {
    //   year: this.monthDaySearchStartYear(monthCode, day),
    //   month: 12,
    //   day: 31
    // };
    // const calendarOfStartDateIso = this.isoToCalendarDate(startDateIso, cache);
    // // Note: relies on lexicographical ordering of monthCodes
    // const calendarYear =
    //   calendarOfStartDateIso.monthCode > monthCode ||
    //   (calendarOfStartDateIso.monthCode === monthCode && calendarOfStartDateIso.day >= day)
    //     ? calendarOfStartDateIso.year
    //     : calendarOfStartDateIso.year - 1;
    //
    //
    //
    // for (let i = 0; i < 20; i++) {
    //   let testCalendarDate = this.adjustCalendarDate({ day, monthCode, year: calendarYear - i }, cache);
    //   const isoDate = this.calendarToIsoDate(testCalendarDate, 'constrain', cache);
    //   const roundTripCalendarDate = this.isoToCalendarDate(isoDate, cache);
    //   if (roundTripCalendarDate.monthCode === monthCode && roundTripCalendarDate.day === day) {
    //     return isoDate;
    //   }
    // }
    // assertNotReached(`no recent ${this.id} year with ${monthCode}-${day}, adjust monthDaySearchStartYear`);
}

// 12.3.24 CalendarMonthDayToISOReferenceDate ( calendar, fields, overflow ), https://tc39.es/proposal-temporal/#sec-temporal-calendarmonthdaytoisoreferencedate
ThrowCompletionOr<ISODate> calendar_month_day_to_iso_reference_date(VM& vm, StringView calendar, CalendarFields const& fields, Overflow overflow)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. Assert: fields.[[Month]] and fields.[[Day]] are not UNSET.
        VERIFY(fields.month.has_value());
        VERIFY(fields.day.has_value());

        // b. Let referenceISOYear be 1972 (the first ISO 8601 leap year after the epoch).
        static constexpr i32 reference_iso_year = 1972;

        // c. If fields.[[Year]] is UNSET, let year be referenceISOYear; else let year be fields.[[Year]].
        auto year = !fields.year.has_value() ? reference_iso_year : *fields.year;

        // d. Let result be ? RegulateISODate(year, fields.[[Month]], fields.[[Day]], overflow).
        auto result = TRY(regulate_iso_date(vm, year, *fields.month, *fields.day, overflow));

        // e. Return CreateISODateRecord(referenceISOYear, result.[[Month]], result.[[Day]]).
        return create_iso_date_record(reference_iso_year, result.month, result.day);
    }

    // 2. Return ? NonISOMonthDayToISOReferenceDate(calendar, fields, overflow).
    return non_iso_month_day_to_iso_reference_date(vm, calendar, fields, overflow);
}

// 12.3.25 NonISOCalendarISOToDate ( calendar, isoDate ), https://tc39.es/proposal-temporal/#sec-temporal-nonisocalendarisotodate
CalendarDate non_iso_calendar_iso_to_date(StringView calendar, ISODate iso_date)
{
    dbgln("!!! non_iso_calendar_iso_to_date");
    auto result = Unicode::iso_date_to_calendar_date(calendar, to_unicode_iso_date(iso_date));

    return CalendarDate {
        .era = result.era,
        .era_year = result.era_year,

        .year = result.year,
        .month = static_cast<u8>(result.month + 1),
        .month_code = result.month_code,

        .day = result.day,
        .day_of_week = result.day_of_week,
        .day_of_year = result.day_of_year,

        .week_of_year = {
            .week = result.week_of_year.week,
            .year = result.week_of_year.year,
        },

        .days_in_week = result.days_in_week,
        .days_in_month = result.days_in_month,
        .days_in_year = result.days_in_year,
        .months_in_year = result.months_in_year,
        .in_leap_year = result.in_leap_year,
    };
}

// 12.3.26 CalendarISOToDate ( calendar, isoDate ), https://tc39.es/proposal-temporal/#sec-temporal-calendarisotodate
CalendarDate calendar_iso_to_date(StringView calendar, ISODate iso_date)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. If MathematicalInLeapYear(EpochTimeForYear(isoDate.[[Year]])) = 1, let inLeapYear be true; else let inLeapYear be false.
        auto in_leap_year = mathematical_in_leap_year(epoch_time_for_year(iso_date.year)) == 1;

        // b. Return Calendar Date Record { [[Era]]: undefined, [[EraYear]]: undefined, [[Year]]: isoDate.[[Year]],
        //    [[Month]]: isoDate.[[Month]], [[MonthCode]]: CreateMonthCode(isoDate.[[Month]], false), [[Day]]: isoDate.[[Day]],
        //    [[DayOfWeek]]: ISODayOfWeek(isoDate), [[DayOfYear]]: ISODayOfYear(isoDate), [[WeekOfYear]]: ISOWeekOfYear(isoDate),
        //    [[DaysInWeek]]: 7, [[DaysInMonth]]: ISODaysInMonth(isoDate.[[Year]], isoDate.[[Month]]),
        //    [[DaysInYear]]: MathematicalDaysInYear(isoDate.[[Year]]), [[MonthsInYear]]: 12, [[InLeapYear]]: inLeapYear }.
        return CalendarDate {
            .era = {},
            .era_year = {},
            .year = iso_date.year,
            .month = iso_date.month,
            .month_code = create_month_code(iso_date.month, false),
            .day = iso_date.day,
            .day_of_week = iso_day_of_week(iso_date),
            .day_of_year = iso_day_of_year(iso_date),
            .week_of_year = iso_week_of_year(iso_date),
            .days_in_week = 7,
            .days_in_month = iso_days_in_month(iso_date.year, iso_date.month),
            .days_in_year = mathematical_days_in_year(iso_date.year),
            .months_in_year = 12,
            .in_leap_year = in_leap_year,
        };
    }

    // 2. Return NonISOCalendarISOToDate(calendar, isoDate).
    return non_iso_calendar_iso_to_date(calendar, iso_date);
}

// 12.3.27 CalendarExtraFields ( calendar, fields ), https://tc39.es/proposal-temporal/#sec-temporal-calendarextrafields
// 4.1.21 CalendarExtraFields ( calendar, fields ), https://tc39.es/proposal-intl-era-monthcode/#sup-temporal-calendarextrafields
Vector<CalendarField> calendar_extra_fields(StringView calendar, CalendarFieldList fields)
{
    // 1. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return {};

    // 2. If fields contains an element equal to YEAR and CalendarSupportsEra(calendar) is true, then
    if (fields.contains_slow(CalendarField::Year) && calendar_supports_era(calendar)) {
        // a. Return « ERA, ERA-YEAR ».
        return { CalendarField::Era, CalendarField::EraYear };
    }

    // 3. Return an empty List.
    return {};
}

// 12.3.28 NonISOFieldKeysToIgnore ( calendar, keys ), https://tc39.es/proposal-temporal/#sec-temporal-nonisofieldkeystoignore
// 4.1.22 NonISOFieldKeysToIgnore ( calendar, keys ), https://tc39.es/proposal-intl-era-monthcode/#sup-temporal-nonisofieldkeystoignore
Vector<CalendarField> non_iso_field_keys_to_ignore(StringView calendar, ReadonlySpan<CalendarField> keys)
{
    dbgln("!!! non_iso_field_keys_to_ignore");

    // 1. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return {};

    // 2. Let ignoredKeys be a copy of keys.
    Vector<CalendarField> ignored_keys { keys };

    // 3. For each element key of keys, do
    for (auto key : keys) {
        // a. If key is MONTH, append MONTH-CODE to ignoredKeys.
        if (key == CalendarField::Month) {
            ignored_keys.append(CalendarField::MonthCode);
        }

        // b. If key is MONTH-CODE, append MONTH.
        if (key == CalendarField::MonthCode) {
            ignored_keys.append(CalendarField::Month);
        }

        // c. If key is one of ERA, ERA-YEAR, or YEAR and CalendarSupportsEra(calendar) is true, then
        if (key == CalendarField::Era || key == CalendarField::EraYear || key == CalendarField::Year) {
            if (calendar_supports_era(calendar)) {
                // i. Append ERA, ERA-YEAR, and YEAR to ignoredKeys.
                ignored_keys.append(CalendarField::Era);
                ignored_keys.append(CalendarField::EraYear);
                ignored_keys.append(CalendarField::Year);
            }
        }

        // d. If key is one of DAY, MONTH, or MONTH-CODE and CalendarHasMidYearEras(calendar) is true, then
        if (key == CalendarField::Day || key == CalendarField::Month || key == CalendarField::MonthCode) {
            if (calendar_has_mid_year_eras(calendar)) {
                // i. Append ERA and ERA-YEAR to ignoredKeys.
                ignored_keys.append(CalendarField::Era);
                ignored_keys.append(CalendarField::EraYear);
            }
        }
    }

    // 4. NOTE: While ignoredKeys can have duplicate elements, this is not intended to be meaningful. This
    //   specification only checks whether particular keys are or are not members of the list.

    // 5. Return ignoredKeys.
    return ignored_keys;
}

// 12.3.29 CalendarFieldKeysToIgnore ( calendar, keys ), https://tc39.es/proposal-temporal/#sec-temporal-calendarfieldkeystoignore
Vector<CalendarField> calendar_field_keys_to_ignore(StringView calendar, ReadonlySpan<CalendarField> keys)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. Let ignoredKeys be an empty List.
        Vector<CalendarField> ignored_keys;

        // b. For each element key of keys, do
        for (auto key : keys) {
            // i. Append key to ignoredKeys.
            ignored_keys.append(key);

            // ii. If key is MONTH, append MONTH-CODE to ignoredKeys.
            if (key == CalendarField::Month)
                ignored_keys.append(CalendarField::MonthCode);
            // iii. Else if key is MONTH-CODE, append MONTH to ignoredKeys.
            else if (key == CalendarField::MonthCode)
                ignored_keys.append(CalendarField::Month);
        }

        // c. NOTE: While ignoredKeys can have duplicate elements, this is not intended to be meaningful. This specification
        //    only checks whether particular keys are or are not members of the list.

        // d. Return ignoredKeys.
        return ignored_keys;
    }

    // 2. Return NonISOFieldKeysToIgnore(calendar, keys).
    return non_iso_field_keys_to_ignore(calendar, keys);
}

// 12.3.30 NonISOResolveFields ( calendar, fields, type ), https://tc39.es/proposal-temporal/#sec-temporal-nonisoresolvefields
ThrowCompletionOr<void> non_iso_resolve_fields(VM& vm, StringView calendar, CalendarFields& fields, DateType type)
{
    dbgln("!!! non_iso_resolve_fields {}", (int)type);
    if ((type == DateType::Date || type == DateType::YearMonth) && !fields.year.has_value()) {
        if (!calendar_supports_era(calendar))
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "year"sv);

        if (!fields.era.has_value() || !fields.era_year.has_value())
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "year (or era and era year)"sv);
    }

    if ((type == DateType::Date || type == DateType::MonthDay) && !fields.day.has_value())
        return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "day"sv);

    if (type == DateType::MonthDay && fields.month.has_value() && !fields.year.has_value()) {
        if (!calendar_supports_era(calendar))
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "year"sv);

        if (!fields.era.has_value() || !fields.era_year.has_value())
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "year (or era and era year)"sv);
    }

    if (true || !is_lunisolar_calendar(calendar)) {
        auto month = fields.month;
        auto month_code = fields.month_code;

        if (!month_code.has_value()) {
            if (!month.has_value())
                return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "month"sv);

            // auto months_per_year = months_per_year_in_non_lunisolar_calendar(calendar);
            fields.month_code = create_month_code(*month, false);
        } else {
            if (!is_valid_month_code_for_calendar(calendar, *month_code))
                return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "monthCode"sv);

            auto [month_number, _] = MUST(parse_month_code(vm, *month_code));

            if (month.has_value() && month != month_number)
                return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "month"sv);

            fields.month = month_number;
        }
    }

    return {};
}

// 12.3.31 CalendarResolveFields ( calendar, fields, type ), https://tc39.es/proposal-temporal/#sec-temporal-calendarresolvefields
ThrowCompletionOr<void> calendar_resolve_fields(VM& vm, StringView calendar, CalendarFields& fields, DateType type)
{
    // 1. If calendar is "iso8601", then
    if (calendar == "iso8601"sv) {
        // a. If type is DATE or YEAR-MONTH and fields.[[Year]] is UNSET, throw a TypeError exception.
        if ((type == DateType::Date || type == DateType::YearMonth) && !fields.year.has_value())
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "year"sv);

        // b. If type is DATE or MONTH-DAY and fields.[[Day]] is UNSET, throw a TypeError exception.
        if ((type == DateType::Date || type == DateType::MonthDay) && !fields.day.has_value())
            return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "day"sv);

        // c. Let month be fields.[[Month]].
        auto const& month = fields.month;

        // d. Let monthCode be fields.[[MonthCode]].
        auto const& month_code = fields.month_code;

        // e. If monthCode is UNSET, then
        if (!month_code.has_value()) {
            // i. If month is UNSET, throw a TypeError exception.
            if (!month.has_value())
                return vm.throw_completion<TypeError>(ErrorType::MissingRequiredProperty, "month"sv);

            // ii. Return UNUSED.
            return {};
        }

        // f. Assert: monthCode is a month code.
        // g. Let parsedMonthCode be ! ParseMonthCode(monthCode).
        auto parsed_month_code = MUST(parse_month_code(vm, *month_code));

        // h. If parsedMonthCode.[[IsLeapMonth]] is true, throw a RangeError exception.
        if (parsed_month_code.is_leap_month)
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "monthCode"sv);

        // i. If parsedMonthCode.[[MonthNumber]] > 12, throw a RangeError exception.
        if (parsed_month_code.month_number > 12)
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "monthCode"sv);

        // j. If month is not UNSET and month ≠ parsedMonthCode.[[MonthNumber]], throw a RangeError exception.
        if (month.has_value() && month != parsed_month_code.month_number)
            return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidCalendarFieldName, "month"sv);

        // k. Set fields.[[Month]] to parsedMonthCode.[[MonthNumber]].
        fields.month = parsed_month_code.month_number;
    }
    // 2. Else,
    else {
        // a. Perform ? NonISOResolveFields(calendar, fields, type).
        return TRY(non_iso_resolve_fields(vm, calendar, fields, type));
    }

    // 3. Return UNUSED.
    return {};
}

// 4.1.1 CalendarSupportsEra ( calendar ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendarsupportsera
bool calendar_supports_era(StringView calendar)
{
    // 1. If calendar is listed in the "Calendar" column of Table 2, return true.
    if (find_value(CALENDAR_ERA_DATA, [&](auto const& row) { return row.calendar == calendar; }).has_value())
        return true;

    // 2. If calendar is listed in the "Calendar Type" column of Table 1, return false.
    // 3. Return an implementation-defined value.
    return false;
}

// 4.1.2 CanonicalizeEraInCalendar ( calendar, era ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-canonicalizeeraincalendar
Optional<StringView> canonicalize_era_in_calendar(StringView calendar, StringView era)
{
    // 1. For each row of Table 2, except the header row, do
    for (auto const& row : CALENDAR_ERA_DATA) {
        // a. Let cal be the Calendar value of the current row.
        // b. If cal is equal to calendar, then
        if (row.calendar == calendar) {
            // i. Let canonicalName be the Era value of the current row.
            auto canonical_name = row.era;

            // ii. If canonicalName is equal to era, return canonicalName.
            if (canonical_name == era)
                return canonical_name;

            // iii. Let aliases be a List whose elements are the strings given in the "Aliases" column of the row.
            // iv. If aliases contains era, return canonicalName.
            if (row.alias.contains_slow(era))
                return canonical_name;
        }
    }

    // 2. If calendar is listed in the "Calendar Type" column of Table 1, return undefined.
    // 3. Return an implementation-defined value.
    return {};
}

// 4.1.3 CalendarHasMidYearEras ( calendar ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendarhasmidyeareras
bool calendar_has_mid_year_eras(StringView calendar)
{
    // 1. If calendar is "japanese", return true.
    if (calendar == "japanese"sv)
        return true;

    // 2. If calendar is listed in the "Calendar Type" column of Table 1, return false.
    // 3. Return an implementation-defined value.
    return false;
}

// 4.1.4 IsValidMonthCodeForCalendar ( calendar, monthCode ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-isvalidmonthcodeforcalendar
bool is_valid_month_code_for_calendar(StringView calendar, StringView month_code)
{
    // 1. Let commonMonthCodes be « "M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12" ».
    // 2. If commonMonthCodes contains monthCode, return true.
    if (month_code.is_one_of("M01"sv, "M02"sv, "M03"sv, "M04"sv, "M05"sv, "M06"sv, "M07"sv, "M08"sv, "M09"sv, "M10"sv, "M11"sv, "M12"sv))
        return true;

    // 3. If calendar is listed in the "Calendar" column of Table 3, then
    if (auto row = find_value(ADDITIONAL_MONTH_CODES, [&](auto const& row) { return row.calendar == calendar; }); row.has_value()) {
        // a. Let r be the row in Table 3 with a value in the Calendar column matching calendar.
        // b. Let specialMonthCodes be a List whose elements are the strings given in the "Additional Month Codes" column of r.
        // c. If specialMonthCodes contains monthCode, return true.
        // d. Return false.
        return row->additional_month_codes.contains_slow(month_code);
    }

    // 4. If calendar is listed in the "Calendar Type" column of Table 1, return false.
    // 5. Return an implementation-defined value.
    return false;
}

// 4.1.5 YearContainsMonthCode ( calendar, arithmeticYear, monthCode ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-yearcontainsmonthcode
bool year_contains_month_code(StringView calendar, i32 arithmetic_year, StringView month_code)
{
    // 1. Assert: IsValidMonthCodeForCalendar(calendar, monthCode) is true.
    if (!is_valid_month_code_for_calendar(calendar, month_code))
        return false;

    // 2. If ! ParseMonthCode(monthCode).[[IsLeap]] is false, return true.
    // NB: We avoid passing in a VM here, and just check if the leap month symbol is specified.
    if (!month_code.ends_with('L'))
        return true;

    // 3. Return whether the leap month indicated by monthCode exists in the year arithmeticYear in calendar, using
    //    calendar-dependent behaviour.
    return Unicode::calendar_year_contains_month_code(calendar, arithmetic_year, month_code);
}

// 4.1.6 ConstrainMonthCode ( calendar, arithmeticYear, monthCode, overflow ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-constrainmonthcode
ThrowCompletionOr<StringView> constrain_month_code(VM& vm, StringView calendar, i32 arithmetic_year, StringView month_code, Overflow overflow)
{
    // 1. Assert: IsValidMonthCodeForCalendar(calendar, monthCode) is true.
    VERIFY(is_valid_month_code_for_calendar(calendar, month_code));

    // 2. If YearContainsMonthCode(calendar, arithmeticYear, monthCode) is true, return monthCode.
    if (year_contains_month_code(calendar, arithmetic_year, month_code))
        return month_code;

    // 3. If overflow is reject, throw a RangeError exception.
    if (overflow == Overflow::Reject)
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidMonthCode);

    // 4. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidMonthCode);

    // 5. Assert: calendar is listed in the "Calendar" column of Table 3.
    // 6. Let r be the row in Table 3 with a value in the Calendar column matching calendar.
    auto row = find_value(ADDITIONAL_MONTH_CODES, [&](auto const& row) { return row.calendar == calendar; });
    VERIFY(row.has_value());

    // 7. Let shiftType be the value given in the "Leap to Common Month Tranformation" column of r.
    // 8. If shiftType is skip-backward, then
    if (row->leap_to_common_month_transformation == AdditionalMonthCodes::Leap::SkipBackward) {
        // a. Return CreateMonthCode(! ParseMonthCode(monthCode).[[MonthNumber]], false).
        return month_code.trim("L"sv, TrimMode::Right);
    }
    // 9. Else,
    else {
        // a. Assert: monthCode is "M05L".
        VERIFY(month_code == "M05L"sv);

        // b. Return "M06".
        return "M06"sv;
    }
}

// 4.1.7 MonthCodeToOrdinal ( calendar, arithmeticYear, monthCode ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-monthcodetoordinal
u8 month_code_to_ordinal(VM& vm, StringView calendar, i32 arithmetic_year, StringView month_code)
{
    dbgln("### {} {} {}", calendar, arithmetic_year, month_code);

    // 1. Assert: YearContainsMonthCode(calendar, arithmeticYear, monthCode) is true.
    VERIFY(year_contains_month_code(calendar, arithmetic_year, month_code));

    // 2. Let monthsBefore be 0.
    u8 months_before = 0;

    // 3. Let number be 1.
    u8 number = 1;

    // 4. Let isLeap be false.
    auto is_leap = false;

    // 5. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return MUST(parse_month_code(vm, month_code)).month_number;

    // 6. Let r be the row in Table 3 with a value in the Calendar column matching calendar.
    auto row = find_value(ADDITIONAL_MONTH_CODES, [&](auto const& row) { return row.calendar == calendar; });
    if (!row.has_value())
        return MUST(parse_month_code(vm, month_code)).month_number;

    // 7. If the "Leap to Common Month Tranformation" column of r is empty, then
    if (!row->leap_to_common_month_transformation.has_value()) {
        // a. Return ! ParseMonthCode(monthCode).[[MonthNumber]].
        return MUST(parse_month_code(vm, month_code)).month_number;
    }

    // 8. Assert: The "Additional Month Codes" column of r does not contain "M00L" or "M13".
    VERIFY(!row->additional_month_codes.contains_slow("M00L"sv));
    VERIFY(!row->additional_month_codes.contains_slow("M13"sv));

    // 9. Assert: The following loop will terminate.

    // 10. Repeat, while number ≤ 12,
    while (number <= 12) {
        // a. Let currentMonthCode be CreateMonthCode(number, isLeap).
        auto current_month_code = create_month_code(number, is_leap);
        dbgln("    {} '{}' {}", number, current_month_code, months_before);

        // b. If YearContainsMonthCode(calendar, arithmeticYear, currentMonthCode), then
        if (year_contains_month_code(calendar, arithmetic_year, current_month_code)) {
            // i. Set monthsBefore to monthsBefore + 1.
            ++months_before;
        }

        // c. If currentMonthCode is monthCode, then
        if (current_month_code == month_code) {
            // i. Return monthsBefore.
            return months_before;
        }

        // d. If isLeap is false, then
        if (!is_leap) {
            // i. Set isLeap to true.
            is_leap = true;
        }
        // e. Else,
        else {
            // i. Set isLeap to false.
            is_leap = false;

            // ii. Set number to number + 1.
            ++number;
        }
    }

    VERIFY_NOT_REACHED();
}

// 4.1.8 CalendarDaysInMonth ( calendar, arithmeticYear, ordinalMonth ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendardaysinmonth
u8 calendar_days_in_month(StringView calendar, i32 arithmetic_year, i8 ordinal_month)
{
    // 1. Return the number of days in the month of calendar corresponding to arithmeticYear and ordinalMonth.
    return Unicode::calendar_days_in_month(calendar, arithmetic_year, ordinal_month - 1);
}

// 4.1.9 CalendarDateEra ( calendar, date ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendardateera
Optional<StringView> calendar_date_era(StringView calendar, ISODate date)
{
    // 1. If CalendarSupportsEra(calendar) is false, return undefined.
    if (!calendar_supports_era(calendar))
        return {};

    // 2. Let era be the String to indicate the era corresponding to date in the context of the calendar represented by
    //    calendar according to implementation-defined processing.
    auto era = Unicode::calendar_era(calendar, to_unicode_iso_date(date));
    if (!era.has_value())
        return {};

    // 3. Return CanonicalizeEraInCalendar(calendar, era).
    return canonicalize_era_in_calendar(calendar, *era);
}

// 4.1.10 CalendarDateEraYear ( calendar, date ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendardateerayear
Optional<i32> calendar_date_era_year(StringView calendar, ISODate date)
{
    // 1. If CalendarSupportsEra(calendar) is false, return undefined.
    if (!calendar_supports_era(calendar))
        return {};

    // 2. Let eraYear be the integer to indicate the era year corresponding to date in the context of the calendar
    //    represented by calendar according to implementation-defined processing.
    // 3. Assert: eraYear is an integer.
    // 4. Return eraYear.
    return Unicode::calendar_era_year(calendar, to_unicode_iso_date(date));
}

// 4.1.11 CalendarDateArithmeticYear ( calendar, date ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendardatearithmeticyear
i32 calendar_date_arithmetic_year(StringView calendar, ISODate date)
{
    // 1. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return date.year;

    // 2. Let r be the row in Table 4 with a value in the Calendar column matching calendar.
    // 3. Let epochYear be the value given in the "Epoch ISO Year" column of r.
    // 4. Let epochDate be the first day of the calendar year starting in ISO year epochYear in the calendar represented
    //    by calendar, according to implementation-defined processing.
    // 5. Let newYear be the first day of the calendar year of date in the calendar represented by calendar, according
    //     to implementation-defined processing.
    // 6. Let arithmeticYear be the number of whole years between epochDate and newYear in the calendar represented by
    //    calendar, according to implementation-defined processing.
    // 7. Return arithmeticYear.
    return Unicode::calendar_year(calendar, to_unicode_iso_date(date)).value_or(date.year);
}

// 4.1.12 CalendarDateArithmeticYearForEraYear ( calendar, era, eraYear ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendardatearithmeticyearforerayear
i32 calendar_date_arithmetic_year_for_era_year(StringView calendar, StringView era, i32 era_year)
{
    // 1. Let era be CanonicalizeEraInCalendar(calendar, era).
    // 2. Assert: era is not undefined.
    era = canonicalize_era_in_calendar(calendar, era).release_value();

    // 3. If calendar is not listed in the "Calendar Type" column of Table 1, return an implementation-defined value.
    if (!CLDR_CALENDAR_TYPES.contains_slow(calendar))
        return era_year;

    // 4. Let r be the row in Table 2 with a value in the Calendar column matching calendar and a value in the Era
    //    column matching era.
    auto row = find_value(CALENDAR_ERA_DATA, [&](auto const& row) { return row.calendar == calendar && row.era == era; });
    VERIFY(row.has_value());

    // 5. Let eraKind be the value given in the "Era Kind" column of r.
    auto era_kind = row->kind;

    // 6. Let offset be the value given in the "Offset" column of r.
    auto offset = row->offset;

    // 7. If eraKind is EPOCH, return eraYear.
    if (era_kind == CalendarEraData::Kind::Epoch)
        return era_year;

    // 8. If eraKind is NEGATIVE, return 1 - eraYear.
    if (era_kind == CalendarEraData::Kind::Negative)
        return 1 - era_year;

    // 9. Assert: eraKind is OFFSET.
    VERIFY(era_kind == CalendarEraData::Kind::Offset);

    // 10. Assert: offset is not undefined.
    VERIFY(offset.has_value());

    // 11. Return offset + eraYear - 1.
    return *offset + era_year - 1;
}

// 4.1.13 CalendarIntegersToISO ( calendar, arithmeticYear, ordinalMonth, day ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendarintegerstoiso
ThrowCompletionOr<ISODate> calendar_integers_to_iso(VM& vm, StringView calendar, i32 arithmetic_year, i8 ordinal_month, u8 day)
{
    // 1. If arithmeticYear, ordinalMonth, and day do not form a valid date in calendar, throw a RangeError exception.
    // 2. Let isoDate be an ISO Date Record such that CalendarISOToDate(calendar, isoDate) returns a Calendar Date Record
    //    whose [[Year]], [[Month]], and [[Day]] field values respectively equal arithmeticYear, ordinalMonth, and day.
    auto iso_date = Unicode::calendar_date_to_iso_date(calendar, { .year = arithmetic_year, .month = ordinal_month - 1, .day = day }, Unicode::Lenient::No);
    if (!iso_date.has_value())
        return vm.throw_completion<RangeError>(ErrorType::TemporalInvalidISODate);

    // 3. Return isoDate.
    return from_unicode_iso_date(iso_date.value());
}

// 4.1.15 CalendarMonthsInYear ( calendar, arithmeticYear ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-calendarmonthsinyear
u8 calendar_months_in_year(StringView calendar, i32 arithmetic_year)
{
    // 1. Return the number of months in arithmetic year arithmeticYear of calendar calendar.
    return Unicode::calendar_months_in_year(calendar, arithmetic_year);
}

// 4.1.16 BalanceNonISODate ( calendar, arithmeticYear, ordinalMonth, day )
NonISODate balance_non_iso_date(StringView calendar, i32 arithmetic_year, i32 ordinal_month, i32 day)
{
    // 1. Let resolvedYear be arithmeticYear.
    auto resolved_year = arithmetic_year;

    // 2. Let resolvedMonth be ordinalMonth.
    auto resolved_month = ordinal_month;

    // 3. Let monthsInYear be CalendarMonthsInYear(calendar, resolvedYear).
    auto months_in_year = calendar_months_in_year(calendar, resolved_year);

    // 4. Repeat, while resolvedMonth ≤ 0,
    while (resolved_month <= 0) {
        // a. Set resolvedYear to resolvedYear - 1.
        --resolved_year;

        // b. Set monthsInYear to CalendarMonthsInYear(calendar, resolvedYear).
        months_in_year = calendar_months_in_year(calendar, resolved_year);

        // c. Set resolvedMonth to resolvedMonth + monthsInYear.
        resolved_month += months_in_year;
    }

    // 5. Repeat, while resolvedMonth > monthsInYear,
    while (resolved_month > months_in_year) {
        // a. Set resolvedMonth to resolvedMonth - monthsInYear.
        resolved_month -= months_in_year;

        // b. Set resolvedYear to resolvedYear + 1.
        ++resolved_year;

        // c. Set monthsInYear to CalendarMonthsInYear(calendar, resolvedYear).
        months_in_year = calendar_months_in_year(calendar, resolved_year);
    }

    // 6. Let resolvedDay be day.
    auto resolved_day = day;

    // 7. Let daysInMonth be CalendarDaysInMonth(calendar, resolvedYear, resolvedMonth).
    auto days_in_month = calendar_days_in_month(calendar, resolved_year, resolved_month);

    // 8. Repeat, while resolvedDay ≤ 0,
    while (resolved_day <= 0) {
        // a. Set resolvedMonth to resolvedMonth - 1.
        --resolved_month;

        // b. If resolvedMonth is 0, then
        if (resolved_month == 0) {
            // i. Set resolvedYear to resolvedYear - 1.
            --resolved_year;

            // ii. Set monthsInYear to CalendarMonthsInYear(calendar, resolvedYear).
            months_in_year = calendar_months_in_year(calendar, resolved_year);

            // iii. Set resolvedMonth to monthsInYear.
            resolved_month = months_in_year;
        }

        // c. Set daysInMonth to CalendarDaysInMonth(calendar, resolvedYear, resolvedMonth).
        days_in_month = calendar_days_in_month(calendar, resolved_year, resolved_month);

        // d. Set resolvedDay to resolvedDay + daysInMonth.
        resolved_day += days_in_month;
    }

    // 9. Repeat, while resolvedDay > daysInMonth,
    while (resolved_day > days_in_month) {
        // a. Set resolvedDay to resolvedDay - daysInMonth.
        resolved_day -= days_in_month;

        // b. Set resolvedMonth to resolvedMonth + 1.
        ++resolved_month;

        // c. If resolvedMonth > monthsInYear, then
        if (resolved_month > months_in_year) {
            // i. Set resolvedYear to resolvedYear + 1.
            ++resolved_year;

            // ii. Set monthsInYear to CalendarMonthsInYear(calendar, resolvedYear).
            months_in_year = calendar_months_in_year(calendar, resolved_year);

            // iii. Set resolvedMonth to 1.
            resolved_month = 1;
        }

        // d. Set daysInMonth to CalendarDaysInMonth(calendar, resolvedYear, resolvedMonth).
        days_in_month = Unicode::calendar_days_in_month(calendar, resolved_year, resolved_month);
    }

    // 10. Return the Record { [[Year]]: resolvedYear, [[Month]]: resolvedMonth, [[Day]]: resolvedDay }.
    return { .year = resolved_year, .month = static_cast<u8>(resolved_month), .day = static_cast<u8>(resolved_day) };
}

// 4.1.17 NonISODateSurpasses ( calendar, sign, fromIsoDate, toIsoDate, years, months, weeks, days ), https://tc39.es/proposal-intl-era-monthcode/#sec-temporal-nonisodatesurpasses
bool non_iso_date_surpasses(VM& vm, StringView calendar, i8 sign, ISODate from_iso_date, ISODate to_iso_date, i32 years, i32 months, i32 weeks, i32 days)
{
    // 1. Let parts be CalendarISOToDate(calendar, fromIsoDate).
    auto parts = calendar_iso_to_date(calendar, from_iso_date);

    // 2. Let y0 be parts.[[Year]] + years.
    auto y0 = parts.year + years;

    // 3. Let m0 be MonthCodeToOrdinal(calendar, y0, ! ConstrainMonthCode(calendar, y0, parts.[[MonthCode]], CONSTRAIN)).
    auto m0 = month_code_to_ordinal(vm, calendar, y0, MUST(constrain_month_code(vm, calendar, y0, parts.month_code, Overflow::Constrain)));

    // 4. Let endOfMonth be BalanceNonISODate(calendar, y0, m0 + months + 1, 0).
    auto end_of_month = balance_non_iso_date(calendar, y0, m0 + months + 1, 0);

    // 5. Let baseDay be parts.[[Day]].
    auto base_day = parts.day;

    i32 y1 = 0;
    i8 m1 = 0;
    i8 d1 = 0;

    // 6. If weeks is not 0 or days is not 0, then
    if (weeks != 0 || days != 0) {
        // a. If baseDay < endOfMonth.[[Day]], then
        //     i. Let regulatedDay be baseDay.
        // b. Else,
        //     i. Let regulatedDay be endOfMonth.[[Day]].
        auto regulated_day = base_day < end_of_month.day ? base_day : end_of_month.day;

        // c. Let daysInWeek be 7 (the number of days in a week for all supported calendars).
        static constexpr auto days_in_week = 7;

        // d. Let balancedDate be BalanceNonISODate(calendar, endOfMonth.[[Year]], endOfMonth.[[Month]], regulatedDay + daysInWeek * weeks + days).
        auto balanced_date = balance_non_iso_date(calendar, end_of_month.year, end_of_month.month, regulated_day + (days_in_week * weeks) + days);

        // e. Let y1 be balancedDate.[[Year]].
        y1 = balanced_date.year;

        // f. Let m1 be balancedDate.[[Month]].
        m1 = balanced_date.month;

        // g. Let d1 be balancedDate.[[Day]].
        d1 = balanced_date.day;
    }
    // 7. Else,
    else {
        // a. Let y1 be endOfMonth.[[Year]].
        y1 = end_of_month.year;

        // b. Let m1 be endOfMonth.[[Month]].
        m1 = end_of_month.month;

        // c. Let d1 be baseDay.
        d1 = base_day;
    }

    // 8. Let calDate2 be CalendarISOToDate(calendar, toIsoDate).
    auto calendar_date2 = calendar_iso_to_date(calendar, to_iso_date);

    // 9. If y1 ≠ calDate2.[[Year]], then
    if (y1 != calendar_date2.year) {
        // a. If sign × (y1 - calDate2.[[Year]]) > 0, return true.
        if (sign * (y1 - calendar_date2.year) > 0)
            return true;
    }
    // 10. Else if m1 ≠ calDate2.[[Month]], then
    else if (m1 != calendar_date2.month) {
        // a. If sign × (m1 - calDate2.[[Month]]) > 0, return true.
        if (sign * (m1 - calendar_date2.month) > 0)
            return true;
    }
    // 11. Else if d1 ≠ calDate2.[[Day]], then
    else if (d1 != calendar_date2.day) {
        // a. If sign × (d1 - calDate2.[[Day]]) > 0, return true.
        if (sign * (d1 - calendar_date2.day) > 0)
            return true;
    }

    // 12. Return false.
    return false;
}

}
