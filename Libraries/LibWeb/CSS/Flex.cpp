/*
 * Copyright (c) 2023-2025, Sam Atkins <sam@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWeb/CSS/Flex.h>
#include <LibWeb/CSS/Percentage.h>

namespace Web::CSS {

Flex::Flex(double value, Type type)
    : m_type(type)
    , m_value(value)
{
}

Flex Flex::make_fr(double value)
{
    return { value, Type::Fr };
}

Flex Flex::percentage_of(Percentage const& percentage) const
{
    return Flex { percentage.as_fraction() * m_value, m_type };
}

String Flex::to_string(SerializationMode serialization_mode) const
{
    if (serialization_mode == SerializationMode::ResolvedValue)
        return MUST(String::formatted("{}fr", to_fr()));
    return MUST(String::formatted("{}{}", raw_value(), unit_name()));
}

double Flex::to_fr() const
{
    switch (m_type) {
    case Type::Fr:
        return m_value;
    }
    VERIFY_NOT_REACHED();
}

StringView Flex::unit_name() const
{
    switch (m_type) {
    case Type::Fr:
        return "fr"sv;
    }
    VERIFY_NOT_REACHED();
}

Optional<Flex::Type> Flex::unit_from_name(StringView name)
{
    if (name.equals_ignoring_ascii_case("fr"sv))
        return Type::Fr;

    return {};
}

}
