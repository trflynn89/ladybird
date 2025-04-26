/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ByteString.h>
#include <AK/LexicalPath.h>
#include <AK/QuickSort.h>
#include <AK/SourceGenerator.h>
#include <AK/String.h>
#include <AK/StringBuilder.h>
#include <LibCore/ArgsParser.h>
#include <LibCore/File.h>
#include <LibMain/Main.h>

struct Property {
    String name;
    String value;
};

static ErrorOr<void> generate_header(LexicalPath output_path, ReadonlySpan<Property> properties)
{
    StringBuilder builder;
    SourceGenerator generator { builder };

    generator.append(R"~~~(
#pragma once

#include <LibJS/Runtime/PropertyKey.h>

namespace Web::Bindings::PropertyNames {
)~~~");

    for (auto const& property : properties) {
        generator.set("name", property.name);

        generator.append(R"~~~(
extern JS::PropertyKey const @name@;)~~~");
    }

    generator.append(R"~~~(

}
)~~~");

    auto output_file = TRY(Core::File::open(output_path.string(), Core::File::OpenMode::Write));
    TRY(output_file->write_until_depleted(builder.string_view()));

    return {};
}

static ErrorOr<void> generate_implementation(LexicalPath output_path, ReadonlySpan<Property> properties)
{
    StringBuilder builder;
    SourceGenerator generator { builder };

    generator.append(R"~~~(
#include <LibWeb/Bindings/PropertyNames.h>

namespace Web::Bindings::PropertyNames {
)~~~");

    for (auto const& property : properties) {
        generator.set("name", property.name);
        generator.set("value", property.value);

        generator.append(R"~~~(
JS::PropertyKey const @name@ { "@value@"_fly_string };)~~~");
    }

    generator.append(R"~~~(

}
)~~~");

    auto output_file = TRY(Core::File::open(output_path.string(), Core::File::OpenMode::Write));
    TRY(output_file->write_until_depleted(builder.string_view()));

    return {};
}

ErrorOr<int> serenity_main(Main::Arguments arguments)
{
    StringView output_path;
    Vector<ByteString> paths;

    Core::ArgsParser args_parser;
    args_parser.add_option(output_path, "Path to output generated files into", "output-path", 'o', "output-path");
    args_parser.add_positional_argument(paths, "Paths of every properties file", "paths");
    args_parser.parse(arguments);

    VERIFY(!paths.is_empty());

    Vector<Property> properties;
    HashTable<String> seen_properties;

    for (auto const& path : paths) {
        auto file_or_error = Core::File::open(path, Core::File::OpenMode::Read);
        if (file_or_error.is_error())
            continue;

        auto contents = TRY(file_or_error.value()->read_until_eof());

        StringView { contents }.for_each_split_view('\n', SplitBehavior::Nothing, [&](auto line) {
            auto name_and_value = line.split_view("||"sv);
            VERIFY(name_and_value.size() == 2);

            auto name = MUST(String::from_utf8(name_and_value[0].trim_whitespace()));
            auto value = MUST(String::from_utf8(name_and_value[1].trim_whitespace()));

            if (seen_properties.set(name) == HashSetResult::InsertedNewEntry)
                properties.empend(move(name), move(value));
        });
    }

    // Sort the properties to ensure we don't cause recompilation due to differently-ordered properties.
    quick_sort(properties, [](auto const& lhs, auto const& rhs) {
        return lhs.name < rhs.name;
    });

    TRY(generate_header(LexicalPath::join(output_path, "PropertyNames.h"sv), properties));
    TRY(generate_implementation(LexicalPath::join(output_path, "PropertyNames.cpp"sv), properties));

    return 0;
}
