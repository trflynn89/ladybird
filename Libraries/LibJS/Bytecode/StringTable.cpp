/*
 * Copyright (c) 2021, Gunnar Beutner <gbeutner@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibJS/Bytecode/StringTable.h>

namespace JS::Bytecode {

StringTableIndex StringTable::insert(String string)
{
    m_strings.append(move(string));
    return { static_cast<u32>(m_strings.size() - 1) };
}

String const& StringTable::get(StringTableIndex index) const
{
    return m_strings[index.value];
}

void StringTable::dump() const
{
    outln("String Table:");
    for (size_t i = 0; i < m_strings.size(); i++)
        outln("{}: {}", i, m_strings[i]);
}

}
