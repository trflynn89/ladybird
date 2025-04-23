/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullOwnPtr.h>

namespace JS {

// https://tc39.es/ecma262/#mathematical-value
class MathematicalValue {
public:
    explicit MathematicalValue();
    explicit MathematicalValue(double);
    ~MathematicalValue();

    MathematicalValue(MathematicalValue const&);
    MathematicalValue(MathematicalValue&&);

    MathematicalValue& operator=(MathematicalValue const&);
    MathematicalValue& operator=(MathematicalValue&&);

    [[nodiscard]] double to_double() const;

    [[nodiscard]] MathematicalValue add(double) const;
    [[nodiscard]] MathematicalValue add(MathematicalValue const&) const;

    [[nodiscard]] MathematicalValue subtract(double) const;
    [[nodiscard]] MathematicalValue subtract(MathematicalValue const&) const;

    [[nodiscard]] MathematicalValue multiply(double) const;
    [[nodiscard]] MathematicalValue multiply(MathematicalValue const&) const;

    [[nodiscard]] MathematicalValue divide(double) const;
    [[nodiscard]] MathematicalValue divide(MathematicalValue const&) const;

private:
    struct Value;
    NonnullOwnPtr<Value> m_value;
};

}
