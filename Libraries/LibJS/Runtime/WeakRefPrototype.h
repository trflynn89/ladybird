/*
 * Copyright (c) 2021, Idan Horowitz <idan.horowitz@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibJS/Runtime/PrototypeObject.h>
#include <LibJS/Runtime/WeakRef.h>

namespace JS {

class JS_API WeakRefPrototype final : public PrototypeObject<WeakRefPrototype, WeakRef> {
    JS_PROTOTYPE_OBJECT(WeakRefPrototype, WeakRef, WeakRef);
    GC_DECLARE_ALLOCATOR(WeakRefPrototype);

public:
    virtual void initialize(Realm&) override;
    virtual ~WeakRefPrototype() override = default;

private:
    explicit WeakRefPrototype(Realm&);

    JS_DECLARE_NATIVE_FUNCTION(deref);
};

}
