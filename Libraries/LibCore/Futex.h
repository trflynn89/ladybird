/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/Time.h>
#include <AK/Types.h>
#include <LibCore/Export.h>

namespace Core {

enum class AtomicWaitResult : u8 {
    Woken,
    NotEqual,
    TimedOut,
};

// Compare and enqueue under the same lock used by atomic_notify. Waiters of either size (4 or 8 bytes) at the
// same address form one FIFO list. Value changes alone do not wake a waiter. These functions coordinate threads
// within this process; a host with agents in other processes must route their operations to one registry.
CORE_API AtomicWaitResult atomic_wait(void* address, u64 expected, size_t size, Optional<AK::Duration> timeout);

// Select up to max_count waiters in FIFO order and return the number selected. A selected waiter is removed before
// it resumes, so repeated notifications never count it twice. A zero count selects no waiters.
CORE_API size_t atomic_notify(void* address, size_t size, size_t max_count);

// Rust bindings for the shared registry. A negative timeout means no deadline. A non-null cancellation callback
// must be nonblocking and is polled while waiting; cancellation removes the waiter and returns TimedOut.
// A nonzero shared object ID and byte offset identify one list across different mappings of the same memory.
// Without an object ID, the address identifies the list (including JavaScript views of WebAssembly memories).
extern "C" {
CORE_API u8 core_atomic_wait(void*, u64 expected, size_t size, i64 timeout_ns, bool (*cancelled)(void*), void* context, u64 object_id = 0, size_t byte_index = 0);
CORE_API size_t core_atomic_notify(void*, size_t max_count, u64 object_id = 0, size_t byte_index = 0);
}

}
