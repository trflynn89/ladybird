/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ConditionVariable.h>
#include <AK/HashMap.h>
#include <AK/Mutex.h>
#include <AK/NeverDestroyed.h>
#include <AK/Vector.h>
#include <LibCore/Futex.h>

namespace Core {

namespace {

enum class WaiterState { Waiting,
    Notified,
    Cancelled };

struct Waiter {
    ConditionVariable condition;
    bool (*cancelled)(void*);
    void* context;
    WaiterState state { WaiterState::Waiting };
};

struct Location {
    u64 object_id;
    FlatPtr offset;
    bool operator==(Location const&) const = default;
};

struct LocationTraits : public AK::DefaultTraits<Location> {
    static unsigned hash(Location const& key) { return pair_int_hash(u64_hash(key.object_id), ptr_hash(key.offset)); }
};

Location location(void* address, u64 object_id = 0, size_t byte_index = 0)
{
    return { object_id, object_id ? byte_index : reinterpret_cast<FlatPtr>(address) };
}

// One registry in LibCore, including when JavaScript and WebAssembly live in different dynamic libraries. The
// platform condition variable only parks threads; comparison, FIFO selection, counting and timeout removal all
// happen under this lock. Waiters live on their waiting thread's stack and need no shared-memory metadata.
struct WaiterLists {
    Mutex lock;
    HashMap<Location, Vector<Waiter*>, LocationTraits> lists;
};

WaiterLists& waiter_lists()
{
    static NeverDestroyed<WaiterLists> lists;
    return *lists;
}

bool word_equals(void* address, u64 expected, size_t size)
{
    if (size == 8)
        return __atomic_load_n(reinterpret_cast<u64*>(address), __ATOMIC_SEQ_CST) == expected;
    VERIFY(size == 4);
    return __atomic_load_n(reinterpret_cast<u32*>(address), __ATOMIC_SEQ_CST) == static_cast<u32>(expected);
}

AtomicWaitResult wait(void* address, u64 expected, size_t size, Optional<AK::Duration> timeout, bool (*cancelled)(void*), void* context, Location const& key)
{
    auto& lists = waiter_lists();
    MutexLocker locker { lists.lock };
    if (!word_equals(address, expected, size))
        return AtomicWaitResult::NotEqual;
    if (timeout.has_value() && *timeout <= AK::Duration::zero())
        return AtomicWaitResult::TimedOut;

    Optional<MonotonicTime> deadline;
    if (timeout.has_value())
        deadline = MonotonicTime::now() + *timeout;

    Waiter waiter { ConditionVariable { lists.lock }, cancelled, context };
    lists.lists.ensure(key).append(&waiter);
    while (waiter.state == WaiterState::Waiting) {
        Optional<AK::Duration> remaining;
        if (deadline.has_value()) {
            remaining = *deadline - MonotonicTime::now();
            if (*remaining <= AK::Duration::zero())
                break;
        }
        // A host coordinating agents in other processes can cancel an abandoned wait when its connection closes.
        // The callback must not block or reenter the registry. Cancellation removes the waiter like a timeout.
        if (cancelled) {
            if (cancelled(context))
                break;
            auto interval = AK::Duration::from_milliseconds(10);
            remaining = remaining.has_value() ? min(*remaining, interval) : interval;
        }
        if (remaining.has_value())
            waiter.condition.wait_for(*remaining);
        else
            waiter.condition.wait();
    }
    if (waiter.state == WaiterState::Notified)
        return AtomicWaitResult::Woken;

    if (waiter.state == WaiterState::Waiting) {
        auto& list = lists.lists.find(key)->value;
        list.remove_first_matching([&](auto* entry) { return entry == &waiter; });
        if (list.is_empty())
            lists.lists.remove(key);
    }
    return AtomicWaitResult::TimedOut;
}

}

AtomicWaitResult atomic_wait(void* address, u64 expected, size_t size, Optional<AK::Duration> timeout)
{
    return wait(address, expected, size, timeout, nullptr, nullptr, location(address));
}

static size_t notify(Location const& key, size_t max_count)
{
    auto& lists = waiter_lists();
    MutexLocker locker { lists.lock };
    auto entry = lists.lists.find(key);
    if (entry == lists.lists.end())
        return 0;
    auto& list = entry->value;
    size_t count = 0;
    size_t removed = 0;
    while (count < max_count && removed < list.size()) {
        auto* waiter = list[removed++];
        // A disconnected agent must not consume a notification while its service thread is still waking up.
        if (waiter->cancelled && waiter->cancelled(waiter->context)) {
            waiter->state = WaiterState::Cancelled;
        } else {
            waiter->state = WaiterState::Notified;
            ++count;
        }
        waiter->condition.signal();
    }
    list.remove(0, removed);
    if (list.is_empty())
        lists.lists.remove(entry);
    return count;
}

size_t atomic_notify(void* address, size_t size, size_t max_count)
{
    VERIFY(size == 4 || size == 8);
    return notify(location(address), max_count);
}

extern "C" CORE_API u8 core_atomic_wait(void* address, u64 expected, size_t size, i64 timeout_ns, bool (*cancelled)(void*), void* context, u64 object_id, size_t byte_index)
{
    Optional<AK::Duration> timeout;
    if (timeout_ns >= 0)
        timeout = AK::Duration::from_nanoseconds(timeout_ns);
    return to_underlying(wait(address, expected, size, timeout, cancelled, context, location(address, object_id, byte_index)));
}

extern "C" CORE_API size_t core_atomic_notify(void* address, size_t max_count, u64 object_id, size_t byte_index)
{
    return notify(location(address, object_id, byte_index), max_count);
}

}
