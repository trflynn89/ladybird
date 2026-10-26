/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Array.h>
#include <AK/Atomic.h>
#include <AK/Function.h>
#include <AK/NumericLimits.h>
#include <AK/Time.h>
#include <AK/Vector.h>
#include <LibCore/AnonymousBuffer.h>
#include <LibCore/Futex.h>
#include <LibCore/System.h>
#include <LibTest/TestCase.h>
#include <LibThreading/Thread.h>

// Use the same backing memory as a SharedArrayBuffer. All wait/notify semantics are provided by the common
// registry; the memory value is never used as a substitute for an explicit notification.
static Core::AnonymousBuffer make_word_buffer()
{
    return MUST(Core::AnonymousBuffer::create_with_size(sizeof(u64)));
}

TEST_CASE(wait_returns_not_equal_on_mismatch)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u32>();
    *word = 42;

    // A mismatched value returns "not-equal" immediately, even without a timeout.
    EXPECT(Core::atomic_wait(word, 99, sizeof(u32), {}) == Core::AtomicWaitResult::NotEqual);
}

TEST_CASE(wait_not_equal_beats_zero_timeout)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u32>();
    *word = 42;

    // With a zero (or sub-tick) timeout and a mismatched value, "not-equal" must win over "timed-out".
    EXPECT(Core::atomic_wait(word, 99, sizeof(u32), AK::Duration::from_milliseconds(0)) == Core::AtomicWaitResult::NotEqual);
}

TEST_CASE(wait_not_equal_with_finite_timeout)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u32>();
    *word = 42;

    EXPECT(Core::atomic_wait(word, 99, sizeof(u32), AK::Duration::from_milliseconds(50)) == Core::AtomicWaitResult::NotEqual);
}

TEST_CASE(wait_times_out_when_value_matches)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u32>();
    *word = 42;

    auto start = MonotonicTime::now();
    // The value matches and nothing notifies — so the wait must run to the timeout.
    EXPECT(Core::atomic_wait(word, 42, sizeof(u32), AK::Duration::from_milliseconds(50)) == Core::AtomicWaitResult::TimedOut);
    // And it must actually have waited, not returned instantly.
    EXPECT((MonotonicTime::now() - start) >= AK::Duration::from_milliseconds(20));
}

TEST_CASE(wait_zero_timeout_when_value_matches)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u32>();
    *word = 42;

    // The value matches but the timeout is zero — so it times out immediately.
    EXPECT(Core::atomic_wait(word, 42, sizeof(u32), AK::Duration::from_milliseconds(0)) == Core::AtomicWaitResult::TimedOut);
}

TEST_CASE(wait_eight_byte_not_equal_in_high_half)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u64>();
    *word = 0x0000000100000042ULL; // low half 0x42, high half 1

    // The low 32 bits match, but the high 32 differ; a full 64-bit compare must report "not-equal".
    EXPECT(Core::atomic_wait(word, 0x0000000200000042ULL, sizeof(u64), {}) == Core::AtomicWaitResult::NotEqual);
}

TEST_CASE(wait_eight_byte_times_out_when_value_matches)
{
    auto buffer = make_word_buffer();
    auto* word = buffer.data<u64>();
    *word = 0x0000000100000042ULL;

    EXPECT(Core::atomic_wait(word, 0x0000000100000042ULL, sizeof(u64), AK::Duration::from_milliseconds(50)) == Core::AtomicWaitResult::TimedOut);
}

TEST_CASE(notify_wakes_a_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u32>();
    *word = 42;

    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<bool> waiter_ready { false };
    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<int> result { -1 };

    auto thread = Threading::Thread::construct("FutexWaiter"sv, [&]() {
        waiter_ready.store(true);
        // The value stays 42 — so this blocks until notified; a generous timeout keeps a slow scheduler from timing us
        // out during the retry loop below.
        auto wait_result = Core::atomic_wait(word, 42, sizeof(u32), AK::Duration::from_milliseconds(5000));
        result.store(static_cast<int>(wait_result));
        return 0;
    });
    thread->start();

    while (!waiter_ready.load())
        (void)Core::System::sleep_ms(1);

    // Retry to cover the window between the ready flag and the thread actually entering the kernel wait.
    for (int i = 0; i < 200 && result.load() == -1; ++i) {
        (void)Core::System::sleep_ms(5);
        Core::atomic_notify(word, sizeof(u32), 1);
    }

    (void)thread->join();
    EXPECT(result.load() == static_cast<int>(Core::AtomicWaitResult::Woken));
}

TEST_CASE(notify_wakes_an_eight_byte_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 0x0000000100000042ULL;

    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<bool> waiter_ready { false };
    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<int> result { -1 };

    auto thread = Threading::Thread::construct("FutexWaiter64"sv, [&]() {
        waiter_ready.store(true);
        // Only an explicit notification can wake this waiter; the value stays unchanged.
        auto wait_result = Core::atomic_wait(word, 0x0000000100000042ULL, sizeof(u64), AK::Duration::from_milliseconds(5000));
        result.store(static_cast<int>(wait_result));
        return 0;
    });
    thread->start();

    while (!waiter_ready.load())
        (void)Core::System::sleep_ms(1);

    size_t max_woken = 0;
    for (int i = 0; i < 200 && result.load() == -1; ++i) {
        (void)Core::System::sleep_ms(5);
        auto woken = Core::atomic_notify(word, sizeof(u64), 1);
        if (woken > max_woken)
            max_woken = woken;
    }

    (void)thread->join();
    EXPECT(result.load() == static_cast<int>(Core::AtomicWaitResult::Woken));
    // An 8-byte notify reports the selected waiter in its count.
    EXPECT(max_woken == 1u);
}

TEST_CASE(notify_from_a_four_byte_view_wakes_an_eight_byte_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 0x0000000100000042ULL;

    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<bool> waiter_ready { false };
    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<int> result { -1 };

    // Both widths at one address share a waiter list.
    auto thread = Threading::Thread::construct("FutexWaiterMixed64"sv, [&]() {
        waiter_ready.store(true);
        auto wait_result = Core::atomic_wait(word, 0x0000000100000042ULL, sizeof(u64), AK::Duration::from_milliseconds(5000));
        result.store(static_cast<int>(wait_result));
        return 0;
    });
    thread->start();

    while (!waiter_ready.load())
        (void)Core::System::sleep_ms(1);

    for (int i = 0; i < 200 && result.load() == -1; ++i) {
        (void)Core::System::sleep_ms(5);
        Core::atomic_notify(word, sizeof(u32), 1);
    }

    (void)thread->join();
    EXPECT(result.load() == static_cast<int>(Core::AtomicWaitResult::Woken));
}

TEST_CASE(notify_from_an_eight_byte_view_wakes_a_four_byte_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 0x0000000100000042ULL;

    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<bool> waiter_ready { false };
    IGNORE_USE_IN_ESCAPING_LAMBDA AK::Atomic<int> result { -1 };

    // The mirror of the above: A 4-byte waiter (on the low half, value 0x42) must be woken by an 8-byte notify.
    auto thread = Threading::Thread::construct("FutexWaiterMixed32"sv, [&]() {
        waiter_ready.store(true);
        auto wait_result = Core::atomic_wait(word, 0x42, sizeof(u32), AK::Duration::from_milliseconds(5000));
        result.store(static_cast<int>(wait_result));
        return 0;
    });
    thread->start();

    while (!waiter_ready.load())
        (void)Core::System::sleep_ms(1);

    for (int i = 0; i < 200 && result.load() == -1; ++i) {
        (void)Core::System::sleep_ms(5);
        Core::atomic_notify(word, sizeof(u64), 1);
    }

    (void)thread->join();
    EXPECT(result.load() == static_cast<int>(Core::AtomicWaitResult::Woken));
}

namespace {

struct WaiterContext {
    Atomic<bool> ready { false };
    Atomic<bool> cancelled { false };
};

bool observe_waiter(void* context)
{
    auto& waiter = *static_cast<WaiterContext*>(context);
    // The host cancellation callback runs after the waiter is enqueued, while the list is locked. This gives the
    // tests a deterministic enrollment barrier without sleeps that guess when a thread entered the kernel.
    waiter.ready.store(true);
    return waiter.cancelled.load();
}

bool wait_until(Function<bool()> const& condition)
{
    auto deadline = MonotonicTime::now() + AK::Duration::from_seconds(2);
    while (!condition()) {
        if (MonotonicTime::now() >= deadline)
            return false;
        (void)Core::System::sleep_ms(1);
    }
    return true;
}

}

TEST_CASE(notify_counts_each_waiter_once)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 42;
    IGNORE_USE_IN_ESCAPING_LAMBDA Array<WaiterContext, 3> contexts;
    IGNORE_USE_IN_ESCAPING_LAMBDA Array<Atomic<u8>, 3> results;
    Vector<NonnullRefPtr<Threading::Thread>> threads;
    for (size_t i = 0; i < contexts.size(); ++i) {
        auto thread = Threading::Thread::construct("CountWaiter"sv, [&, i] {
            results[i].store(Core::core_atomic_wait(word, 42, i == 1 ? 8 : 4, 5'000'000'000, observe_waiter, &contexts[i]));
            return 0;
        });
        thread->start();
        threads.append(move(thread));
        EXPECT(wait_until([&] { return contexts[i].ready.load(); }));
    }
    EXPECT_EQ(Core::atomic_notify(word, 4, 0), 0u);
    EXPECT_EQ(Core::atomic_notify(word, 4, 1), 1u);
    EXPECT_EQ(Core::atomic_notify(word, 8, NumericLimits<size_t>::max()), 2u);
    EXPECT_EQ(Core::atomic_notify(word, 4, NumericLimits<size_t>::max()), 0u);
    for (size_t i = 0; i < threads.size(); ++i) {
        (void)threads[i]->join();
        EXPECT_EQ(results[i].load(), to_underlying(Core::AtomicWaitResult::Woken));
    }
}

TEST_CASE(notify_selects_waiters_in_fifo_order)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 42;
    IGNORE_USE_IN_ESCAPING_LAMBDA Array<WaiterContext, 3> contexts;
    IGNORE_USE_IN_ESCAPING_LAMBDA Array<Atomic<int>, 3> results;
    for (auto& result : results)
        result.store(-1);
    Vector<NonnullRefPtr<Threading::Thread>> threads;
    for (size_t i = 0; i < contexts.size(); ++i) {
        auto thread = Threading::Thread::construct("FIFOWaiter"sv, [&, i] {
            results[i].store(Core::core_atomic_wait(word, 42, i == 1 ? 8 : 4, 5'000'000'000, observe_waiter, &contexts[i]));
            return 0;
        });
        thread->start();
        threads.append(move(thread));
        EXPECT(wait_until([&] { return contexts[i].ready.load(); }));
    }
    for (size_t i = 0; i < contexts.size(); ++i) {
        EXPECT_EQ(Core::atomic_notify(word, 8, 1), 1u);
        EXPECT(wait_until([&] { return results[i].load() != -1; }));
        EXPECT_EQ(results[i].load(), static_cast<int>(Core::AtomicWaitResult::Woken));
        for (size_t j = i + 1; j < contexts.size(); ++j)
            EXPECT_EQ(results[j].load(), -1);
    }
    for (auto& thread : threads)
        (void)thread->join();
}

TEST_CASE(changing_the_word_does_not_notify_a_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 42;
    IGNORE_USE_IN_ESCAPING_LAMBDA WaiterContext context;
    IGNORE_USE_IN_ESCAPING_LAMBDA Atomic<u8> result;
    auto thread = Threading::Thread::construct("StoreWaiter"sv, [&] {
        result.store(Core::core_atomic_wait(word, 42, 8, 50'000'000, observe_waiter, &context));
        return 0;
    });
    thread->start();
    EXPECT(wait_until([&] { return context.ready.load(); }));
    __atomic_store_n(word, 43, __ATOMIC_SEQ_CST);
    (void)thread->join();
    EXPECT_EQ(result.load(), to_underlying(Core::AtomicWaitResult::TimedOut));
    EXPECT_EQ(Core::atomic_notify(word, 8, 1), 0u);
}

TEST_CASE(cancellation_removes_an_abandoned_waiter)
{
    auto buffer = make_word_buffer();
    IGNORE_USE_IN_ESCAPING_LAMBDA auto* word = buffer.data<u64>();
    *word = 42;
    IGNORE_USE_IN_ESCAPING_LAMBDA WaiterContext context;
    IGNORE_USE_IN_ESCAPING_LAMBDA Atomic<u8> result;
    auto thread = Threading::Thread::construct("CancelledWaiter"sv, [&] {
        result.store(Core::core_atomic_wait(word, 42, 8, -1, observe_waiter, &context));
        return 0;
    });
    thread->start();
    EXPECT(wait_until([&] { return context.ready.load(); }));
    context.cancelled.store(true);
    EXPECT_EQ(Core::atomic_notify(word, 8, 1), 0u);
    (void)thread->join();
    EXPECT_EQ(result.load(), to_underlying(Core::AtomicWaitResult::TimedOut));
    EXPECT_EQ(Core::atomic_notify(word, 8, 1), 0u);
}

TEST_CASE(shared_location_is_independent_of_mapping_address)
{
    IGNORE_USE_IN_ESCAPING_LAMBDA u64 first_mapping = 42;
    u64 second_mapping = 42;
    IGNORE_USE_IN_ESCAPING_LAMBDA WaiterContext context;
    IGNORE_USE_IN_ESCAPING_LAMBDA Atomic<u8> result;
    // Two addresses represent views of one shared location. The object ID and byte offset identify its list;
    // neither the mapping's address nor the element width contributes to that identity.
    auto thread = Threading::Thread::construct("AliasWaiter"sv, [&] {
        result.store(Core::core_atomic_wait(&first_mapping, 42, 8, 5'000'000'000, observe_waiter, &context, 1234, 8));
        return 0;
    });
    thread->start();
    EXPECT(wait_until([&] { return context.ready.load(); }));
    EXPECT_EQ(Core::core_atomic_notify(&second_mapping, 1, 1234, 0), 0u);
    EXPECT_EQ(Core::core_atomic_notify(&second_mapping, 1, 5678, 8), 0u);
    EXPECT_EQ(Core::core_atomic_notify(&second_mapping, 1, 1234, 8), 1u);
    (void)thread->join();
    EXPECT_EQ(result.load(), to_underlying(Core::AtomicWaitResult::Woken));
}
