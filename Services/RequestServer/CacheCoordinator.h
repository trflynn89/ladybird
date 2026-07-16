/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Badge.h>
#include <AK/Optional.h>
#include <LibHTTP/Cache/MemoryCache.h>

namespace HTTP {

class DiskCache;

}

namespace RequestServer {

class ConnectionFromClient;

class CacheCoordinator {
public:
    explicit CacheCoordinator(Optional<HTTP::DiskCache&>);

    Optional<HTTP::DiskCache&> disk_cache() const { return m_disk_cache; }
    NonnullRefPtr<HTTP::MemoryCache> private_memory_cache() const { return m_private_memory_cache; }

    void reset_private_memory_cache(Badge<ConnectionFromClient>);

private:
    Optional<HTTP::DiskCache&> m_disk_cache;
    NonnullRefPtr<HTTP::MemoryCache> m_private_memory_cache;
};

}
