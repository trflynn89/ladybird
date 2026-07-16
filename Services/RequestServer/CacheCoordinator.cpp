/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <RequestServer/CacheCoordinator.h>

namespace RequestServer {

CacheCoordinator::CacheCoordinator(Optional<HTTP::DiskCache&> disk_cache)
    : m_disk_cache(disk_cache)
    , m_private_memory_cache(HTTP::MemoryCache::create())
{
}

void CacheCoordinator::reset_private_memory_cache(Badge<ConnectionFromClient>)
{
    m_private_memory_cache = HTTP::MemoryCache::create();
}

}
