/*
 * Copyright (c) 2023, Luke Wilde <lukew@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWeb/Bindings/IntersectionObserverEntryPrototype.h>
#include <LibWeb/Bindings/Intrinsics.h>
#include <LibWeb/DOM/Element.h>
#include <LibWeb/IntersectionObserver/IntersectionObserverEntry.h>

namespace Web::IntersectionObserver {

GC_DEFINE_ALLOCATOR(IntersectionObserverEntry);

WebIDL::ExceptionOr<GC::Ref<IntersectionObserverEntry>> IntersectionObserverEntry::construct_impl(JS::Realm& realm, Web::IntersectionObserver::IntersectionObserverEntryInit const& options)
{
    auto& vm = realm.vm();

    GC::Ptr<Geometry::DOMRectReadOnly> root_bounds;
    if (options.root_bounds.has_value())
        root_bounds = Geometry::DOMRectReadOnly::from_rect(vm, options.root_bounds.value());

    auto bounding_client_rect = Geometry::DOMRectReadOnly::from_rect(vm, options.bounding_client_rect);
    auto intersection_rect = Geometry::DOMRectReadOnly::from_rect(vm, options.intersection_rect);
    return realm.create<IntersectionObserverEntry>(realm, options.time, root_bounds, bounding_client_rect, intersection_rect, options.is_intersecting, options.intersection_ratio, *options.target);
}

IntersectionObserverEntry::IntersectionObserverEntry(JS::Realm& realm, HighResolutionTime::DOMHighResTimeStamp time, GC::Ptr<Geometry::DOMRectReadOnly> root_bounds, GC::Ref<Geometry::DOMRectReadOnly> bounding_client_rect, GC::Ref<Geometry::DOMRectReadOnly> intersection_rect, bool is_intersecting, double intersection_ratio, GC::Ref<DOM::Element> target)
    : Bindings::PlatformObject(realm)
    , m_time(time)
    , m_root_bounds(root_bounds)
    , m_bounding_client_rect(bounding_client_rect)
    , m_intersection_rect(intersection_rect)
    , m_is_intersecting(is_intersecting)
    , m_intersection_ratio(intersection_ratio)
    , m_target(target)
{
}

IntersectionObserverEntry::~IntersectionObserverEntry() = default;

void IntersectionObserverEntry::initialize(JS::Realm& realm)
{
    WEB_SET_PROTOTYPE_FOR_INTERFACE(IntersectionObserverEntry);
    Base::initialize(realm);
}

void IntersectionObserverEntry::visit_edges(JS::Cell::Visitor& visitor)
{
    Base::visit_edges(visitor);
    visitor.visit(m_root_bounds);
    visitor.visit(m_bounding_client_rect);
    visitor.visit(m_intersection_rect);
    visitor.visit(m_target);
}

}
