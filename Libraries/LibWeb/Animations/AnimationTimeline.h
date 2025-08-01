/*
 * Copyright (c) 2023, Matthew Olsson <mattco@serenityos.org>.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibWeb/Bindings/PlatformObject.h>

namespace Web::Animations {

// https://www.w3.org/TR/web-animations-1/#animationtimeline
class AnimationTimeline : public Bindings::PlatformObject {
    WEB_PLATFORM_OBJECT(AnimationTimeline, Bindings::PlatformObject);
    GC_DECLARE_ALLOCATOR(AnimationTimeline);

public:
    Optional<double> current_time() const;
    virtual void set_current_time(Optional<double>);

    GC::Ptr<DOM::Document> associated_document() const { return m_associated_document; }
    void set_associated_document(GC::Ptr<DOM::Document>);

    virtual bool is_inactive() const;
    bool is_monotonically_increasing() const { return m_is_monotonically_increasing; }

    // https://www.w3.org/TR/web-animations-1/#timeline-time-to-origin-relative-time
    virtual Optional<double> convert_a_timeline_time_to_an_origin_relative_time(Optional<double>) { VERIFY_NOT_REACHED(); }
    virtual bool can_convert_a_timeline_time_to_an_origin_relative_time() const { return false; }

    void associate_with_animation(GC::Ref<Animation> value) { m_associated_animations.set(value); }
    void disassociate_with_animation(GC::Ref<Animation> value) { m_associated_animations.remove(value); }
    HashTable<GC::Ref<Animation>> const& associated_animations() const { return m_associated_animations; }

protected:
    AnimationTimeline(JS::Realm&);

    virtual void initialize(JS::Realm&) override;
    virtual void visit_edges(Cell::Visitor&) override;
    virtual void finalize() override;

    // https://www.w3.org/TR/web-animations-1/#dom-animationtimeline-currenttime
    Optional<double> m_current_time {};

    // https://drafts.csswg.org/web-animations-1/#monotonically-increasing-timeline
    bool m_is_monotonically_increasing { false };

    // https://www.w3.org/TR/web-animations-1/#timeline-associated-with-a-document
    GC::Ptr<DOM::Document> m_associated_document {};

    HashTable<GC::Ref<Animation>> m_associated_animations {};
};

}
