/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Utf16String.h>
#include <AK/Utf16View.h>
#include <AK/Utf32View.h>
#include <AK/Variant.h>
#include <LibUnicode/CharacterTypes.h>
#include <LibUnicode/Locale.h>
#include <LibUnicode/RustFFI.h>
#include <LibUnicode/Segmenter.h>

namespace Unicode {

SegmenterGranularity segmenter_granularity_from_string(StringView segmenter_granularity)
{
    if (segmenter_granularity == "grapheme"sv)
        return SegmenterGranularity::Grapheme;
    if (segmenter_granularity == "line"sv)
        return SegmenterGranularity::Line;
    if (segmenter_granularity == "sentence"sv)
        return SegmenterGranularity::Sentence;
    if (segmenter_granularity == "word"sv)
        return SegmenterGranularity::Word;
    VERIFY_NOT_REACHED();
}

StringView segmenter_granularity_to_string(SegmenterGranularity segmenter_granularity)
{
    switch (segmenter_granularity) {
    case SegmenterGranularity::Grapheme:
        return "grapheme"sv;
    case SegmenterGranularity::Line:
        return "line"sv;
    case SegmenterGranularity::Sentence:
        return "sentence"sv;
    case SegmenterGranularity::Word:
        return "word"sv;
    }
    VERIFY_NOT_REACHED();
}

// Fast path segmenter for ASCII text where every character is its own grapheme.
// This avoids all ICU overhead for the common case of ASCII-only text.
class AsciiGraphemeSegmenter final : public Segmenter {
public:
    explicit AsciiGraphemeSegmenter(size_t length)
        : Segmenter(SegmenterGranularity::Grapheme)
        , m_length(length)
    {
    }

    virtual ~AsciiGraphemeSegmenter() override = default;

    virtual NonnullOwnPtr<Segmenter> clone() const override
    {
        return make<AsciiGraphemeSegmenter>(m_length);
    }

    virtual void set_segmented_text(String text) override
    {
        m_length = text.byte_count();
    }

    virtual void set_segmented_text(Utf16View const& text) override
    {
        m_length = text.length_in_code_units();
    }

    virtual size_t current_boundary() override
    {
        return m_current;
    }

    virtual Optional<size_t> previous_boundary(size_t index, Inclusive inclusive) override
    {
        if (inclusive == Inclusive::Yes && index <= m_length)
            return index;
        if (index == 0)
            return {};
        return index - 1;
    }

    virtual Optional<size_t> next_boundary(size_t index, Inclusive inclusive) override
    {
        if (inclusive == Inclusive::Yes && index <= m_length)
            return index;
        if (index >= m_length)
            return {};
        return index + 1;
    }

    virtual void for_each_boundary(String text, SegmentationCallback callback) override
    {
        set_segmented_text(move(text));
        for_each_boundary_impl(callback);
    }

    virtual void for_each_boundary(Utf16View const& text, SegmentationCallback callback) override
    {
        set_segmented_text(text);
        for_each_boundary_impl(callback);
    }

    virtual void for_each_boundary(Utf32View const& text, SegmentationCallback callback) override
    {
        m_length = text.length();
        for_each_boundary_impl(callback);
    }

    virtual bool is_current_boundary_word_like() const override
    {
        return false;
    }

private:
    void for_each_boundary_impl(SegmentationCallback& callback)
    {
        for (size_t i = 0; i <= m_length; ++i) {
            if (callback(i) == IterationDecision::Break)
                return;
        }
    }

    size_t m_length { 0 };
    size_t m_current { 0 };
};

struct SegmentBoundary {
    size_t offset { 0 };
    bool word_like { false };
};

class SegmenterImpl final : public Segmenter {
public:
    SegmenterImpl(FFI::IcuSegmenter* segmenter, String locale, SegmenterGranularity segmenter_granularity)
        : Segmenter(segmenter_granularity)
        , m_segmenter(segmenter)
        , m_locale(move(locale))
    {
        VERIFY(m_segmenter);
    }

    virtual ~SegmenterImpl() override
    {
        FFI::icu_segmenter_destroy(m_segmenter);
    }

    virtual NonnullOwnPtr<Segmenter> clone() const override
    {
        auto clone = Segmenter::create(m_locale, m_segmenter_granularity);

        m_segmented_text.visit(
            [&](String const& text) {
                clone->set_segmented_text(text);
                (void)clone->next_boundary(m_boundaries[m_current_boundary_index].offset, Inclusive::Yes);
            },
            [&](Utf16String const& text) {
                clone->set_segmented_text(text.utf16_view());
                (void)clone->next_boundary(m_boundaries[m_current_boundary_index].offset, Inclusive::Yes);
            },
            [](Empty) {});

        return clone;
    }

    virtual void set_segmented_text(String text) override
    {
        m_segmented_text = move(text);
        auto const& segmented_text = m_segmented_text.get<String>();
        populate_utf8_boundaries(segmented_text);
    }

    virtual void set_segmented_text(Utf16View const& text) override
    {
        if (text.has_ascii_storage()) {
            set_segmented_text(MUST(text.to_utf8()));
            return;
        }

        m_segmented_text = Utf16String::from_utf16(text);
        auto const& segmented_text = m_segmented_text.get<Utf16String>();
        populate_utf16_boundaries(segmented_text.utf16_view());
    }

    virtual size_t current_boundary() override
    {
        if (m_boundaries.is_empty())
            return 0;
        return m_boundaries[m_current_boundary_index].offset;
    }

    virtual Optional<size_t> previous_boundary(size_t boundary, Inclusive inclusive) override
    {
        if (m_boundaries.is_empty())
            return {};

        auto aligned_boundary = align_boundary(boundary);
        auto boundary_index = lower_bound_boundary_index(aligned_boundary);

        if (inclusive == Inclusive::Yes && boundary_index < m_boundaries.size() && m_boundaries[boundary_index].offset == aligned_boundary) {
            m_current_boundary_index = boundary_index;
            return aligned_boundary;
        }

        if (boundary_index == 0)
            return {};

        m_current_boundary_index = boundary_index - 1;
        return m_boundaries[m_current_boundary_index].offset;
    }

    virtual Optional<size_t> next_boundary(size_t boundary, Inclusive inclusive) override
    {
        if (m_boundaries.is_empty())
            return {};

        auto aligned_boundary = align_boundary(boundary);
        auto boundary_index = inclusive == Inclusive::Yes ? lower_bound_boundary_index(aligned_boundary) : upper_bound_boundary_index(aligned_boundary);

        if (boundary_index >= m_boundaries.size())
            return {};

        m_current_boundary_index = boundary_index;
        return m_boundaries[m_current_boundary_index].offset;
    }

    virtual void for_each_boundary(String text, SegmentationCallback callback) override
    {
        if (text.is_empty())
            return;

        set_segmented_text(move(text));
        for_each_boundary_impl(callback);
    }

    virtual void for_each_boundary(Utf16View const& text, SegmentationCallback callback) override
    {
        if (text.is_empty())
            return;

        set_segmented_text(text);
        for_each_boundary_impl(callback);
    }

    virtual void for_each_boundary(Utf32View const& text, SegmentationCallback callback) override
    {
        if (text.is_empty())
            return;

        set_segmented_text(MUST(String::formatted("{}", text)));

        auto code_points = m_segmented_text.get<String>().code_points();
        auto current = code_points.begin();
        size_t code_point_index = 0;

        for_each_boundary_impl([&](auto index) {
            auto it = code_points.iterator_at_byte_offset(index);

            while (current != it) {
                ++code_point_index;
                ++current;
            }

            return callback(code_point_index);
        });
    }

    virtual bool is_current_boundary_word_like() const override
    {
        if (m_boundaries.is_empty())
            return false;
        return m_boundaries[m_current_boundary_index].word_like;
    }

private:
    static bool append_boundary(void* context, size_t boundary, bool word_like)
    {
        auto& boundaries = *reinterpret_cast<Vector<SegmentBoundary>*>(context);
        return !boundaries.try_append({ boundary, word_like }).is_error();
    }

    size_t align_boundary(size_t boundary) const
    {
        return m_segmented_text.visit(
            [&](String const& text) {
                if (boundary >= text.byte_count())
                    return text.byte_count();

                auto code_points = text.code_points();
                auto iterator = code_points.iterator_at_byte_offset(boundary);
                return code_points.byte_offset_of(iterator);
            },
            [&](Utf16String const& text) {
                auto view = text.utf16_view();
                if (boundary >= view.length_in_code_units())
                    return view.length_in_code_units();

                auto code_point_offset = view.code_point_offset_of(boundary);
                return view.code_unit_offset_of(code_point_offset);
            },
            [](Empty) -> size_t { VERIFY_NOT_REACHED(); });
    }

    size_t lower_bound_boundary_index(size_t boundary) const
    {
        size_t left = 0;
        size_t right = m_boundaries.size();

        while (left < right) {
            auto middle = left + (right - left) / 2;
            if (m_boundaries[middle].offset < boundary)
                left = middle + 1;
            else
                right = middle;
        }

        return left;
    }

    size_t upper_bound_boundary_index(size_t boundary) const
    {
        size_t left = 0;
        size_t right = m_boundaries.size();

        while (left < right) {
            auto middle = left + (right - left) / 2;
            if (m_boundaries[middle].offset <= boundary)
                left = middle + 1;
            else
                right = middle;
        }

        return left;
    }

    void populate_utf8_boundaries(String const& text)
    {
        m_boundaries.clear();
        m_current_boundary_index = 0;

        auto success = FFI::icu_segmenter_segment_utf8(m_segmenter, text.bytes().data(), text.byte_count(), &m_boundaries, append_boundary);
        VERIFY(success);
        VERIFY(!m_boundaries.is_empty());
    }

    void populate_utf16_boundaries(Utf16View const& text)
    {
        m_boundaries.clear();
        m_current_boundary_index = 0;

        auto span = text.utf16_span();
        auto success = FFI::icu_segmenter_segment_utf16(m_segmenter, reinterpret_cast<u16 const*>(span.data()), span.size(), &m_boundaries, append_boundary);
        VERIFY(success);
        VERIFY(!m_boundaries.is_empty());
    }

    template<typename Callback>
    void for_each_boundary_impl(Callback&& callback)
    {
        for (size_t i = 0; i < m_boundaries.size(); ++i) {
            m_current_boundary_index = i;

            if (callback(m_boundaries[i].offset) == IterationDecision::Break)
                return;
        }
    }

    FFI::IcuSegmenter* m_segmenter { nullptr };
    String m_locale;
    Variant<Empty, String, Utf16String> m_segmented_text;
    Vector<SegmentBoundary> m_boundaries;
    size_t m_current_boundary_index { 0 };
};

NonnullOwnPtr<Segmenter> Segmenter::create(SegmenterGranularity segmenter_granularity)
{
    return Segmenter::create(default_locale(), segmenter_granularity);
}

NonnullOwnPtr<Segmenter> Segmenter::create(StringView locale, SegmenterGranularity segmenter_granularity)
{
    auto locale_string = MUST(String::from_utf8(locale));
    auto* segmenter = FFI::icu_segmenter_create(to_underlying(segmenter_granularity), locale.bytes().data(), locale.length());
    VERIFY(segmenter);
    return make<SegmenterImpl>(segmenter, move(locale_string), segmenter_granularity);
}

NonnullOwnPtr<Segmenter> Segmenter::create_for_ascii_grapheme(size_t length)
{
    return make<AsciiGraphemeSegmenter>(length);
}

bool Segmenter::should_continue_beyond_word(Utf16View const& word)
{
    for (auto code_point : word) {
        if (!code_point_has_punctuation_general_category(code_point) && !code_point_has_separator_general_category(code_point))
            return false;
    }
    return true;
}

}
