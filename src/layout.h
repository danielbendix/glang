#ifndef LANG_layout_h
#define LANG_layout_h

#include "common.h"

#include <utility>
#include <cassert>

using Offset = u32;

struct Align {
    u8 alignment;

    Align(u8 alignment) : alignment{alignment} {
        assert(alignment <= 31);
    }

    u32 alignmentValue() const {
        return 1 << alignment;
    }
};

struct Layout {
private:
    Align _alignment;
    u32 _size;
public:

    Layout(Align alignment, u32 size) : _alignment{alignment}, _size{size} {}

    /// The alignment as a power of two.
    u8 alignment() const {
        return _alignment.alignment;
    }

    /// The actual numeric value of the alignment.
    u32 alignmentValue() const {
        return _alignment.alignmentValue();
    }

    /// The size required for a single value.
    u32 size() const {
        return _size;
    }

    /// The value that must be added to access the next value when stored contiguously.
    u32 stride() const {
        auto alignMask = alignmentValue() - 1;
        return (_size + alignMask) & ~alignMask;
    }
};

/**
 * Combine the two layouts according to LANG rules.
 * This results in a combined layout, and an offset for the second layout within the first.
 */
std::pair<Layout, Offset> incorporateLayoutAsField(Layout parent, Layout child);

/**
 * Combine the two layouts according to C rules.
 * This results in a combined layout, and an offset for the second layout within the first.
 */
std::pair<Layout, Offset> incorporateLayoutAsField_C_ABI(Layout parent, Layout child);

/**
 * Round the size of the layout up to be a multiple of its alignment value.
 */
Layout addPaddingToLayout_C_ABI(Layout layout);

#endif // LANG_layout_h
