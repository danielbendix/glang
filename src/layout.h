#ifndef LANG_layout_h
#define LANG_layout_h

#include <utility>

#include <cstdint>
#include <cassert>

using Offset = uint32_t;

struct Align {
    uint8_t alignment;

    Align(uint8_t alignment) : alignment{alignment} {
        assert(alignment <= 31);
    }

    uint32_t alignmentValue() const {
        return 1 << alignment;
    }
};

struct Layout {
private:
    Align _alignment;
    uint32_t _size;
public:

    Layout(Align alignment, uint32_t size) : _alignment{alignment}, _size{size} {}

    /// The alignment as a power of two.
    uint8_t alignment() const {
        return _alignment.alignment;
    }

    /// The actual numeric value of the alignment.
    uint32_t alignmentValue() const {
        return _alignment.alignmentValue();
    }

    /// The size required for a single value.
    uint32_t size() const {
        return _size;
    }

    /// The value that must be added to access the next value when stored contiguously.
    uint32_t stride() const {
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
