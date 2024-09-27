#include "layout.h"

#include <algorithm>

std::pair<Layout, Offset> incorporateLayoutAsField(Layout parent, Layout child)
{
    uint8_t alignment = std::max(parent.alignment(), child.alignment());
    auto alignMask = child.alignmentValue() - 1;

    Offset offset = (parent.size() + alignMask) & ~alignMask;
    uint32_t newSize = offset + child.size();

    return {{alignment, newSize}, offset};
}

std::pair<Layout, Offset> incorporateLayoutAsField_C_ABI(Layout parent, Layout child)
{
    uint8_t alignment = std::max(parent.alignment(), child.alignment());

    auto alignMask = child.alignmentValue() - 1;
    
    // We may want to consider if we want to allow values in here that are not already sized like,
    // and compatible with the C ABI.
    auto childAlignMask = child.alignmentValue() - 1;
    auto childSize = (child.size() + childAlignMask) & ~childAlignMask;

    Offset offset = (parent.size() + alignMask) & ~alignMask;

    uint32_t newSize = offset + childSize;

    return {{alignment, newSize}, offset};
}

Layout addPaddingToLayout_C_ABI(Layout layout)
{
    auto alignMask = layout.alignmentValue() - 1;
    auto size = (layout.size() + alignMask) & ~alignMask;
    return {layout.alignment(), size};
}
