// For x86-64

#include <stdlib.h>

void *aqaml_malloc_detail(unsigned int size)
{
    return malloc(size);
}

typedef struct AQamlBlock {
    unsigned long header;
    unsigned long data[];
} AQamlBlock;

unsigned int aqaml_structural_equal_detail(unsigned long lhs, unsigned long rhs)
{
    if ((lhs & 1) == 1)  // integer
        return lhs == rhs ? 1 : 0;

    // pointer
    AQamlBlock *lhs_blk = (AQamlBlock *)lhs, *rhs_blk = (AQamlBlock *)rhs;
    if (lhs_blk->header != rhs_blk->header) return 1;
    unsigned long size = (lhs_blk->header >> 12) - 1;

    for (int i = 0; i < size; i++) {
        unsigned long lhs = lhs_blk->data[i], rhs = rhs_blk->data[i];
        if (aqaml_structural_equal_detail(lhs, rhs) == 0) return 0;
    }

    return 1;
}

void *aqaml_alloc_block(unsigned long size_in_bytes, unsigned long color,
                        unsigned long tag)
{
    unsigned long *ptr = aqaml_malloc_detail(size_in_bytes);
    // size in word (54 bits) | color (2 bits) | tag byte (8 bits)
    *ptr = ((size_in_bytes / 2) << 10) | (color << 8) | tag;
    return ptr;
}
