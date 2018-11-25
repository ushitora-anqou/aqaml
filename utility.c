// For x86-64

#include <stdio.h>
#include <stdlib.h>

void *aqaml_malloc_detail(unsigned int size)
{
    return malloc(size);
}

typedef struct AQamlBlock {
    unsigned long header;
    unsigned long data[];
} AQamlBlock;

AQamlBlock *get_block(unsigned long ptr)
{
    return (AQamlBlock *)(ptr - 8);
}

unsigned int aqaml_structural_equal_detail(unsigned long lhs, unsigned long rhs)
{
    if ((lhs & 1) == 1)  // integer
        return lhs == rhs ? 1 : 0;

    // pointer
    AQamlBlock *lhs_blk = get_block(lhs), *rhs_blk = get_block(rhs);
    if (lhs_blk->header != rhs_blk->header) return 1;
    unsigned long size = lhs_blk->header >> 10;

    for (int i = 0; i < size; i++) {
        unsigned long lhs = lhs_blk->data[i], rhs = rhs_blk->data[i];
        if (aqaml_structural_equal_detail(lhs, rhs) == 0) return 0;
    }

    return 1;
}

void *aqaml_alloc_block(unsigned long size, unsigned long color,
                        unsigned long tag)
{
    unsigned long *ptr = aqaml_malloc_detail((size + 1) * 8);
    // size in word (54 bits) | color (2 bits) | tag byte (8 bits)
    *ptr = (size << 10) | (color << 8) | tag;
    return (ptr + 1);
}

typedef struct AQamlString {
    unsigned long header;
    unsigned char data[];
} AQamlString;

AQamlString *get_string(unsigned long ptr)
{
    return (AQamlString *)(ptr - 8);
}

unsigned long aqaml_string_length_detail(unsigned long ptr)
{
    AQamlString *str = get_string(ptr);
    unsigned long length = (str->header >> 10) * 8 - 1;
    length -= str->data[length];
    return length;
}

void aqaml_print_string_detail(unsigned long ptr)
{
    AQamlString *str = get_string(ptr);
    unsigned long length = aqaml_string_length_detail(ptr);

    for (unsigned long i = 0; i < length; i++) putchar(str->data[i]);
}
