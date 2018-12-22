// For x86-64

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct AQamlValue {
    enum {
        AQAML_INTEGER,
        AQAML_ARRAY,  // array, tuple, record
        AQAML_STRING,
    } kind;

    union {
        unsigned long integer;

        struct {
            unsigned long header;
            unsigned long data[];
        } * array;

        struct {
            unsigned long header;
            unsigned char str[];
        } * string;
    };
} AQamlValue;

unsigned long aqaml_string_length_detail(unsigned long ptr);

void *aqaml_malloc_detail(unsigned int size)
{
    return malloc(size);
}

AQamlValue get_value(unsigned long src)
{
    if ((src & 1) == 1)
        return (AQamlValue){.kind = AQAML_INTEGER, .integer = src};

    AQamlValue val = {.integer = src - 8};
    unsigned int tag = val.array->header & 0xff;
    switch (tag) {
    case 0 ... 250:  // below No_scan_tag
        val.kind = AQAML_ARRAY;
        return val;

    case 252:  // string
        val.kind = AQAML_STRING;
        return val;
    }

    fprintf(stderr, "%u\n", (unsigned int)(val.array->header & 0xffffffff));
    assert(0);
}

unsigned int aqaml_structural_equal_detail(unsigned long lhs_src,
                                           unsigned long rhs_src)
{
    AQamlValue lhs = get_value(lhs_src), rhs = get_value(rhs_src);

    if (lhs.kind != rhs.kind) return 0;

    switch (lhs.kind) {
    case AQAML_INTEGER:
        return lhs.integer == rhs.integer;

    case AQAML_ARRAY: {
        unsigned long size = lhs.array->header >> 10;
        for (int i = 0; i < size; i++) {
            unsigned long lhs_src = lhs.array->data[i],
                          rhs_src = rhs.array->data[i];
            if (aqaml_structural_equal_detail(lhs_src, rhs_src) == 0) return 0;
        }
    } break;

    case AQAML_STRING: {
        unsigned long lhs_size = aqaml_string_length_detail(lhs_src),
                      rhs_size = aqaml_string_length_detail(rhs_src);
        if (lhs_size != rhs_size) return 0;
        return memcmp(lhs.string->str, rhs.string->str, lhs_size) == 0;
    } break;

    default:
        assert(0);
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

unsigned long aqaml_concat_string_detail(unsigned long lhs_src,
                                         unsigned long rhs_src)
{
    unsigned long lhs_len = aqaml_string_length_detail(lhs_src),
                  rhs_len = aqaml_string_length_detail(rhs_src);
    AQamlValue lhs = get_value(lhs_src), rhs = get_value(rhs_src);
    // assert(lhs.kind == AQAML_STRING && rhs.kind == AQAML_STRING);

    unsigned long ret_src =
        (unsigned long)aqaml_alloc_block((lhs_len + rhs_len) / 8 + 1, 0, 252);
    AQamlValue ret = get_value(ret_src);
    unsigned long space = 7 - (lhs_len + rhs_len) % 8;

    for (unsigned long i = 0; i < lhs_len; i++)
        ret.string->str[i] = lhs.string->str[i];
    for (unsigned long i = 0; i < rhs_len; i++)
        ret.string->str[i + lhs_len] = rhs.string->str[i];
    for (unsigned long i = 0; i < space; i++)
        ret.string->str[i + lhs_len + rhs_len] = 0u;
    ret.string->str[lhs_len + rhs_len + space] = space;

    return ret_src;
}

unsigned long aqaml_string_length_detail(unsigned long ptr)
{
    AQamlValue val = get_value(ptr);
    assert(val.kind == AQAML_STRING);
    unsigned long length = (val.string->header >> 10) * 8 - 1;
    length -= val.string->str[length];
    return length;
}

void aqaml_print_string_detail(unsigned long ptr)
{
    AQamlValue val = get_value(ptr);
    assert(val.kind == AQAML_STRING);
    unsigned long length = aqaml_string_length_detail(ptr);

    for (unsigned long i = 0; i < length; i++) putchar(val.string->str[i]);
}
