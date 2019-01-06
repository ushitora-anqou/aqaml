// For x86-64

#include <assert.h>
#include <stdint.h>
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
        uint64_t integer;

        struct {
            uint64_t header;
            uint64_t data[];
        } * array;

        struct {
            uint64_t header;
            uint8_t str[];
        } * string;
    };
} AQamlValue;

uint64_t aqaml_string_length_detail(uint64_t ptr);
uint64_t aqaml_string_create_detail(uint64_t len);

void *aqaml_malloc_detail(uint32_t size)
{
    return malloc(size);
}

AQamlValue get_value(uint64_t src)
{
    if ((src & 1) == 1)
        return (AQamlValue){.kind = AQAML_INTEGER, .integer = src};

    AQamlValue val = {.integer = src - 8};
    uint32_t tag = val.array->header & 0xff;
    switch (tag) {
    case 0 ... 250:  // below No_scan_tag
        val.kind = AQAML_ARRAY;
        return val;

    case 252:  // string
        val.kind = AQAML_STRING;
        return val;
    }

    fprintf(stderr, "%u\n", (uint32_t)(val.array->header & 0xffffffff));
    assert(0);
}

uint32_t aqaml_structural_equal_detail(uint64_t lhs_src, uint64_t rhs_src)
{
    AQamlValue lhs = get_value(lhs_src), rhs = get_value(rhs_src);

    if (lhs.kind != rhs.kind) return 0;

    switch (lhs.kind) {
    case AQAML_INTEGER:
        return lhs.integer == rhs.integer;

    case AQAML_ARRAY: {
        uint64_t size = lhs.array->header >> 10;
        for (uint32_t i = 0; i < size; i++) {
            uint64_t lhs_src = lhs.array->data[i], rhs_src = rhs.array->data[i];
            if (aqaml_structural_equal_detail(lhs_src, rhs_src) == 0) return 0;
        }
    } break;

    case AQAML_STRING: {
        uint64_t lhs_size = aqaml_string_length_detail(lhs_src),
                 rhs_size = aqaml_string_length_detail(rhs_src);
        if (lhs_size != rhs_size) return 0;
        return memcmp(lhs.string->str, rhs.string->str, lhs_size) == 0;
    } break;

    default:
        assert(0);
    }

    return 1;
}

uint64_t aqaml_alloc_block(uint64_t size, uint64_t color, uint64_t tag)
{
    uint64_t *ptr = aqaml_malloc_detail((size + 1) * 8);
    // size in word (54 bits) | color (2 bits) | tag byte (8 bits)
    *ptr = (size << 10) | (color << 8) | tag;
    return (uint64_t)(ptr + 1);
}

uint64_t aqaml_concat_string_detail(uint64_t lhs_src, uint64_t rhs_src)
{
    uint64_t lhs_len = aqaml_string_length_detail(lhs_src),
             rhs_len = aqaml_string_length_detail(rhs_src);
    AQamlValue lhs = get_value(lhs_src), rhs = get_value(rhs_src);
    // assert(lhs.kind == AQAML_STRING && rhs.kind == AQAML_STRING);

    uint64_t ret_src = aqaml_string_create_detail(lhs_len + rhs_len);
    AQamlValue ret = get_value(ret_src);

    for (uint64_t i = 0; i < lhs_len; i++)
        ret.string->str[i] = lhs.string->str[i];
    for (uint64_t i = 0; i < rhs_len; i++)
        ret.string->str[i + lhs_len] = rhs.string->str[i];

    return ret_src;
}

uint64_t aqaml_string_length_detail(uint64_t ptr)
{
    AQamlValue val = get_value(ptr);
    assert(val.kind == AQAML_STRING);
    uint64_t length = (val.string->header >> 10) * 8 - 1;
    length -= val.string->str[length];
    return length;
}

uint64_t aqaml_string_get_detail(uint64_t str_src, uint64_t index)
{
    AQamlValue val = get_value(str_src);
    // assert(str_src.kind == AQAML_STRING);
    uint64_t length = aqaml_string_length_detail(str_src);
    assert(index < length);  // TODO: raise Invalid_argument
    return val.string->str[index];
}

void aqaml_string_set_detail(uint64_t str_src, uint64_t index, uint64_t chr)
{
    AQamlValue val = get_value(str_src);
    // assert(str_src.kind == AQAML_STRING);
    uint64_t length = aqaml_string_length_detail(str_src);
    assert(index < length);  // TODO: raise Invalid_argument
    val.string->str[index] = (uint8_t)chr;
}

uint64_t aqaml_string_create_detail(uint64_t len)
{
    uint64_t ret_src = aqaml_alloc_block(len / 8 + 1, 0, 252);
    uint64_t space = 7 - len % 8;
    AQamlValue ret = get_value(ret_src);
    for (uint64_t i = 0; i < space; i++) ret.string->str[len + i] = 0;
    ret.string->str[len + space] = space;
    return ret_src;
}

uint64_t aqaml_string_sub_detail(uint64_t str_src, uint64_t start, uint64_t len)
{
    AQamlValue str = get_value(str_src);
    // assert(str.kind == AQAML_STRING);
    assert(start + len <= aqaml_string_length_detail(str_src));
    uint64_t ret_src = aqaml_string_create_detail(len);
    AQamlValue ret = get_value(ret_src);

    for (uint64_t i = 0; i < len; i++)
        ret.string->str[i] = str.string->str[start + i];

    return ret_src;
}

void aqaml_string_blit_detail(uint64_t src_src, uint64_t srcoff,
                              uint64_t dst_src, uint64_t dstoff, uint64_t len)
{
    AQamlValue src = get_value(src_src), dst = get_value(dst_src);
    assert(src.kind == AQAML_STRING && dst.kind == AQAML_STRING);
    uint64_t src_len = aqaml_string_length_detail(src_src),
             dst_len = aqaml_string_length_detail(dst_src);
    assert(srcoff + len <= src_len && dstoff + len <= dst_len);

    for (uint64_t i = 0; i < len; i++)
        dst.string->str[dstoff + i] = src.string->str[srcoff + i];
}

void aqaml_print_string_detail(uint64_t ptr)
{
    AQamlValue val = get_value(ptr);
    assert(val.kind == AQAML_STRING);
    uint64_t length = aqaml_string_length_detail(ptr);

    for (uint64_t i = 0; i < length; i++) putchar(val.string->str[i]);
}

uint64_t aqaml_concat_list_detail(uint64_t lhs_src, uint64_t rhs_src)
{
    AQamlValue lhs = get_value(lhs_src), rhs = get_value(rhs_src);

    if (lhs.kind == AQAML_INTEGER)  // lhs is an empty list.
        return rhs_src;
    if (rhs.kind == AQAML_INTEGER)  // rhs is an empty list
        return lhs_src;

    assert(lhs.kind == AQAML_ARRAY && rhs.kind == AQAML_ARRAY);

    // Find the last block of lhs.
    while (lhs.array->data[1] != 1u) lhs = get_value(lhs.array->data[1]);

    // Concat the lists.
    lhs.array->data[1] = rhs_src;

    return lhs_src;
}

uint64_t aqaml_string_of_int_detail(int64_t num)
{
    // calculate length
    uint64_t tmp = num < 0 ? -num : num, length = 0;
    if (num < 0) length++;
    do {
        length++;
    } while (tmp /= 10);

    // create string
    uint64_t ret_src = aqaml_string_create_detail(length);
    AQamlValue ret = get_value(ret_src);
    tmp = num < 0 ? -num : num;
    for (uint64_t i = length - 1;; i--) {
        ret.string->str[i] = '0' + tmp % 10;
        tmp /= 10;
        if (tmp == 0) break;
    }
    if (num < 0) ret.string->str[0] = '-';

    return ret_src;
}

