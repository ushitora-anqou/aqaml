// For x86-64

#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

////// struct Vector implementation from here

typedef struct Vector Vector;
struct Vector {
    void **data;
    size_t size, rsved_size;
};

Vector *new_vector(void)
{
    Vector *ret = (Vector *)malloc(sizeof(Vector));
    ret->size = 0;
    ret->rsved_size = 0;
    ret->data = NULL;
    return ret;
}

// NOTE: delete_vector() WILL NOT call free(3) for each element in the vector,
//       since it doesn't know the actual content of each one.
void delete_vector(Vector *vec)
{
    if (vec->data != NULL) free(vec->data);
    free(vec);
}

void vector_push_back(Vector *vec, void *item)
{
    assert(vec != NULL);

    if (vec->size == vec->rsved_size) {
        vec->rsved_size = vec->rsved_size > 0 ? vec->rsved_size * 2 : 2;
        // TODO: Use realloc(3), which somehow show an "Illegal instruction"
        // error.
        void **ndata = (void **)malloc(sizeof(void *) * vec->rsved_size);
        if (vec->data != NULL) {
            memcpy(ndata, vec->data, vec->size * sizeof(void *));
            free(vec->data);
        }
        vec->data = ndata;
    }

    vec->data[vec->size++] = item;
}

void *vector_pop_back(Vector *vec)
{
    if (vec->size == 0) return NULL;
    return vec->data[--vec->size];
}

void *vector_get(Vector *vec, size_t i)
{
    if (i >= vec->size) return NULL;
    return vec->data[i];
}

size_t vector_size(Vector *vec)
{
    return vec->size;
}

void *vector_set(Vector *vec, size_t i, void *item)
{
    assert(vec != NULL && i < vector_size(vec));
    vec->data[i] = item;
    return item;
}

void vector_push_back_vector(Vector *vec, Vector *src)
{
    for (size_t i = 0; i < vector_size(src); i++)
        vector_push_back(vec, vector_get(src, i));
}

void vector_sort(Vector *vec, int (*compar)(const void *, const void *))
{
    qsort(vec->data, vec->size, sizeof(void *), compar);
}

////// struct Vector implementation to here

extern uint64_t aqaml_initial_rsp, aqaml_current_rsp;
extern uint64_t aqaml_sys_argv;
static Vector *malloced_regions = NULL;
enum GC_COLOR { WHITE = 0, BLACK = 3, GRAY = 1, BLUE = 2 };
static const int No_scan_tag = 251;

typedef struct AQamlValue {
    enum {
        AQAML_INTEGER,
        AQAML_ARRAY,  // array, tuple, record
        AQAML_STRING,
    } kind;

    union {
        int64_t integer;

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
uint64_t aqaml_appcls_detail(uint64_t nargs, uint64_t cls_src, uint64_t *args);

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

int aqaml_gc_is_malloced(uint64_t value)
{
    assert(malloced_regions != NULL);

    if ((value & 1) == 1) return 0;  // integer

    // [begin, end)
    uint64_t begin = 0, end = vector_size(malloced_regions);
    while (end - begin > 1) {
        uint64_t mid = begin + (end - begin) / 2;
        uint64_t t = (uint64_t)vector_get(malloced_regions, mid);
        if (t <= value)
            begin = mid;
        else
            end = mid;
    }

    return (uint64_t)vector_get(malloced_regions, begin) == value;
}

void aqaml_gc_mark_block(uint64_t *ptr)
{
    uint64_t size = ptr[0] >> 10, color = (ptr[0] >> 8) & 3,
             tag = ptr[0] & 0xff;
    if (color != WHITE) return;  // already scanned or partially scanned

    if (tag > No_scan_tag) {  // opaque block
        ptr[0] |= (3 << 8);
        return;
    }

    // start marking this block
    ptr[0] |= (1 << 8);  // make the color GRAY
    for (uint64_t i = 0; i < size; i++) {
        uint64_t elm = ptr[i + 1] - 8;
        if (aqaml_gc_is_malloced(elm)) aqaml_gc_mark_block((uint64_t *)elm);
    }
    // end marking
    ptr[0] |= (1 << 9);  // make the color BLACK
}

int aqaml_gc_compar_for_malloced_regions(const void *lhs, const void *rhs)
{
    if (*(uint64_t *)lhs < *(uint64_t *)rhs) return -1;
    if (*(uint64_t *)rhs < *(uint64_t *)lhs) return 1;
    return 0;
}

void aqaml_gc(void)
{
    if (aqaml_initial_rsp == 0) return;

    assert(aqaml_current_rsp < aqaml_initial_rsp);
    assert((aqaml_initial_rsp - aqaml_current_rsp) % 8 == 0);

    vector_sort(malloced_regions, aqaml_gc_compar_for_malloced_regions);

    // Let's mark.
    // First, mark pointers on the stack.
    uint64_t size = (aqaml_initial_rsp - aqaml_current_rsp) / 8;
    for (uint64_t i = 0; i < size; i++) {
        uint64_t elm = *(uint64_t *)(aqaml_current_rsp + i * 8) - 8;
        if (aqaml_gc_is_malloced(elm))
            // decrease for block's header
            aqaml_gc_mark_block((uint64_t *)elm);
    }
    // Then, mark global pointers.
    aqaml_gc_mark_block((uint64_t *)(aqaml_sys_argv - 8));

    // Let's sweep.
    Vector *new_malloced_regions = new_vector();
    for (int i = 0; i < vector_size(malloced_regions); i++) {
        uint64_t *ptr = (uint64_t *)vector_get(malloced_regions, i);
        uint64_t color = (ptr[0] >> 8) & 3;
        switch (color) {
        case BLACK:  // marked
            vector_push_back(new_malloced_regions, ptr);
            break;
        case WHITE:  // not marked i.e. should be freed
            free(ptr);
            break;
        default:  // unreachable
            assert(0);
        }
    }
    delete_vector(malloced_regions);
    malloced_regions = new_malloced_regions;

    // clear color flags
    for (uint64_t i = 0; i < vector_size(malloced_regions); i++) {
        uint64_t *ptr = (uint64_t *)vector_get(malloced_regions, i);
        ptr[0] &= 0xFFFFFCFF;  // make the color WHITE
    }
    *(uint64_t *)(aqaml_sys_argv - 8) &= 0xFFFFFCFF;  // make the color WHITE
}

uint64_t aqaml_alloc_block(uint64_t size, uint64_t color, uint64_t tag)
{
    if (malloced_regions == NULL) {  // initialize
        malloced_regions = new_vector();
        assert(malloced_regions != NULL);
    }

    static uint64_t gc_threshold = 100000;
    if (vector_size(malloced_regions) > gc_threshold) {
        // fprintf(stderr, "thr = %lu\t#regions = %d\t=> ", gc_threshold,
        //        vector_size(malloced_regions));
        aqaml_gc();
        // fprintf(stderr, "%d\n", vector_size(malloced_regions));

        gc_threshold *= 1.1;
    }

    uint64_t *ptr = (uint64_t *)malloc((size + 1) * 8);
    vector_push_back(malloced_regions, ptr);

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

uint64_t aqaml_create_string_from_cstr(const char *const cstr)
{
    uint64_t len = strlen(cstr);
    uint64_t ret_src = aqaml_string_create_detail(len);
    AQamlValue ret = get_value(ret_src);
    for (uint64_t i = 0; i < len; i++) ret.string->str[i] = cstr[i];
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

uint64_t aqaml_string_make_detail(uint64_t n, uint64_t c)
{
    uint64_t ret_src = aqaml_string_create_detail(n);
    AQamlValue ret = get_value(ret_src);
    for (uint64_t i = 0; i < n; i++) ret.string->str[i] = (uint8_t)c;
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

void aqaml_prerr_string_detail(uint64_t ptr)
{
    AQamlValue val = get_value(ptr);
    assert(val.kind == AQAML_STRING);
    uint64_t length = aqaml_string_length_detail(ptr);

    for (uint64_t i = 0; i < length; i++) fputc(val.string->str[i], stderr);
}

uint64_t aqaml_open_in_detail(uint64_t path_src)
{
    AQamlValue path = get_value(path_src);
    assert(path.kind == AQAML_STRING);
    FILE *fp = fopen((char *)(path.string->str), "r");
    if (fp == NULL) return 0;
    uint64_t ret_src = aqaml_alloc_block(1, 0, 247);
    AQamlValue ret = get_value(ret_src);
    ret.array->data[0] = (uint64_t)fp;
    return ret_src;
}

void aqaml_close_in_detail(uint64_t chan_src)
{
    AQamlValue chan = get_value(chan_src);
    assert(chan.kind == AQAML_ARRAY);
    FILE *fp = (FILE *)(chan.array->data[0]);
    fclose(fp);
}

uint64_t aqaml_get_stdin_detail()
{
    uint64_t ret_src = aqaml_alloc_block(1, 0, 247);
    AQamlValue ret = get_value(ret_src);
    ret.array->data[0] = (uint64_t)stdin;
    return ret_src;
}

uint64_t aqaml_input_char_detail(uint64_t ptr)
{
    AQamlValue chan = get_value(ptr);
    assert(chan.kind == AQAML_ARRAY);
    FILE *fp = (FILE *)(chan.array->data[0]);
    int ch = fgetc(fp);
    if (ch == EOF) return -1;
    return ((uint64_t)ch << 1) | 1;
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

void aqaml_write_buffer(uint8_t *buf, uint64_t *widx, uint8_t ch)
{
    if (buf) buf[*widx] = ch;
    (*widx)++;
}

uint64_t aqaml_vsprintf_detail(uint8_t *buf, uint8_t *fmt, uint64_t fmt_len,
                               uint64_t *args)
{
    uint64_t widx = 0;

    for (uint64_t i = 0; i < fmt_len; i++) {
        if (fmt[i] != '%') {
            aqaml_write_buffer(buf, &widx, fmt[i]);
            continue;
        }

        i++;
        switch (fmt[i]) {
        case 'd': {
            AQamlValue num = get_value(*args++);
            assert(num.kind == AQAML_INTEGER);
            int64_t ival = num.integer >> 1;
            if (ival == 0) {
                aqaml_write_buffer(buf, &widx, '0');
                break;
            }
            if (ival < 0) {
                aqaml_write_buffer(buf, &widx, '-');
                ival *= -1;
            }
            // max_int = 4611686018427387903, so size should be >= 20
            int8_t i = 0, dig[25];
            for (; ival != 0; ival /= 10) dig[i++] = ival % 10;
            while (--i >= 0) aqaml_write_buffer(buf, &widx, '0' + dig[i]);
        } break;

        case 's': {
            uint64_t str_src = *args++;
            AQamlValue str = get_value(str_src);
            // assert(str.kind == AQAML_STRING);
            uint64_t len = aqaml_string_length_detail(str_src);
            for (uint64_t i = 0; i < len; i++)
                aqaml_write_buffer(buf, &widx, str.string->str[i]);
        } break;

        case 'c': {
            AQamlValue chr = get_value(*args++);
            assert(chr.kind == AQAML_INTEGER);
            aqaml_write_buffer(buf, &widx, (uint8_t)(chr.integer >> 1));
        } break;

        default:
            assert(0);
        }
    }

    return widx;
}

uint64_t aqaml_safe_sprintf(uint64_t fmt_src, uint64_t *args)
{
    AQamlValue fmt = get_value(fmt_src);
    // assert(fmt.kind == AQAML_STRING);
    uint64_t fmt_len = aqaml_string_length_detail(fmt_src);
    uint64_t result_len =
        aqaml_vsprintf_detail(NULL, fmt.string->str, fmt_len, args);
    uint64_t ret_src = aqaml_string_create_detail(result_len);
    AQamlValue ret = get_value(ret_src);
    aqaml_vsprintf_detail(ret.string->str, fmt.string->str, fmt_len, args);
    return ret_src;
}

uint64_t aqaml_printf_ksprintf1_detail(uint64_t arg0_src, uint64_t *fmt_src_ptr)
{
    uint64_t args[] = {arg0_src};
    uint64_t fmt_src = *fmt_src_ptr, cls_src = *(fmt_src_ptr + 1);
    uint64_t str_src = aqaml_safe_sprintf(fmt_src, args);
    return aqaml_appcls_detail(1, cls_src, &str_src);
}

uint64_t aqaml_printf_ksprintf2_detail(uint64_t arg0_src, uint64_t arg1_src,
                                       uint64_t *fmt_src_ptr)
{
    uint64_t args[] = {arg0_src, arg1_src};
    uint64_t fmt_src = *fmt_src_ptr, cls_src = *(fmt_src_ptr + 1);
    uint64_t str_src = aqaml_safe_sprintf(fmt_src, args);
    return aqaml_appcls_detail(1, cls_src, &str_src);
}

uint64_t aqaml_printf_ksprintf3_detail(uint64_t arg0_src, uint64_t arg1_src,
                                       uint64_t arg2_src, uint64_t *fmt_src_ptr)
{
    uint64_t args[] = {arg0_src, arg1_src, arg2_src};
    uint64_t fmt_src = *fmt_src_ptr, cls_src = *(fmt_src_ptr + 1);
    uint64_t str_src = aqaml_safe_sprintf(fmt_src, args);
    return aqaml_appcls_detail(1, cls_src, &str_src);
}

uint64_t aqaml_printf_ksprintf4_detail(uint64_t arg0_src, uint64_t arg1_src,
                                       uint64_t arg2_src, uint64_t arg3_src,
                                       uint64_t *fmt_src_ptr)
{
    uint64_t args[] = {arg0_src, arg1_src, arg2_src, arg3_src};
    uint64_t fmt_src = *fmt_src_ptr, cls_src = *(fmt_src_ptr + 1);
    uint64_t str_src = aqaml_safe_sprintf(fmt_src, args);
    return aqaml_appcls_detail(1, cls_src, &str_src);
}

uint64_t aqaml_printf_ksprintf5_detail(uint64_t arg0_src, uint64_t arg1_src,
                                       uint64_t arg2_src, uint64_t arg3_src,
                                       uint64_t arg4_src, uint64_t *fmt_src_ptr)
{
    uint64_t args[] = {arg0_src, arg1_src, arg2_src, arg3_src, arg4_src};
    uint64_t fmt_src = *fmt_src_ptr, cls_src = *(fmt_src_ptr + 1);
    uint64_t str_src = aqaml_safe_sprintf(fmt_src, args);
    return aqaml_appcls_detail(1, cls_src, &str_src);
}

// dummy
void aqaml_printf_ksprintf1(void);
void aqaml_printf_ksprintf2(void);
void aqaml_printf_ksprintf3(void);
void aqaml_printf_ksprintf4(void);
void aqaml_printf_ksprintf5(void);

uint64_t aqaml_printf_ksprintf_detail(uint64_t callback_src, uint64_t fmt_src)
{
    AQamlValue fmt = get_value(fmt_src);
    // assert(fmt.kind == AQAML_STRING);
    uint64_t fmt_len = aqaml_string_length_detail(fmt_src);

    // Count how many arguments should be taken.
    uint64_t cnt = 0;
    for (uint64_t i = 0; i < fmt_len; i++) {
        if (fmt.string->str[i] != '%') continue;
        i++;
        assert(i != fmt_len);
        if (fmt.string->str[i] == '%') continue;
        cnt++;
    }

    // TODO: more than 5
    uint64_t functable[] = {(uint64_t)0,
                            (uint64_t)aqaml_printf_ksprintf1,
                            (uint64_t)aqaml_printf_ksprintf2,
                            (uint64_t)aqaml_printf_ksprintf3,
                            (uint64_t)aqaml_printf_ksprintf4,
                            (uint64_t)aqaml_printf_ksprintf5};

    if (functable[cnt] == 0)
        return aqaml_appcls_detail(1, callback_src, &fmt_src);

    uint64_t ret_src = aqaml_alloc_block(4, 0, 247);
    AQamlValue ret = get_value(ret_src);
    ret.array->data[0] = functable[cnt];
    ret.array->data[1] = cnt;
    ret.array->data[2] = fmt_src;
    ret.array->data[3] = callback_src;

    return ret_src;
}

uint64_t aqaml_call_func1(uint64_t, uint64_t);
uint64_t aqaml_call_func2(uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func3(uint64_t, uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func4(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func5(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                          uint64_t);
uint64_t aqaml_call_func6(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                          uint64_t, uint64_t);
uint64_t aqaml_call_func7(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                          uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func8(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                          uint64_t, uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func9(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                          uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
uint64_t aqaml_call_func10(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                           uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                           uint64_t);

uint64_t aqaml_appcls_detail(uint64_t nargs, uint64_t cls_src, uint64_t *args)
{
    while (1) {
        AQamlValue cls = get_value(cls_src);
        assert(cls.kind == AQAML_ARRAY);

        uint64_t func_ptr = cls.array->data[0], cls_nargs = cls.array->data[1],
                 cls_data = (uint64_t)(cls.array->data + 2), ret;
        switch (cls_nargs) {
        case 0:
            ret = aqaml_call_func1(func_ptr, cls_data);
            break;
        case 1:
            ret = aqaml_call_func2(func_ptr, args[0], cls_data);
            args += 1;
            break;
        case 2:
            ret = aqaml_call_func3(func_ptr, args[0], args[1], cls_data);
            args += 2;
            break;
        case 3:
            ret =
                aqaml_call_func4(func_ptr, args[0], args[1], args[2], cls_data);
            args += 3;
            break;
        case 4:
            ret = aqaml_call_func5(func_ptr, args[0], args[1], args[2], args[3],
                                   cls_data);
            args += 4;
            break;
        case 5:
            ret = aqaml_call_func6(func_ptr, args[0], args[1], args[2], args[3],
                                   args[4], cls_data);
            args += 5;
            break;
        case 6:
            ret = aqaml_call_func7(func_ptr, args[0], args[1], args[2], args[3],
                                   args[4], args[5], cls_data);
            args += 6;
            break;
        case 7:
            ret = aqaml_call_func8(func_ptr, args[0], args[1], args[2], args[3],
                                   args[4], args[5], args[6], cls_data);
            args += 7;
            break;
        case 8:
            ret =
                aqaml_call_func9(func_ptr, args[0], args[1], args[2], args[3],
                                 args[4], args[5], args[6], args[7], cls_data);
            args += 8;
            break;
        case 9:
            ret = aqaml_call_func10(func_ptr, args[0], args[1], args[2],
                                    args[3], args[4], args[5], args[6], args[7],
                                    args[8], cls_data);
            args += 9;
            break;
        }
        nargs -= cls_nargs;
        // TODO: curry
        if (nargs == 0) return ret;
        cls_src = ret;
    }
}

uint64_t aqaml_array_get_detail(uint64_t ary_src, uint64_t idx)
{
    AQamlValue ary = get_value(ary_src);
    assert(ary.kind == AQAML_ARRAY);
    assert(idx < (ary.array->header >> 10));
    return ary.array->data[idx];
}

uint64_t aqaml_array_length_detail(uint64_t ary_src)
{
    AQamlValue ary = get_value(ary_src);
    assert(ary.kind == AQAML_ARRAY);
    return ary.array->header >> 10;
}

_Noreturn void aqaml_gracefully_exit(void)
{
    for (size_t i = 0; i < vector_size(malloced_regions); i++)
        free(vector_get(malloced_regions, i));
    delete_vector(malloced_regions);
    exit(0);
}
