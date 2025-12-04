/*
 * Metascript String - UTF-8 with ORC
 * C API Header for codegen backends
 *
 * Memory Layout:
 *   [RefHeader (8 bytes)][msString (24 bytes)][UTF-8 bytes + null terminator]
 *
 * msString:
 *   - len: size_t (length in bytes, NOT code points)
 *   - capacity: size_t (allocated capacity)
 *   - data: char* (pointer to UTF-8 bytes, null-terminated)
 *
 * Usage:
 *   msString* s = ms_string_new("hello", 5);  // rc=1
 *   ms_string_incref(s);                      // rc=2
 *   msString* s2 = ms_string_concat(s, ...); // Create new string
 *   ms_string_decref(s);                      // rc=1
 *   ms_string_decref(s);                      // rc=0, freed
 */

#ifndef METASCRIPT_STRING_H
#define METASCRIPT_STRING_H

#include "orc.h"
#include <string.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Metascript String Structure
 */
typedef struct {
    size_t len;       /* Length in bytes (NOT code points) */
    size_t capacity;  /* Allocated capacity */
    char* data;       /* UTF-8 bytes (null-terminated) */
} msString;

/*
 * Get RefHeader from string pointer
 */
static inline msRefHeader* ms_string_get_header(msString* str) {
    return (msRefHeader*)((char*)str - sizeof(msRefHeader));
}

/*
 * Create new string from UTF-8 bytes
 * Returns NULL if invalid UTF-8
 * Initial rc=1 (owned by creator)
 */
static inline msString* ms_string_new(const char* data, size_t len) {
    // TODO: UTF-8 validation (for now, assume valid)

    // Allocate: [RefHeader][msString][data + null terminator]
    size_t total_size = sizeof(msRefHeader) + sizeof(msString) + len + 1;
    void* mem = malloc(total_size);
    if (mem == NULL) return NULL;

    // Initialize RefHeader (matches orc.h msRefHeader)
    msRefHeader* header = (msRefHeader*)mem;
    header->rc = 1;
    header->flags.color = MS_COLOR_BLACK;
    header->flags.buffered = 0;
    header->flags._reserved = 0;
    ms_set_type_id(header, 0);  // Unknown type for strings

    // Initialize msString
    msString* str = (msString*)((char*)mem + sizeof(msRefHeader));
    str->len = len;
    str->capacity = len;

    // Initialize data buffer (after msString struct)
    str->data = (char*)mem + sizeof(msRefHeader) + sizeof(msString);

    // Copy data and add null terminator
    memcpy(str->data, data, len);
    str->data[len] = '\0';

    return str;
}

/*
 * Create empty string
 */
static inline msString* ms_string_empty(void) {
    return ms_string_new("", 0);
}

/*
 * Create string from C string literal (null-terminated)
 */
static inline msString* ms_string_from_cstr(const char* cstr) {
    return ms_string_new(cstr, strlen(cstr));
}

/*
 * Increment reference count
 */
static inline void ms_string_incref(msString* str) {
    if (str == NULL) return;
    msRefHeader* header = ms_string_get_header(str);
    header->rc++;
}

/*
 * Decrement reference count, free if rc=0
 */
static inline void ms_string_decref(msString* str) {
    if (str == NULL) return;

    msRefHeader* header = ms_string_get_header(str);

    #ifdef DEBUG
    if (header->rc == 0) {
        fprintf(stderr, "ERROR: ms_string_decref called on freed string\n");
        abort();
    }
    #endif

    header->rc--;

    if (header->rc == 0) {
        // Free entire allocation (header + string + data)
        free(header);
    }
}

/*
 * Get current reference count (for debugging/testing)
 */
static inline uint32_t ms_string_getrc(msString* str) {
    if (str == NULL) return 0;
    return ms_string_get_header(str)->rc;
}

/*
 * Concatenate two strings
 */
static inline msString* ms_string_concat(msString* a, msString* b) {
    if (a == NULL || b == NULL) return NULL;

    size_t new_len = a->len + b->len;

    // Allocate new string
    size_t total_size = sizeof(msRefHeader) + sizeof(msString) + new_len + 1;
    void* mem = malloc(total_size);
    if (mem == NULL) return NULL;

    // Initialize RefHeader (matches orc.h msRefHeader)
    msRefHeader* header = (msRefHeader*)mem;
    header->rc = 1;
    header->flags.color = MS_COLOR_BLACK;
    header->flags.buffered = 0;
    header->flags._reserved = 0;
    ms_set_type_id(header, 0);  // Unknown type for strings

    // Initialize msString
    msString* result = (msString*)((char*)mem + sizeof(msRefHeader));
    result->len = new_len;
    result->capacity = new_len;

    // Initialize data buffer
    result->data = (char*)mem + sizeof(msRefHeader) + sizeof(msString);

    // Copy both strings
    memcpy(result->data, a->data, a->len);
    memcpy(result->data + a->len, b->data, b->len);
    result->data[new_len] = '\0';

    return result;
}

/*
 * Extract substring [start..end)
 * Returns NULL if invalid range
 */
static inline msString* ms_string_substring(msString* str, size_t start, size_t end) {
    if (str == NULL) return NULL;
    if (start > end || end > str->len) return NULL;

    size_t sub_len = end - start;
    return ms_string_new(str->data + start, sub_len);
}

/*
 * Check equality
 */
static inline bool ms_string_equals(msString* a, msString* b) {
    if (a == NULL || b == NULL) return false;
    if (a->len != b->len) return false;
    return memcmp(a->data, b->data, a->len) == 0;
}

/*
 * Compare strings (for sorting)
 * Returns: -1 (a < b), 0 (a == b), 1 (a > b)
 */
static inline int ms_string_compare(msString* a, msString* b) {
    if (a == NULL || b == NULL) return 0;

    size_t min_len = (a->len < b->len) ? a->len : b->len;
    int cmp = memcmp(a->data, b->data, min_len);

    if (cmp != 0) return cmp;

    // Prefix match, compare lengths
    if (a->len < b->len) return -1;
    if (a->len > b->len) return 1;
    return 0;
}

/*
 * Check if string starts with prefix
 */
static inline bool ms_string_starts_with(msString* str, const char* prefix, size_t prefix_len) {
    if (str == NULL) return false;
    if (prefix_len > str->len) return false;
    return memcmp(str->data, prefix, prefix_len) == 0;
}

/*
 * Check if string ends with suffix
 */
static inline bool ms_string_ends_with(msString* str, const char* suffix, size_t suffix_len) {
    if (str == NULL) return false;
    if (suffix_len > str->len) return false;
    size_t start = str->len - suffix_len;
    return memcmp(str->data + start, suffix, suffix_len) == 0;
}

/*
 * Get length in bytes (NOT code points)
 */
static inline size_t ms_string_len(msString* str) {
    return (str != NULL) ? str->len : 0;
}

/*
 * Get C string pointer (null-terminated)
 * WARNING: Do NOT modify the returned pointer!
 */
static inline const char* ms_string_cstr(msString* str) {
    return (str != NULL) ? str->data : "";
}

/*
 * Clone: increment RC and return ptr
 */
static inline msString* ms_string_clone(msString* str) {
    ms_string_incref(str);
    return str;
}

/*
 * ORC-managed string concatenation helpers for codegen
 * These return ORC-managed msString* (use ms_decref for cleanup)
 */

// Concatenate msString + number → msString (ORC-managed)
static inline msString* ms_string_concat_num(msString* str, double num) {
    if (str == NULL) return NULL;

    char buf[64];
    snprintf(buf, sizeof(buf), "%g", num);
    size_t buf_len = strlen(buf);
    size_t new_len = str->len + buf_len;

    // Allocate new string with ORC header
    size_t total_size = sizeof(msRefHeader) + sizeof(msString) + new_len + 1;
    void* mem = malloc(total_size);
    if (mem == NULL) return NULL;

    // Initialize RefHeader
    msRefHeader* header = (msRefHeader*)mem;
    header->rc = 1;
    header->flags.color = MS_COLOR_BLACK;
    header->flags.buffered = 0;
    header->flags._reserved = 0;
    ms_set_type_id(header, 0);

    // Initialize msString
    msString* result = (msString*)((char*)mem + sizeof(msRefHeader));
    result->len = new_len;
    result->capacity = new_len;
    result->data = (char*)mem + sizeof(msRefHeader) + sizeof(msString);

    // Copy data
    memcpy(result->data, str->data, str->len);
    memcpy(result->data + str->len, buf, buf_len);
    result->data[new_len] = '\0';

    return result;
}

// Concatenate number + msString → msString (ORC-managed)
static inline msString* ms_num_concat_string(double num, msString* str) {
    if (str == NULL) return NULL;

    char buf[64];
    snprintf(buf, sizeof(buf), "%g", num);
    size_t buf_len = strlen(buf);
    size_t new_len = buf_len + str->len;

    // Allocate new string with ORC header
    size_t total_size = sizeof(msRefHeader) + sizeof(msString) + new_len + 1;
    void* mem = malloc(total_size);
    if (mem == NULL) return NULL;

    // Initialize RefHeader
    msRefHeader* header = (msRefHeader*)mem;
    header->rc = 1;
    header->flags.color = MS_COLOR_BLACK;
    header->flags.buffered = 0;
    header->flags._reserved = 0;
    ms_set_type_id(header, 0);

    // Initialize msString
    msString* result = (msString*)((char*)mem + sizeof(msRefHeader));
    result->len = new_len;
    result->capacity = new_len;
    result->data = (char*)mem + sizeof(msRefHeader) + sizeof(msString);

    // Copy data
    memcpy(result->data, buf, buf_len);
    memcpy(result->data + buf_len, str->data, str->len);
    result->data[new_len] = '\0';

    return result;
}

/*
 * Legacy C string concatenation helpers (deprecated - use msString versions above)
 * These return heap-allocated C strings (caller must free with free())
 */

// Concatenate two C strings (legacy - returns raw char*)
static inline char* ms_cstr_concat(const char* a, const char* b) {
    size_t len_a = strlen(a);
    size_t len_b = strlen(b);
    char* result = (char*)malloc(len_a + len_b + 1);
    if (result) {
        memcpy(result, a, len_a);
        memcpy(result + len_a, b, len_b + 1);
    }
    return result;
}

// Concatenate C string + number (legacy)
static inline char* ms_cstr_concat_num(const char* str, double num) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", num);
    return ms_cstr_concat(str, buf);
}

// Concatenate number + C string (legacy)
static inline char* ms_num_concat_cstr(double num, const char* str) {
    char buf[64];
    snprintf(buf, sizeof(buf), "%g", num);
    return ms_cstr_concat(buf, str);
}

/*
 * Sink: transfer ownership (no RC change, just return ptr)
 */
static inline msString* ms_string_sink(msString* str) {
    return str; // No-op, ownership transferred
}

#ifdef __cplusplus
}
#endif

#endif /* METASCRIPT_STRING_H */
