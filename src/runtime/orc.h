/*
 * Metascript ORC (Owned Reference Counting) Runtime
 * C API Header for codegen backends
 *
 * Memory Layout:
 *   [RefHeader (8 bytes)][User Data]
 *
 * RefHeader:
 *   - rc (4 bytes): Reference count
 *   - rootIdx (4 bytes): Index in cycle collector roots (0xFFFFFFFF = not in roots)
 *
 * Usage:
 *   User* user = (User*)ms_alloc(sizeof(User));  // rc=1
 *   ms_incref(user);                              // rc=2
 *   ms_decref(user);                              // rc=1
 *   ms_decref(user);                              // rc=0, freed
 */

#ifndef METASCRIPT_ORC_H
#define METASCRIPT_ORC_H

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Reference Header (8 bytes, always placed before user data)
 */
typedef struct {
    uint32_t rc;        /* Reference count (0 = freed) */
    uint32_t root_idx;  /* Index in cycle collector roots (0xFFFFFFFF = not in roots) */
} msRefHeader;

#define MS_NOT_IN_ROOTS 0xFFFFFFFF

/*
 * Get RefHeader from user data pointer
 */
static inline msRefHeader* ms_get_header(void* ptr) {
    return (msRefHeader*)((char*)ptr - sizeof(msRefHeader));
}

/*
 * Allocate memory with ORC header
 * Returns pointer to user data (header is placed before it)
 * Initial rc=1 (owned by creator)
 */
static inline void* ms_alloc(size_t size) {
    // Allocate: [RefHeader][User Data]
    void* mem = malloc(sizeof(msRefHeader) + size);
    if (mem == NULL) {
        return NULL; // Allocation failed
    }

    // Initialize header
    msRefHeader* header = (msRefHeader*)mem;
    header->rc = 1;
    header->root_idx = MS_NOT_IN_ROOTS;

    // Return pointer to user data (after header)
    return (char*)mem + sizeof(msRefHeader);
}

/*
 * Increment reference count
 */
static inline void ms_incref(void* ptr) {
    if (ptr == NULL) return;

    msRefHeader* header = ms_get_header(ptr);
    header->rc++;
}

/*
 * Decrement reference count, free if rc=0
 * IMPORTANT: Do NOT call on already-freed pointers (undefined behavior)
 */
static inline void ms_decref(void* ptr) {
    if (ptr == NULL) return;

    msRefHeader* header = ms_get_header(ptr);

    // Debug assertion: catch double-free
    #ifdef DEBUG
    if (header->rc == 0) {
        fprintf(stderr, "ERROR: ms_decref called on freed pointer\n");
        abort();
    }
    #endif

    header->rc--;

    if (header->rc == 0) {
        // TODO: If in cycle collector roots, remove from roots

        // Free entire allocation (header + data)
        free(header);
    }
}

/*
 * Get current reference count (for debugging/testing)
 */
static inline uint32_t ms_getrc(void* ptr) {
    if (ptr == NULL) return 0;

    msRefHeader* header = ms_get_header(ptr);
    return header->rc;
}

/*
 * Check if object is in cycle collector roots
 */
static inline int ms_is_in_roots(void* ptr) {
    if (ptr == NULL) return 0;

    msRefHeader* header = ms_get_header(ptr);
    return header->root_idx != MS_NOT_IN_ROOTS;
}

/*
 * Sink: transfer ownership (no RC change, just return ptr)
 * Used for move semantics: result = ms_sink(value);
 */
static inline void* ms_sink(void* ptr) {
    return ptr; // No-op, ownership transferred
}

/*
 * Clone: increment RC and return ptr
 * Used when creating alias: result = ms_clone(value);
 */
static inline void* ms_clone(void* ptr) {
    ms_incref(ptr);
    return ptr;
}

#ifdef __cplusplus
}
#endif

#endif /* METASCRIPT_ORC_H */
