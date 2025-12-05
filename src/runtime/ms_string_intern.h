/*
 * Metascript String Interning
 *
 * Deduplicates string literals at compile time and runtime.
 * Identical strings share the same memory allocation.
 *
 * Benefits:
 *   - Memory savings: N identical "hello" → 1 allocation
 *   - O(1) equality: pointer comparison instead of memcmp
 *   - Zero RC overhead: interned strings are immortal (never freed)
 *
 * Usage patterns:
 *
 * 1. Compile-time interning (codegen emits):
 *    static msInternedString _str_hello = MS_INTERN_INIT("hello", 5);
 *    msString* s = ms_intern_get(&_str_hello);
 *
 * 2. Runtime interning (for dynamic strings):
 *    msString* s = ms_intern("hello");  // Returns cached or creates new
 *
 * 3. Fast equality:
 *    if (ms_intern_eq(a, b)) { ... }  // Pointer compare if both interned
 *
 * Memory model:
 *   - Interned strings have rc = UINT32_MAX (immortal)
 *   - Never freed during program lifetime
 *   - Stored in global hash table for deduplication
 */

#ifndef METASCRIPT_STRING_INTERN_H
#define METASCRIPT_STRING_INTERN_H

#include "ms_string.h"
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Marker for immortal (interned) strings */
#define MS_INTERN_IMMORTAL_RC UINT32_MAX

/* Initial intern table size (power of 2) */
#define MS_INTERN_TABLE_INITIAL_SIZE 256

/* Load factor threshold for rehashing (75%) */
#define MS_INTERN_LOAD_FACTOR 0.75

/*
 * Compile-time interned string literal
 * Used by codegen for static string deduplication
 */
typedef struct {
    const char* data;       /* String literal pointer */
    size_t len;             /* Length (excluding null terminator) */
    msString* cached;       /* Lazily initialized msString* */
} msInternedString;

/* Macro for static initialization */
#define MS_INTERN_INIT(str, length) { .data = (str), .len = (length), .cached = NULL }

/* Convenience macro that computes length */
#define MS_INTERN_LITERAL(str) MS_INTERN_INIT(str, sizeof(str) - 1)

/*
 * Global intern table entry
 */
typedef struct msInternEntry {
    msString* str;              /* The interned string */
    uint64_t hash;              /* Cached hash value */
    struct msInternEntry* next; /* Chain for collision handling */
} msInternEntry;

/*
 * Global intern table
 */
typedef struct {
    msInternEntry** buckets;    /* Hash buckets */
    size_t num_buckets;         /* Number of buckets */
    size_t count;               /* Number of interned strings */
    bool initialized;           /* Whether table is initialized */
} msInternTable;

/* Global intern table instance */
static msInternTable ms_intern_table = {
    .buckets = NULL,
    .num_buckets = 0,
    .count = 0,
    .initialized = false
};

/*
 * FNV-1a hash function (fast, good distribution)
 */
static inline uint64_t ms_intern_hash(const char* data, size_t len) {
    uint64_t hash = 14695981039346656037ULL;  /* FNV offset basis */
    for (size_t i = 0; i < len; i++) {
        hash ^= (uint64_t)(unsigned char)data[i];
        hash *= 1099511628211ULL;  /* FNV prime */
    }
    return hash;
}

/*
 * Initialize intern table (called lazily on first intern)
 */
static inline void ms_intern_init_table(void) {
    if (ms_intern_table.initialized) return;

    ms_intern_table.num_buckets = MS_INTERN_TABLE_INITIAL_SIZE;
    ms_intern_table.buckets = (msInternEntry**)calloc(
        ms_intern_table.num_buckets, sizeof(msInternEntry*));
    ms_intern_table.count = 0;
    ms_intern_table.initialized = true;
}

/*
 * Create an immortal (interned) string
 */
static inline msString* ms_intern_create(const char* data, size_t len) {
    msString* str = ms_string_new(data, len);
    if (str) {
        /* Mark as immortal - will never be freed */
        msRefHeader* header = ms_string_get_header(str);
        header->rc = MS_INTERN_IMMORTAL_RC;
    }
    return str;
}

/*
 * Resize intern table when load factor exceeded
 */
static inline void ms_intern_resize(void) {
    size_t old_size = ms_intern_table.num_buckets;
    size_t new_size = old_size * 2;
    msInternEntry** new_buckets = (msInternEntry**)calloc(new_size, sizeof(msInternEntry*));

    if (!new_buckets) return;  /* Keep old table on allocation failure */

    /* Rehash all entries */
    for (size_t i = 0; i < old_size; i++) {
        msInternEntry* entry = ms_intern_table.buckets[i];
        while (entry) {
            msInternEntry* next = entry->next;
            size_t new_idx = entry->hash & (new_size - 1);
            entry->next = new_buckets[new_idx];
            new_buckets[new_idx] = entry;
            entry = next;
        }
    }

    free(ms_intern_table.buckets);
    ms_intern_table.buckets = new_buckets;
    ms_intern_table.num_buckets = new_size;
}

/*
 * Intern a string (runtime)
 * Returns existing interned string or creates new one
 */
static inline msString* ms_intern(const char* data, size_t len) {
    if (!ms_intern_table.initialized) {
        ms_intern_init_table();
    }

    uint64_t hash = ms_intern_hash(data, len);
    size_t idx = hash & (ms_intern_table.num_buckets - 1);

    /* Search for existing entry */
    msInternEntry* entry = ms_intern_table.buckets[idx];
    while (entry) {
        if (entry->hash == hash &&
            entry->str->len == len &&
            memcmp(entry->str->data, data, len) == 0) {
            return entry->str;  /* Found existing */
        }
        entry = entry->next;
    }

    /* Check load factor */
    if ((double)ms_intern_table.count / ms_intern_table.num_buckets > MS_INTERN_LOAD_FACTOR) {
        ms_intern_resize();
        idx = hash & (ms_intern_table.num_buckets - 1);  /* Recalculate index */
    }

    /* Create new interned string */
    msString* str = ms_intern_create(data, len);
    if (!str) return NULL;

    /* Add to table */
    entry = (msInternEntry*)malloc(sizeof(msInternEntry));
    if (!entry) {
        /* Can't add to table, but string is still valid (just not interned) */
        return str;
    }

    entry->str = str;
    entry->hash = hash;
    entry->next = ms_intern_table.buckets[idx];
    ms_intern_table.buckets[idx] = entry;
    ms_intern_table.count++;

    return str;
}

/*
 * Intern from C string literal
 */
static inline msString* ms_intern_cstr(const char* cstr) {
    return ms_intern(cstr, strlen(cstr));
}

/*
 * Get interned string from compile-time literal (lazy init)
 * Used by codegen for static string deduplication
 */
static inline msString* ms_intern_get(msInternedString* lit) {
    if (lit->cached == NULL) {
        lit->cached = ms_intern(lit->data, lit->len);
    }
    return lit->cached;
}

/*
 * Check if a string is interned (immortal)
 */
static inline bool ms_string_is_interned(msString* str) {
    if (str == NULL) return false;
    return ms_string_get_header(str)->rc == MS_INTERN_IMMORTAL_RC;
}

/*
 * Fast equality check for potentially interned strings
 * If both are interned, uses pointer comparison (O(1))
 * Otherwise falls back to memcmp
 */
static inline bool ms_intern_eq(msString* a, msString* b) {
    if (a == b) return true;  /* Same pointer = definitely equal */
    if (a == NULL || b == NULL) return false;

    /* If both interned from same table, pointer equality is sufficient */
    if (ms_string_is_interned(a) && ms_string_is_interned(b)) {
        return false;  /* Different pointers + both interned = different strings */
    }

    /* Fall back to content comparison */
    return ms_string_equals(a, b);
}

/*
 * Get intern table statistics (for debugging/profiling)
 */
typedef struct {
    size_t total_strings;
    size_t total_buckets;
    size_t used_buckets;
    size_t max_chain_length;
    size_t total_bytes;
} msInternStats;

static inline msInternStats ms_intern_stats(void) {
    msInternStats stats = {0};

    if (!ms_intern_table.initialized) return stats;

    stats.total_strings = ms_intern_table.count;
    stats.total_buckets = ms_intern_table.num_buckets;

    for (size_t i = 0; i < ms_intern_table.num_buckets; i++) {
        msInternEntry* entry = ms_intern_table.buckets[i];
        if (entry) {
            stats.used_buckets++;
            size_t chain_len = 0;
            while (entry) {
                chain_len++;
                stats.total_bytes += entry->str->len;
                entry = entry->next;
            }
            if (chain_len > stats.max_chain_length) {
                stats.max_chain_length = chain_len;
            }
        }
    }

    return stats;
}

/*
 * Cleanup intern table (call at program exit if needed)
 * Note: Interned strings are immortal, so this just frees the table structure
 */
static inline void ms_intern_cleanup(void) {
    if (!ms_intern_table.initialized) return;

    for (size_t i = 0; i < ms_intern_table.num_buckets; i++) {
        msInternEntry* entry = ms_intern_table.buckets[i];
        while (entry) {
            msInternEntry* next = entry->next;
            /* Note: We don't free the strings themselves (immortal) */
            /* In a full cleanup, you'd set rc=1 and decref */
            free(entry);
            entry = next;
        }
    }

    free(ms_intern_table.buckets);
    ms_intern_table.buckets = NULL;
    ms_intern_table.num_buckets = 0;
    ms_intern_table.count = 0;
    ms_intern_table.initialized = false;
}

/*
 * ============================================================================
 * Codegen helpers - for compile-time string literal pooling
 * ============================================================================
 *
 * The compiler generates code like:
 *
 *   // String literal pool (top of generated file)
 *   static msInternedString _lit_0 = MS_INTERN_LITERAL("hello");
 *   static msInternedString _lit_1 = MS_INTERN_LITERAL("world");
 *
 *   // Usage in code
 *   msString* s1 = ms_intern_get(&_lit_0);
 *   msString* s2 = ms_intern_get(&_lit_0);  // Same pointer as s1!
 *
 * This achieves:
 *   1. Compile-time deduplication (same literal = same _lit_N)
 *   2. Lazy runtime initialization (only created when first used)
 *   3. Zero RC overhead (immortal strings)
 */

#ifdef __cplusplus
}
#endif

#endif /* METASCRIPT_STRING_INTERN_H */
