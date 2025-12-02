/*
 * Metascript String C API Test
 * Verifies that the C header works correctly
 */

#include "string.h"
#include <stdio.h>
#include <assert.h>

void test_new_and_free() {
    printf("test_new_and_free...");

    msString* s = ms_string_new("hello", 5);
    assert(s != NULL);
    assert(ms_string_getrc(s) == 1);
    assert(ms_string_len(s) == 5);
    assert(strcmp(ms_string_cstr(s), "hello") == 0);

    ms_string_decref(s);

    printf("OK\n");
}

void test_empty_string() {
    printf("test_empty_string...");

    msString* s = ms_string_empty();
    assert(s != NULL);
    assert(ms_string_len(s) == 0);
    assert(strcmp(ms_string_cstr(s), "") == 0);

    ms_string_decref(s);

    printf("OK\n");
}

void test_from_cstr() {
    printf("test_from_cstr...");

    msString* s = ms_string_from_cstr("hello world");
    assert(s != NULL);
    assert(ms_string_len(s) == 11);
    assert(strcmp(ms_string_cstr(s), "hello world") == 0);

    ms_string_decref(s);

    printf("OK\n");
}

void test_incref_decref() {
    printf("test_incref_decref...");

    msString* s = ms_string_new("test", 4);
    assert(ms_string_getrc(s) == 1);

    ms_string_incref(s);
    assert(ms_string_getrc(s) == 2);

    ms_string_incref(s);
    assert(ms_string_getrc(s) == 3);

    ms_string_decref(s);
    assert(ms_string_getrc(s) == 2);

    ms_string_decref(s);
    assert(ms_string_getrc(s) == 1);

    ms_string_decref(s); // Freed

    printf("OK\n");
}

void test_concat() {
    printf("test_concat...");

    msString* a = ms_string_new("hello", 5);
    msString* b = ms_string_new(" world", 6);

    msString* result = ms_string_concat(a, b);
    assert(result != NULL);
    assert(ms_string_len(result) == 11);
    assert(strcmp(ms_string_cstr(result), "hello world") == 0);
    assert(ms_string_getrc(result) == 1);

    ms_string_decref(a);
    ms_string_decref(b);
    ms_string_decref(result);

    printf("OK\n");
}

void test_concat_with_empty() {
    printf("test_concat_with_empty...");

    msString* a = ms_string_new("hello", 5);
    msString* b = ms_string_empty();

    msString* result = ms_string_concat(a, b);
    assert(strcmp(ms_string_cstr(result), "hello") == 0);

    ms_string_decref(a);
    ms_string_decref(b);
    ms_string_decref(result);

    printf("OK\n");
}

void test_substring() {
    printf("test_substring...");

    msString* s = ms_string_new("hello world", 11);

    msString* sub = ms_string_substring(s, 0, 5);
    assert(sub != NULL);
    assert(strcmp(ms_string_cstr(sub), "hello") == 0);
    assert(ms_string_getrc(sub) == 1);

    ms_string_decref(s);
    ms_string_decref(sub);

    printf("OK\n");
}

void test_substring_invalid_range() {
    printf("test_substring_invalid_range...");

    msString* s = ms_string_new("hello", 5);

    // start > end
    msString* sub1 = ms_string_substring(s, 5, 2);
    assert(sub1 == NULL);

    // end > len
    msString* sub2 = ms_string_substring(s, 0, 10);
    assert(sub2 == NULL);

    ms_string_decref(s);

    printf("OK\n");
}

void test_equals() {
    printf("test_equals...");

    msString* a = ms_string_new("hello", 5);
    msString* b = ms_string_new("hello", 5);
    msString* c = ms_string_new("world", 5);

    assert(ms_string_equals(a, b));
    assert(!ms_string_equals(a, c));

    ms_string_decref(a);
    ms_string_decref(b);
    ms_string_decref(c);

    printf("OK\n");
}

void test_compare() {
    printf("test_compare...");

    msString* a = ms_string_new("apple", 5);
    msString* b = ms_string_new("banana", 6);

    assert(ms_string_compare(a, b) < 0); // apple < banana
    assert(ms_string_compare(b, a) > 0); // banana > apple
    assert(ms_string_compare(a, a) == 0); // apple == apple

    ms_string_decref(a);
    ms_string_decref(b);

    printf("OK\n");
}

void test_starts_with() {
    printf("test_starts_with...");

    msString* s = ms_string_new("hello world", 11);

    assert(ms_string_starts_with(s, "hello", 5));
    assert(!ms_string_starts_with(s, "world", 5));
    assert(ms_string_starts_with(s, "", 0));

    ms_string_decref(s);

    printf("OK\n");
}

void test_ends_with() {
    printf("test_ends_with...");

    msString* s = ms_string_new("hello world", 11);

    assert(ms_string_ends_with(s, "world", 5));
    assert(!ms_string_ends_with(s, "hello", 5));
    assert(ms_string_ends_with(s, "", 0));

    ms_string_decref(s);

    printf("OK\n");
}

void test_null_safety() {
    printf("test_null_safety...");

    // All operations should handle NULL gracefully
    ms_string_incref(NULL);
    ms_string_decref(NULL);
    assert(ms_string_getrc(NULL) == 0);
    assert(ms_string_len(NULL) == 0);
    assert(strcmp(ms_string_cstr(NULL), "") == 0);

    msString* s = ms_string_clone(NULL);
    assert(s == NULL);

    s = ms_string_sink(NULL);
    assert(s == NULL);

    printf("OK\n");
}

void test_clone_and_sink() {
    printf("test_clone_and_sink...");

    msString* s = ms_string_new("test", 4);
    assert(ms_string_getrc(s) == 1);

    // Clone: increment RC
    msString* s2 = ms_string_clone(s);
    assert(s2 == s); // Same pointer
    assert(ms_string_getrc(s) == 2);

    // Sink: no RC change (move semantics)
    msString* s3 = ms_string_sink(s);
    assert(s3 == s); // Same pointer
    assert(ms_string_getrc(s) == 2); // No change

    ms_string_decref(s2);
    ms_string_decref(s3);

    printf("OK\n");
}

int main() {
    printf("===================================================\n");
    printf("Metascript String C API Tests\n");
    printf("===================================================\n\n");

    test_new_and_free();
    test_empty_string();
    test_from_cstr();
    test_incref_decref();
    test_concat();
    test_concat_with_empty();
    test_substring();
    test_substring_invalid_range();
    test_equals();
    test_compare();
    test_starts_with();
    test_ends_with();
    test_null_safety();
    test_clone_and_sink();

    printf("\n===================================================\n");
    printf("All tests passed!\n");
    printf("===================================================\n");

    return 0;
}
