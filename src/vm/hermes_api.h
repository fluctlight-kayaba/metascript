/**
 * Hermes C API for Metascript Macro VM
 * Minimal subset for compile-time macro execution
 *
 * Based on Vimcraft's Hermes integration
 */

#ifndef METASCRIPT_HERMES_API_H
#define METASCRIPT_HERMES_API_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque types
typedef struct MSHermesRuntime MSHermesRuntime;
typedef struct MSHermesValue MSHermesValue;

// Host function callback (Zig -> JS -> Zig calls)
typedef MSHermesValue* (*MSHostFunction)(
    MSHermesRuntime* runtime,
    void* context,
    MSHermesValue** args,
    size_t arg_count
);

// HostObject callbacks (for vim.ast.* style APIs)
typedef MSHermesValue* (*MSHostObjectGet)(
    MSHermesRuntime* runtime,
    void* context,
    const char* prop_name
);

typedef MSHermesValue* (*MSHostObjectSet)(
    MSHermesRuntime* runtime,
    void* context,
    const char* prop_name,
    MSHermesValue* value
);

//
// Runtime Lifecycle
//

MSHermesRuntime* ms_hermes_create(void);
void ms_hermes_destroy(MSHermesRuntime* runtime);

//
// JavaScript Execution
//

// Execute JavaScript source code
MSHermesValue* ms_hermes_eval(
    MSHermesRuntime* runtime,
    const char* source,
    size_t source_len,
    const char* filename
);

// Execute pre-compiled Hermes bytecode (HBC)
// Returns null on error (check ms_hermes_has_exception)
MSHermesValue* ms_hermes_eval_bytecode(
    MSHermesRuntime* runtime,
    const uint8_t* bytecode,
    size_t bytecode_len,
    const char* source_url
);

bool ms_hermes_has_exception(MSHermesRuntime* runtime);
const char* ms_hermes_get_exception(MSHermesRuntime* runtime);
void ms_hermes_clear_exception(MSHermesRuntime* runtime);

//
// Host Function/Object Registration
//

void ms_hermes_register_function(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostFunction callback,
    void* context
);

void ms_hermes_register_host_object(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostObjectGet getter,
    MSHostObjectSet setter,
    void* context
);

MSHermesValue* ms_hermes_create_function(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostFunction callback,
    void* context
);

//
// Value Operations
//

typedef enum {
    MS_TYPE_UNDEFINED,
    MS_TYPE_NULL,
    MS_TYPE_BOOLEAN,
    MS_TYPE_NUMBER,
    MS_TYPE_STRING,
    MS_TYPE_OBJECT,
    MS_TYPE_ARRAY,
} MSValueType;

MSValueType ms_value_type(MSHermesValue* value);
bool ms_value_is_string(MSHermesValue* value);
bool ms_value_is_number(MSHermesValue* value);
bool ms_value_is_object(MSHermesValue* value);
bool ms_value_is_array(MSHermesRuntime* runtime, MSHermesValue* value);

// Extract values
bool ms_value_get_bool(MSHermesValue* value);
double ms_value_get_number(MSHermesValue* value);
const char* ms_value_get_string(MSHermesRuntime* runtime, MSHermesValue* value, size_t* len);

// Create values
MSHermesValue* ms_value_undefined(MSHermesRuntime* runtime);
MSHermesValue* ms_value_null(MSHermesRuntime* runtime);
MSHermesValue* ms_value_bool(MSHermesRuntime* runtime, bool value);
MSHermesValue* ms_value_number(MSHermesRuntime* runtime, double value);
MSHermesValue* ms_value_string(MSHermesRuntime* runtime, const char* str, size_t len);
MSHermesValue* ms_value_object(MSHermesRuntime* runtime);

// Object operations
MSHermesValue* ms_object_get(MSHermesRuntime* runtime, MSHermesValue* obj, const char* key);
void ms_object_set(MSHermesRuntime* runtime, MSHermesValue* obj, const char* key, MSHermesValue* val);

// Array operations
MSHermesValue* ms_array_create(MSHermesRuntime* runtime, size_t len);
size_t ms_array_length(MSHermesRuntime* runtime, MSHermesValue* arr);
MSHermesValue* ms_array_get(MSHermesRuntime* runtime, MSHermesValue* arr, size_t idx);
void ms_array_set(MSHermesRuntime* runtime, MSHermesValue* arr, size_t idx, MSHermesValue* val);
void ms_array_push(MSHermesRuntime* runtime, MSHermesValue* arr, MSHermesValue* val);

// Call function
MSHermesValue* ms_call(MSHermesRuntime* runtime, MSHermesValue* func, MSHermesValue** args, size_t argc);

// Memory
void ms_value_destroy(MSHermesValue* value);

#ifdef __cplusplus
}
#endif

#endif // METASCRIPT_HERMES_API_H
