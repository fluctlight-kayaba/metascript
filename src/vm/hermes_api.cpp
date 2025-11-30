/**
 * Hermes C API Implementation for Metascript
 * Minimal subset for compile-time macro execution
 *
 * Based on Vimcraft's implementation, stripped to essentials
 */

#include "hermes_api.h"

#include <jsi/jsi.h>
#include <hermes/hermes.h>
#include <memory>
#include <string>
#include <map>
#include <vector>

using namespace facebook;
using namespace facebook::jsi;
using namespace facebook::hermes;

//
// BytecodeBuffer - Buffer implementation for pre-compiled HBC
//
class BytecodeBuffer : public Buffer {
    std::vector<uint8_t> data_;
public:
    explicit BytecodeBuffer(const uint8_t* data, size_t size)
        : data_(data, data + size) {}

    const uint8_t* data() const override {
        return data_.data();
    }

    size_t size() const override {
        return data_.size();
    }
};

//
// Internal Wrapper Structs
//

struct MSHermesRuntime {
    std::unique_ptr<facebook::hermes::HermesRuntime> runtime;
    std::string last_exception;
};

struct MSHermesValue {
    Value value;
    MSHermesValue(Value&& v) : value(std::move(v)) {}
};

//
// Host Function Context
//

struct MSHostFunctionContext {
    MSHermesRuntime* runtime;
    MSHostFunction callback;
    void* user_context;
};

static std::map<std::string, MSHostFunctionContext> g_host_functions;

//
// Runtime Lifecycle
//

extern "C" {

MSHermesRuntime* ms_hermes_create(void) {
    try {
        auto wrapper = new MSHermesRuntime();
        wrapper->runtime = makeHermesRuntime();
        if (!wrapper->runtime) {
            delete wrapper;
            return nullptr;
        }
        return wrapper;
    } catch (...) {
        return nullptr;
    }
}

void ms_hermes_destroy(MSHermesRuntime* runtime) {
    if (runtime) delete runtime;
}

//
// JavaScript Execution
//

MSHermesValue* ms_hermes_eval(
    MSHermesRuntime* runtime,
    const char* source,
    size_t source_len,
    const char* filename
) {
    if (!runtime || !source) return nullptr;

    try {
        std::string src(source, source_len);
        std::string url = filename ? filename : "<eval>";
        auto buffer = std::make_shared<StringBuffer>(std::move(src));
        Value result = runtime->runtime->evaluateJavaScript(buffer, url);
        return new MSHermesValue(std::move(result));
    } catch (const JSError& e) {
        runtime->last_exception = e.what();
        return nullptr;
    } catch (const std::exception& e) {
        runtime->last_exception = e.what();
        return nullptr;
    }
}

MSHermesValue* ms_hermes_eval_bytecode(
    MSHermesRuntime* runtime,
    const uint8_t* bytecode,
    size_t bytecode_len,
    const char* source_url
) {
    if (!runtime || !bytecode || bytecode_len == 0) return nullptr;

    try {
        std::string url = source_url ? source_url : "<bytecode>";

        // Create a BytecodeBuffer from the pre-compiled HBC
        // Hermes detects bytecode by checking for HBC magic number at start
        auto buffer = std::make_shared<BytecodeBuffer>(bytecode, bytecode_len);

        Value result = runtime->runtime->evaluateJavaScript(buffer, url);
        return new MSHermesValue(std::move(result));
    } catch (const JSError& e) {
        runtime->last_exception = e.what();
        return nullptr;
    } catch (const std::exception& e) {
        runtime->last_exception = e.what();
        return nullptr;
    }
}

bool ms_hermes_has_exception(MSHermesRuntime* runtime) {
    return runtime && !runtime->last_exception.empty();
}

const char* ms_hermes_get_exception(MSHermesRuntime* runtime) {
    if (!runtime || runtime->last_exception.empty()) return nullptr;
    return runtime->last_exception.c_str();
}

void ms_hermes_clear_exception(MSHermesRuntime* runtime) {
    if (runtime) runtime->last_exception.clear();
}

//
// Host Function Registration
//

void ms_hermes_register_function(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostFunction callback,
    void* context
) {
    if (!runtime || !name || !callback) return;

    try {
        std::string func_name(name);
        g_host_functions[func_name] = {runtime, callback, context};

        auto hostFunc = Function::createFromHostFunction(
            *runtime->runtime,
            PropNameID::forAscii(*runtime->runtime, name),
            0,
            [func_name](Runtime& rt, const Value& thisVal, const Value* args, size_t count) -> Value {
                auto it = g_host_functions.find(func_name);
                if (it == g_host_functions.end()) return Value::undefined();

                auto& ctx = it->second;
                std::vector<MSHermesValue*> arg_wrappers;
                arg_wrappers.reserve(count);

                for (size_t i = 0; i < count; i++) {
                    arg_wrappers.push_back(new MSHermesValue(Value(rt, args[i])));
                }

                MSHermesValue* result = ctx.callback(ctx.runtime, ctx.user_context, arg_wrappers.data(), count);

                for (auto* w : arg_wrappers) delete w;

                if (result) {
                    Value ret(rt, result->value);
                    delete result;
                    return ret;
                }
                return Value::undefined();
            }
        );

        runtime->runtime->global().setProperty(*runtime->runtime, name, std::move(hostFunc));
    } catch (...) {}
}

//
// HostObject Support
//

class CustomHostObject : public HostObject {
    MSHermesRuntime* runtime_;
    MSHostObjectGet getter_;
    MSHostObjectSet setter_;
    void* context_;

public:
    CustomHostObject(MSHermesRuntime* rt, MSHostObjectGet get, MSHostObjectSet set, void* ctx)
        : runtime_(rt), getter_(get), setter_(set), context_(ctx) {}

    Value get(Runtime& rt, const PropNameID& name) override {
        if (!getter_) return Value::undefined();
        std::string prop = name.utf8(rt);
        MSHermesValue* result = getter_(runtime_, context_, prop.c_str());
        if (result) {
            Value ret(rt, result->value);
            delete result;
            return ret;
        }
        return Value::undefined();
    }

    void set(Runtime& rt, const PropNameID& name, const Value& value) override {
        if (!setter_) return;
        std::string prop = name.utf8(rt);
        MSHermesValue val_wrapper(Value(rt, value));
        MSHermesValue* result = setter_(runtime_, context_, prop.c_str(), &val_wrapper);
        if (result) delete result;
    }
};

void ms_hermes_register_host_object(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostObjectGet getter,
    MSHostObjectSet setter,
    void* context
) {
    if (!runtime || !name || !getter) return;

    try {
        auto hostObj = std::make_shared<CustomHostObject>(runtime, getter, setter, context);
        runtime->runtime->global().setProperty(
            *runtime->runtime, name,
            Object::createFromHostObject(*runtime->runtime, hostObj)
        );
    } catch (...) {}
}

MSHermesValue* ms_hermes_create_function(
    MSHermesRuntime* runtime,
    const char* name,
    MSHostFunction callback,
    void* context
) {
    if (!runtime || !name || !callback) return nullptr;

    try {
        std::string func_name(name);
        g_host_functions[func_name] = {runtime, callback, context};

        auto hostFunc = Function::createFromHostFunction(
            *runtime->runtime,
            PropNameID::forAscii(*runtime->runtime, name),
            0,
            [func_name](Runtime& rt, const Value& thisVal, const Value* args, size_t count) -> Value {
                auto it = g_host_functions.find(func_name);
                if (it == g_host_functions.end()) return Value::undefined();

                auto& ctx = it->second;
                std::vector<MSHermesValue*> arg_wrappers;
                for (size_t i = 0; i < count; i++) {
                    arg_wrappers.push_back(new MSHermesValue(Value(rt, args[i])));
                }

                MSHermesValue* result = ctx.callback(ctx.runtime, ctx.user_context, arg_wrappers.data(), count);

                for (auto* w : arg_wrappers) delete w;

                if (result) {
                    Value ret(rt, result->value);
                    delete result;
                    return ret;
                }
                return Value::undefined();
            }
        );

        return new MSHermesValue(Value(*runtime->runtime, std::move(hostFunc)));
    } catch (...) {
        return nullptr;
    }
}

//
// Value Operations
//

MSValueType ms_value_type(MSHermesValue* value) {
    if (!value) return MS_TYPE_UNDEFINED;
    if (value->value.isUndefined()) return MS_TYPE_UNDEFINED;
    if (value->value.isNull()) return MS_TYPE_NULL;
    if (value->value.isBool()) return MS_TYPE_BOOLEAN;
    if (value->value.isNumber()) return MS_TYPE_NUMBER;
    if (value->value.isString()) return MS_TYPE_STRING;
    if (value->value.isObject()) return MS_TYPE_OBJECT;
    return MS_TYPE_UNDEFINED;
}

bool ms_value_is_string(MSHermesValue* value) {
    return value && value->value.isString();
}

bool ms_value_is_number(MSHermesValue* value) {
    return value && value->value.isNumber();
}

bool ms_value_is_object(MSHermesValue* value) {
    return value && value->value.isObject();
}

bool ms_value_is_array(MSHermesRuntime* runtime, MSHermesValue* value) {
    if (!runtime || !value || !value->value.isObject()) return false;
    try {
        return value->value.asObject(*runtime->runtime).isArray(*runtime->runtime);
    } catch (...) {
        return false;
    }
}

bool ms_value_get_bool(MSHermesValue* value) {
    return value && value->value.isBool() && value->value.getBool();
}

double ms_value_get_number(MSHermesValue* value) {
    return (value && value->value.isNumber()) ? value->value.getNumber() : 0.0;
}

thread_local std::string g_string_buf;

const char* ms_value_get_string(MSHermesRuntime* runtime, MSHermesValue* value, size_t* len) {
    if (!runtime || !value || !value->value.isString()) {
        if (len) *len = 0;
        return nullptr;
    }
    try {
        g_string_buf = value->value.getString(*runtime->runtime).utf8(*runtime->runtime);
        if (len) *len = g_string_buf.length();
        return g_string_buf.c_str();
    } catch (...) {
        if (len) *len = 0;
        return nullptr;
    }
}

MSHermesValue* ms_value_undefined(MSHermesRuntime* runtime) {
    return runtime ? new MSHermesValue(Value::undefined()) : nullptr;
}

MSHermesValue* ms_value_null(MSHermesRuntime* runtime) {
    return runtime ? new MSHermesValue(Value::null()) : nullptr;
}

MSHermesValue* ms_value_bool(MSHermesRuntime* runtime, bool value) {
    return runtime ? new MSHermesValue(Value(value)) : nullptr;
}

MSHermesValue* ms_value_number(MSHermesRuntime* runtime, double value) {
    return runtime ? new MSHermesValue(Value(value)) : nullptr;
}

MSHermesValue* ms_value_string(MSHermesRuntime* runtime, const char* str, size_t len) {
    if (!runtime || !str) return nullptr;
    try {
        return new MSHermesValue(Value(*runtime->runtime,
            String::createFromUtf8(*runtime->runtime, std::string(str, len))));
    } catch (...) {
        return nullptr;
    }
}

MSHermesValue* ms_value_object(MSHermesRuntime* runtime) {
    if (!runtime) return nullptr;
    try {
        return new MSHermesValue(Value(*runtime->runtime, Object(*runtime->runtime)));
    } catch (...) {
        return nullptr;
    }
}

MSHermesValue* ms_object_get(MSHermesRuntime* runtime, MSHermesValue* obj, const char* key) {
    if (!runtime || !obj || !key || !obj->value.isObject()) return nullptr;
    try {
        Value val = obj->value.asObject(*runtime->runtime).getProperty(*runtime->runtime, key);
        return new MSHermesValue(std::move(val));
    } catch (...) {
        return nullptr;
    }
}

void ms_object_set(MSHermesRuntime* runtime, MSHermesValue* obj, const char* key, MSHermesValue* val) {
    if (!runtime || !obj || !key || !val || !obj->value.isObject()) return;
    try {
        obj->value.asObject(*runtime->runtime).setProperty(
            *runtime->runtime, key, Value(*runtime->runtime, val->value));
    } catch (...) {}
}

//
// Array Operations
//

MSHermesValue* ms_array_create(MSHermesRuntime* runtime, size_t len) {
    if (!runtime) return nullptr;
    try {
        return new MSHermesValue(Value(*runtime->runtime, Array(*runtime->runtime, len)));
    } catch (...) {
        return nullptr;
    }
}

size_t ms_array_length(MSHermesRuntime* runtime, MSHermesValue* arr) {
    if (!runtime || !arr || !arr->value.isObject()) return 0;
    try {
        Object obj = arr->value.asObject(*runtime->runtime);
        if (!obj.isArray(*runtime->runtime)) return 0;
        return obj.getArray(*runtime->runtime).size(*runtime->runtime);
    } catch (...) {
        return 0;
    }
}

MSHermesValue* ms_array_get(MSHermesRuntime* runtime, MSHermesValue* arr, size_t idx) {
    if (!runtime || !arr || !arr->value.isObject()) return nullptr;
    try {
        Object obj = arr->value.asObject(*runtime->runtime);
        if (!obj.isArray(*runtime->runtime)) return nullptr;
        Array array = obj.getArray(*runtime->runtime);
        if (idx >= array.size(*runtime->runtime)) return nullptr;
        return new MSHermesValue(array.getValueAtIndex(*runtime->runtime, idx));
    } catch (...) {
        return nullptr;
    }
}

void ms_array_set(MSHermesRuntime* runtime, MSHermesValue* arr, size_t idx, MSHermesValue* val) {
    if (!runtime || !arr || !val || !arr->value.isObject()) return;
    try {
        Object obj = arr->value.asObject(*runtime->runtime);
        if (!obj.isArray(*runtime->runtime)) return;
        obj.getArray(*runtime->runtime).setValueAtIndex(
            *runtime->runtime, idx, Value(*runtime->runtime, val->value));
    } catch (...) {}
}

void ms_array_push(MSHermesRuntime* runtime, MSHermesValue* arr, MSHermesValue* val) {
    if (!runtime || !arr || !val) return;
    size_t len = ms_array_length(runtime, arr);
    ms_array_set(runtime, arr, len, val);
}

//
// Function Call
//

MSHermesValue* ms_call(MSHermesRuntime* runtime, MSHermesValue* func, MSHermesValue** args, size_t argc) {
    if (!runtime || !func || !func->value.isObject()) return nullptr;

    try {
        Runtime& rt = *runtime->runtime;
        if (!func->value.asObject(rt).isFunction(rt)) return nullptr;

        Function f = func->value.asObject(rt).getFunction(rt);
        std::vector<Value> argVals;
        argVals.reserve(argc);

        for (size_t i = 0; i < argc; i++) {
            if (args && args[i]) {
                argVals.emplace_back(Value(rt, args[i]->value));
            } else {
                argVals.emplace_back(Value::undefined());
            }
        }

        Value result = f.call(rt, static_cast<const Value*>(argVals.data()), argVals.size());
        return new MSHermesValue(std::move(result));
    } catch (const JSError& e) {
        runtime->last_exception = e.getMessage();
        return nullptr;
    } catch (const std::exception& e) {
        runtime->last_exception = e.what();
        return nullptr;
    }
}

void ms_value_destroy(MSHermesValue* value) {
    if (value) delete value;
}

} // extern "C"
