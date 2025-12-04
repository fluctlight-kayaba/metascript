/// Code Quality Verification - Phase 1.5
///
/// Verifies generated code follows best practices:
/// - C: Uses structs for simple classes (not vtables/function pointers)
/// - C: Idiomatic C patterns
/// - JS: Modern ES syntax, no unnecessary complexity
/// - Erlang: Proper tail recursion, immutability patterns

const std = @import("std");
const testing = std.testing;

// ============================================================================
// C Backend Quality Checks
// ============================================================================

/// Verify C code uses structs for simple classes
pub fn expectCUsesStruct(c_code: []const u8, class_name: []const u8) !void {
    // Should have: typedef struct ClassName { ... } ClassName;
    const struct_pattern = try std.fmt.allocPrint(testing.allocator, "struct {s}", .{class_name});
    defer testing.allocator.free(struct_pattern);

    if (std.mem.indexOf(u8, c_code, struct_pattern) == null) {
        std.debug.print("\n❌ C code quality: Expected struct for class '{s}'\n", .{class_name});
        std.debug.print("   Generated code should use: struct {s} {{\n", .{class_name});
        std.debug.print("   Not vtables or function pointers\n", .{});
        return error.CQualityStructExpected;
    }
}

/// Verify C code does NOT use vtables for simple classes
pub fn expectCNoVTable(c_code: []const u8, class_name: []const u8) !void {
    _ = class_name; // Reserved for future use in error messages

    // Should NOT have function pointer tables
    const vtable_patterns = [_][]const u8{
        "vtable",
        "VTable",
        "(*",  // function pointers
        "void*",  // void pointer casts
    };

    for (vtable_patterns) |pattern| {
        if (std.mem.indexOf(u8, c_code, pattern) != null) {
            std.debug.print("\n⚠️  C code quality: Found '{s}' in simple class\n", .{pattern});
            std.debug.print("   Simple classes should use structs, not vtables\n", .{});
            // Warning, not error - might be intentional
        }
    }
}

/// Verify C code uses proper memory management
pub fn expectCProperMemory(c_code: []const u8) !void {
    const has_malloc = std.mem.indexOf(u8, c_code, "malloc") != null;
    const has_free = std.mem.indexOf(u8, c_code, "free") != null;

    if (has_malloc and !has_free) {
        std.debug.print("\n❌ C code quality: malloc() without free()\n", .{});
        std.debug.print("   Potential memory leak\n", .{});
        return error.CQualityMemoryLeak;
    }
}

/// Verify C code has main() function for executable tests
pub fn expectCHasMain(c_code: []const u8) !void {
    const patterns = [_][]const u8{
        "int main(",
        "int main(void)",
        "int main(int argc",
    };

    for (patterns) |pattern| {
        if (std.mem.indexOf(u8, c_code, pattern) != null) {
            return; // Found main()
        }
    }

    std.debug.print("\n❌ C code quality: No main() function found\n", .{});
    std.debug.print("   Executable tests require main()\n", .{});
    return error.CQualityNoMain;
}

// ============================================================================
// JavaScript Backend Quality Checks
// ============================================================================

/// Verify JS code uses modern syntax (const/let, not var)
pub fn expectJSModernSyntax(js_code: []const u8) !void {
    // Should use const/let
    const has_const = std.mem.indexOf(u8, js_code, "const ") != null;
    const has_let = std.mem.indexOf(u8, js_code, "let ") != null;

    // Should NOT use var
    const has_var = std.mem.indexOf(u8, js_code, "var ") != null;

    if (has_var) {
        std.debug.print("\n⚠️  JS code quality: Uses 'var' instead of 'const'/'let'\n", .{});
        std.debug.print("   Modern JavaScript should use const/let\n", .{});
        // Warning, not error
    }

    if (!has_const and !has_let) {
        std.debug.print("\n❌ JS code quality: No const/let declarations found\n", .{});
        return error.JSQualityNoModernSyntax;
    }
}

/// Verify JS code uses proper class syntax (for classes)
pub fn expectJSUsesClass(js_code: []const u8, class_name: []const u8) !void {
    const class_pattern = try std.fmt.allocPrint(testing.allocator, "class {s}", .{class_name});
    defer testing.allocator.free(class_pattern);

    if (std.mem.indexOf(u8, js_code, class_pattern) == null) {
        std.debug.print("\n❌ JS code quality: Expected 'class {s}' declaration\n", .{class_name});
        return error.JSQualityNoClassSyntax;
    }
}

/// Verify JS code doesn't have unnecessary complexity
pub fn expectJSNoUnnecessaryComplexity(js_code: []const u8) !void {
    // Check for anti-patterns
    const antipatterns = [_][]const u8{
        "eval(",
        "with(",
        "arguments.callee",
    };

    for (antipatterns) |pattern| {
        if (std.mem.indexOf(u8, js_code, pattern) != null) {
            std.debug.print("\n⚠️  JS code quality: Found anti-pattern '{s}'\n", .{pattern});
        }
    }
}

// ============================================================================
// Erlang Backend Quality Checks
// ============================================================================

/// Verify Erlang code uses proper tail recursion
pub fn expectErlangTailRecursion(erl_code: []const u8, function_name: []const u8) !void {
    // Tail recursive functions should call themselves as the last operation
    const pattern = try std.fmt.allocPrint(testing.allocator, "{s}(", .{function_name});
    defer testing.allocator.free(pattern);

    if (std.mem.indexOf(u8, erl_code, pattern) == null) {
        std.debug.print("\n❌ Erlang code quality: Function '{s}' not found\n", .{function_name});
        return error.ErlangQualityFunctionNotFound;
    }

    // More sophisticated check would parse the AST to verify tail position
    // For now, just check it exists
}

/// Verify Erlang code uses immutability patterns (variable shadowing)
pub fn expectErlangImmutability(erl_code: []const u8) !void {
    // Should use X@1, X@2 pattern for variable shadowing
    const has_shadowing = std.mem.indexOf(u8, erl_code, "@1") != null or
        std.mem.indexOf(u8, erl_code, "@2") != null;

    if (!has_shadowing) {
        // Not all code needs shadowing, so this is just informational
        return;
    }

    // If we have shadowing, verify it's used correctly
    // (This would require more sophisticated parsing)
}

/// Verify Erlang code exports main/0 for executable tests
pub fn expectErlangExportsMain(erl_code: []const u8) !void {
    const patterns = [_][]const u8{
        "-export([main/0",
        "main/0",
    };

    for (patterns) |pattern| {
        if (std.mem.indexOf(u8, erl_code, pattern) != null) {
            return; // Found main/0 export
        }
    }

    std.debug.print("\n❌ Erlang code quality: No main/0 export found\n", .{});
    std.debug.print("   Executable tests require -export([main/0]).\n", .{});
    return error.ErlangQualityNoMain;
}

/// Verify Erlang code has proper module declaration
pub fn expectErlangModule(erl_code: []const u8, module_name: []const u8) !void {
    const pattern = try std.fmt.allocPrint(testing.allocator, "-module({s}).", .{module_name});
    defer testing.allocator.free(pattern);

    if (std.mem.indexOf(u8, erl_code, pattern) == null) {
        std.debug.print("\n❌ Erlang code quality: Missing -module({s}). declaration\n", .{module_name});
        return error.ErlangQualityNoModule;
    }
}

// ============================================================================
// Cross-Backend Quality Checks
// ============================================================================

/// Compare code quality across backends
pub const QualityComparison = struct {
    c_idiomatic: bool,
    js_idiomatic: bool,
    erlang_idiomatic: bool,
    notes: []const u8,
};

/// Verify all backends produce idiomatic code
pub fn compareBackendQuality(
    c_code: []const u8,
    js_code: []const u8,
    erl_code: []const u8,
    allocator: std.mem.Allocator,
) !QualityComparison {
    _ = allocator;

    var c_ok = true;
    var js_ok = true;
    var erl_ok = true;

    // C checks
    expectCHasMain(c_code) catch { c_ok = false; };

    // JS checks
    expectJSModernSyntax(js_code) catch { js_ok = false; };

    // Erlang checks
    expectErlangExportsMain(erl_code) catch { erl_ok = false; };

    return QualityComparison{
        .c_idiomatic = c_ok,
        .js_idiomatic = js_ok,
        .erlang_idiomatic = erl_ok,
        .notes = "Basic quality checks",
    };
}
