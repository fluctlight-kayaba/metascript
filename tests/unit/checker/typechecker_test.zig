/// Type Checker Unit Tests
///
/// External test file for type checker component.
/// New tests should be added here, not in src/checker/typechecker.zig.
///
/// NOTE: Legacy tests remain in typechecker.zig for now. This file
/// will contain new tests going forward and serves as the target
/// for gradual migration of existing tests.
///
/// Test categories:
/// - Basic: initialization, break/continue validation
/// - Return types: matching, mismatch, void functions
/// - Assignments: const/let, readonly
/// - Structural typing: object compatibility
/// - Union types: compatibility, narrowing
/// - Function calls: argument types, arity
/// - Arithmetic: numeric type operations
/// - Generics: type parameters, instantiation, constraints
/// - Modules: import/export handling

const std = @import("std");
const testing = std.testing;

// Import via src module (configured in build.zig)
const src = @import("src");
const checker = src.checker;
const TypeChecker = checker.TypeChecker;
const TypeError = checker.TypeError;
const symbol = checker.symbol;
const types = src.ast.types;
const ast = src.ast;
const location = src.ast.location;

// ============================================================================
// Basic Tests
// ============================================================================

test "type checker initialization" {
    var tc = try TypeChecker.init(testing.allocator);
    defer tc.deinit();

    // Just verify it initializes without error
    try testing.expect(!tc.hasErrors());
}

test "type checker: break outside loop" {
    var tc = try TypeChecker.init(testing.allocator);
    defer tc.deinit();

    // Create a program with break outside loop
    var break_node = ast.Node{
        .kind = .break_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .break_stmt = {} },
    };
    var stmts = [_]*ast.Node{&break_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try tc.check(&program);
    try testing.expect(!ok);
    try testing.expect(tc.hasErrors());
    try testing.expectEqual(@as(usize, 1), tc.errors.items.len);
    try testing.expectEqual(TypeError.Kind.break_outside_loop, tc.errors.items[0].kind);
}

test "type checker: continue outside loop" {
    var tc = try TypeChecker.init(testing.allocator);
    defer tc.deinit();

    // Create a program with continue outside loop
    var continue_node = ast.Node{
        .kind = .continue_stmt,
        .location = location.SourceLocation.dummy(),
        .data = .{ .continue_stmt = {} },
    };
    var stmts = [_]*ast.Node{&continue_node};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try tc.check(&program);
    try testing.expect(!ok);
    try testing.expect(tc.hasErrors());
    try testing.expectEqual(TypeError.Kind.continue_outside_loop, tc.errors.items[0].kind);
}

test "type checker: empty program" {
    var tc = try TypeChecker.init(testing.allocator);
    defer tc.deinit();

    var stmts = [_]*ast.Node{};
    var program = ast.Node{
        .kind = .program,
        .location = location.SourceLocation.dummy(),
        .data = .{ .program = .{
            .statements = &stmts,
            .file_id = 0,
        } },
    };

    const ok = try tc.check(&program);
    try testing.expect(ok);
    try testing.expect(!tc.hasErrors());
}

// ============================================================================
// Reference submodule tests
// ============================================================================

test {
    // Include tests from submodules
    testing.refAllDecls(symbol);
    testing.refAllDecls(checker.resolver);
    testing.refAllDecls(checker.inference);
}
