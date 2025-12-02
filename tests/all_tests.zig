/// All Tests Entry Point
///
/// This file aggregates all test modules for running the complete test suite.
///
/// Run commands:
///   zig build test-all      # Run ALL tests
///   zig build test-unit     # Unit tests only
///   zig build test-e2e      # End-to-end tests
///   zig build test-property # Property/fuzz tests
///   zig build test-quick    # Quick smoke tests
///
/// Test Categories:
/// - Unit tests (40%): Single component testing - tests/unit/
/// - Integration tests (30%): Multiple components - tests/integration/
/// - E2E tests (20%): Full pipeline with real files - tests/e2e/
/// - Property tests (10%): Fuzz/randomized testing - tests/property/
///
/// Test Pyramid:
///                 /\
///                / E2E\
///               /──────\
///              / Integ- \
///             / ration  \
///            /────────────\
///           /   Unit Tests  \
///          /──────────────────\
///         /   Property Tests    \
///        /────────────────────────\

const std = @import("std");

// ============================================================================
// Test Utilities and Fixtures
// ============================================================================

pub const testing_helpers = @import("helpers/testing.zig");
pub const fixtures = @import("fixtures/sources.zig");

// ============================================================================
// Unit Tests (tests/unit/)
// ============================================================================

// Note: These are run via `zig test` directly on the files, not through
// the build system. The build system runs inline tests in src/ files.

// For documentation, unit tests cover:
// - tests/unit/lexer_test.zig - Lexer tokenization tests
// - tests/unit/parser_test.zig - Parser AST generation tests

// ============================================================================
// Integration Tests (tests/integration/)
// ============================================================================

// Integration tests verify multiple components working together:
// - tests/integration/macro_expansion_test.zig - Lexer → Parser → Macro

// ============================================================================
// E2E Tests (tests/e2e/)
// ============================================================================

// End-to-end tests compile real .ms files:
// - tests/e2e/compilation_test.zig - Full compilation pipeline

// ============================================================================
// Property/Fuzz Tests (tests/property/)
// ============================================================================

// Property tests verify invariants with random input:
// - tests/property/lexer_fuzz_test.zig - Lexer robustness

// ============================================================================
// Source File Tests (inline tests in src/)
// ============================================================================

// These are discovered automatically when running `zig build test`:
// - src/lexer/token.zig - Token type tests
// - src/ast/location.zig - Location tests
// - src/lsp/server.zig - LSP server tests
// - src/lsp/jsonrpc.zig - JSON-RPC tests
// - src/lsp/file_store.zig - File store tests

// ============================================================================
// Test Infrastructure
// ============================================================================

test "test suite smoke test" {
    // This test always passes - used to verify test infrastructure works
    try std.testing.expect(true);
}

test "fixtures are valid" {
    // Verify fixtures module loads correctly
    try std.testing.expect(fixtures.EMPTY.len == 0);
    try std.testing.expect(fixtures.CLASS_EMPTY.len > 0);
    try std.testing.expect(fixtures.FULL_MODULE.len > 100);
}

test "testing helpers are available" {
    // Verify testing helpers work
    try testing_helpers.expectContains("hello world", "world");
    try testing_helpers.expectStartsWith("hello world", "hello");
    try testing_helpers.expectEndsWith("hello world", "world");
}

test "Timer helper works" {
    const timer = testing_helpers.Timer.start();
    try std.testing.expect(timer.elapsed() >= 0);
}

test "TestContext initializes correctly" {
    var ctx = testing_helpers.TestContext.init();
    defer ctx.deinit();
    // Just verify we can get the arena allocator without error
    _ = ctx.arenaAlloc();
}
