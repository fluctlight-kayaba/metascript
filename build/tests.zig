// Test configuration for Metascript
// Organizes unit, integration, e2e, and property tests

const std = @import("std");
const hermes = @import("hermes.zig");

pub fn setup(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    enable_vm: bool,
    src_module: *std.Build.Module,
) void {
    // =========================================================================
    // Unit Tests (src/)
    // =========================================================================

    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (enable_vm) hermes.addHermes(b, unit_tests);
    const run_unit_tests = b.addRunArtifact(unit_tests);

    // LSP server tests
    const lsp_server_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/server.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (enable_vm) hermes.addHermes(b, lsp_server_tests);
    const run_lsp_server_tests = b.addRunArtifact(lsp_server_tests);

    // JSON-RPC tests
    const jsonrpc_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/jsonrpc.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_jsonrpc_tests = b.addRunArtifact(jsonrpc_tests);

    // FileStore tests
    const filestore_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/file_store.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_filestore_tests = b.addRunArtifact(filestore_tests);

    // Normalization tests
    const normalize_tests = b.addTest(.{
        .root_source_file = b.path("tests/normalize_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    normalize_tests.root_module.addImport("src", src_module);
    const run_normalize_tests = b.addRunArtifact(normalize_tests);

    // SpreadElement tests (unit tests for AST node + type integration)
    const spread_element_tests = b.addTest(.{
        .root_source_file = b.path("tests/unit/spread_element_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    spread_element_tests.root_module.addImport("src", src_module);
    const run_spread_element_tests = b.addRunArtifact(spread_element_tests);

    // TypeChecker tests (extracted from src/checker/typechecker.zig)
    const typechecker_tests = b.addTest(.{
        .root_source_file = b.path("tests/unit/checker/typechecker_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    typechecker_tests.root_module.addImport("src", src_module);
    const run_typechecker_tests = b.addRunArtifact(typechecker_tests);

    // Ownership analysis tests (shared DRC infrastructure)
    const ownership_tests = b.addTest(.{
        .root_source_file = b.path("src/analysis/ownership.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_ownership_tests = b.addRunArtifact(ownership_tests);

    // Cycle detection tests (shared DRC infrastructure)
    const cycle_detection_tests = b.addTest(.{
        .root_source_file = b.path("src/analysis/cycle_detection.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_cycle_detection_tests = b.addRunArtifact(cycle_detection_tests);

    // RC trait tests (backend-agnostic RC interface)
    const rc_trait_tests = b.addTest(.{
        .root_source_file = b.path("src/codegen/rc_trait.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_rc_trait_tests = b.addRunArtifact(rc_trait_tests);

    // DRC unified module tests (Lobster-style ownership + cycle detection)
    const drc_tests = b.addTest(.{
        .root_source_file = b.path("src/analysis/drc.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_drc_tests = b.addRunArtifact(drc_tests);

    // Default test step
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_lsp_server_tests.step);
    test_step.dependOn(&run_jsonrpc_tests.step);
    test_step.dependOn(&run_filestore_tests.step);
    test_step.dependOn(&run_normalize_tests.step);
    test_step.dependOn(&run_spread_element_tests.step);
    test_step.dependOn(&run_typechecker_tests.step);
    test_step.dependOn(&run_ownership_tests.step);
    test_step.dependOn(&run_cycle_detection_tests.step);
    test_step.dependOn(&run_rc_trait_tests.step);
    test_step.dependOn(&run_drc_tests.step);

    // Analysis tests (DRC infrastructure)
    const test_analysis_step = b.step("test-analysis", "Run DRC/ownership analysis tests");
    test_analysis_step.dependOn(&run_ownership_tests.step);
    test_analysis_step.dependOn(&run_cycle_detection_tests.step);
    test_analysis_step.dependOn(&run_rc_trait_tests.step);
    test_analysis_step.dependOn(&run_drc_tests.step);

    // =========================================================================
    // Test Helpers
    // =========================================================================

    const helper_tests = b.addTest(.{
        .root_source_file = b.path("tests/helpers/testing.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_helper_tests = b.addRunArtifact(helper_tests);

    // =========================================================================
    // E2E Tests
    // =========================================================================

    const e2e_tests = b.addTest(.{
        .root_source_file = b.path("tests/e2e/compilation_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    e2e_tests.root_module.addImport("src", src_module);
    const run_e2e_tests = b.addRunArtifact(e2e_tests);

    const test_e2e_step = b.step("test-e2e", "Run end-to-end tests");
    test_e2e_step.dependOn(&run_e2e_tests.step);

    // =========================================================================
    // Property/Fuzz Tests
    // =========================================================================

    const property_tests = b.addTest(.{
        .root_source_file = b.path("tests/property/lexer_fuzz_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    property_tests.root_module.addImport("src", src_module);
    const run_property_tests = b.addRunArtifact(property_tests);

    const test_property_step = b.step("test-property", "Run property/fuzz tests");
    test_property_step.dependOn(&run_property_tests.step);

    // =========================================================================
    // Backend Tests (Phase 1 - Critical for TDD)
    // =========================================================================

    // Create fixtures module (single source of truth for test fixtures)
    const fixtures_module = b.createModule(.{
        .root_source_file = b.path("tests/fixtures/fixtures.zig"),
    });

    // Erlang backend tests
    const erlang_backend_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/erlang_codegen_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    erlang_backend_tests.root_module.addImport("src", src_module);
    erlang_backend_tests.root_module.addImport("fixtures", fixtures_module);
    const run_erlang_backend_tests = b.addRunArtifact(erlang_backend_tests);

    // C backend tests
    const c_backend_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/c_codegen_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    c_backend_tests.root_module.addImport("src", src_module);
    c_backend_tests.root_module.addImport("fixtures", fixtures_module);
    const run_c_backend_tests = b.addRunArtifact(c_backend_tests);

    // Cross-backend parity tests
    const cross_backend_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/cross_backend_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    cross_backend_tests.root_module.addImport("src", src_module);
    cross_backend_tests.root_module.addImport("fixtures", fixtures_module);
    const run_cross_backend_tests = b.addRunArtifact(cross_backend_tests);

    // Backend analytics test
    const backend_analytics_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/backend_analytics.zig"),
        .target = target,
        .optimize = optimize,
    });
    backend_analytics_tests.root_module.addImport("src", src_module);
    backend_analytics_tests.root_module.addImport("fixtures", fixtures_module);
    const run_backend_analytics_tests = b.addRunArtifact(backend_analytics_tests);

    // Error detection test
    const error_detection_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/error_detection_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    error_detection_tests.root_module.addImport("src", src_module);
    const run_error_detection_tests = b.addRunArtifact(error_detection_tests);

    const test_error_detection_step = b.step("test-error-detection", "Test that error detection works");
    test_error_detection_step.dependOn(&run_error_detection_tests.step);

    // Trace test (debug flow)
    const trace_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/trace_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    trace_tests.root_module.addImport("src", src_module);
    const run_trace_tests = b.addRunArtifact(trace_tests);

    const test_trace_step = b.step("test-trace", "Trace compilation flow step by step");
    test_trace_step.dependOn(&run_trace_tests.step);

    // Test real fixtures
    const test_real_fixture_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/test_real_fixture.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_real_fixture_tests.root_module.addImport("src", src_module);
    test_real_fixture_tests.root_module.addImport("fixtures", fixtures_module);
    const run_test_real_fixture_tests = b.addRunArtifact(test_real_fixture_tests);

    const test_real_fixture_step = b.step("test-real-fixture", "Test real fixtures individually");
    test_real_fixture_step.dependOn(&run_test_real_fixture_tests.step);

    // Test parser permissiveness
    const test_should_fail_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/test_should_fail.zig"),
        .target = target,
        .optimize = optimize,
    });
    test_should_fail_tests.root_module.addImport("src", src_module);
    const run_test_should_fail_tests = b.addRunArtifact(test_should_fail_tests);

    const test_should_fail_step = b.step("test-should-fail", "Test that unsupported features actually fail");
    test_should_fail_step.dependOn(&run_test_should_fail_tests.step);

    // C backend execution tests (compile + run + verify output)
    const c_execution_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/c_execution_test.zig"),
        .target = target,
        .optimize = optimize,
    });
    c_execution_tests.root_module.addImport("src", src_module);
    const run_c_execution_tests = b.addRunArtifact(c_execution_tests);

    // Individual backend test steps (focus on one backend at a time)
    const test_c_step = b.step("test-backend-c", "Run C backend tests only");
    test_c_step.dependOn(&run_c_backend_tests.step);

    const test_c_exec_step = b.step("test-c-exec", "Run C backend execution tests (compile + run + verify output)");
    test_c_exec_step.dependOn(&run_c_execution_tests.step);

    const test_erlang_step = b.step("test-backend-erlang", "Run Erlang backend tests only");
    test_erlang_step.dependOn(&run_erlang_backend_tests.step);

    const test_cross_step = b.step("test-backend-cross", "Run cross-backend parity tests");
    test_cross_step.dependOn(&run_cross_backend_tests.step);

    // Combined backend test step (all backends)
    const test_backends_step = b.step("test-backends", "Run ALL backend code generation tests (Phase 1 TDD)");
    test_backends_step.dependOn(&run_erlang_backend_tests.step);
    test_backends_step.dependOn(&run_c_backend_tests.step);
    test_backends_step.dependOn(&run_cross_backend_tests.step);

    // Backend analytics step (metrics only, always passes)
    const test_analytics_step = b.step("test-analytics", "Run backend success rate analytics");
    test_analytics_step.dependOn(&run_backend_analytics_tests.step);

    // Enhanced execution analytics (Phase 2: compile + execute tests)
    const execution_analytics_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/execution_analytics.zig"),
        .target = target,
        .optimize = optimize,
    });
    execution_analytics_tests.root_module.addImport("src", src_module);
    execution_analytics_tests.root_module.addImport("fixtures", fixtures_module);
    const run_execution_analytics_tests = b.addRunArtifact(execution_analytics_tests);

    const test_execution_step = b.step("test-execution", "Run enhanced execution analytics (compile + run generated code)");
    test_execution_step.dependOn(&run_execution_analytics_tests.step);

    // Score Codegen - Comprehensive quality scoring (uses zig cc for fast C compilation)
    const score_codegen_tests = b.addTest(.{
        .root_source_file = b.path("tests/backends/score_codegen.zig"),
        .target = target,
        .optimize = optimize,
    });
    score_codegen_tests.root_module.addImport("src", src_module);
    score_codegen_tests.root_module.addImport("fixtures", fixtures_module);
    const run_score_codegen_tests = b.addRunArtifact(score_codegen_tests);

    const test_score_codegen_step = b.step("test-score-codegen", "Run comprehensive codegen quality scoring (compile + quality + component attribution)");
    test_score_codegen_step.dependOn(&run_score_codegen_tests.step);

    // Convenience aliases for score-codegen
    const score_c_step = b.step("score-c", "Score C backend only");
    score_c_step.dependOn(&run_score_codegen_tests.step);

    const score_js_step = b.step("score-js", "Score JavaScript backend only");
    score_js_step.dependOn(&run_score_codegen_tests.step);

    const score_erl_step = b.step("score-erl", "Score Erlang backend only");
    score_erl_step.dependOn(&run_score_codegen_tests.step);

    // =========================================================================
    // Aggregate Test Steps
    // =========================================================================

    const test_unit_step = b.step("test-unit", "Run unit tests (tests/unit/)");
    test_unit_step.dependOn(&run_helper_tests.step);

    const test_integration_step = b.step("test-integration", "Run integration tests");
    test_integration_step.dependOn(&run_helper_tests.step);

    // All tests (slow first run due to Hermes C++ compilation)
    const test_all_step = b.step("test-all", "Run ALL tests");
    test_all_step.dependOn(&run_unit_tests.step);
    test_all_step.dependOn(&run_lsp_server_tests.step);
    test_all_step.dependOn(&run_jsonrpc_tests.step);
    test_all_step.dependOn(&run_filestore_tests.step);
    test_all_step.dependOn(&run_normalize_tests.step);
    test_all_step.dependOn(&run_helper_tests.step);
    test_all_step.dependOn(&run_property_tests.step);
    test_all_step.dependOn(&run_e2e_tests.step);
    test_all_step.dependOn(&run_erlang_backend_tests.step);
    test_all_step.dependOn(&run_c_backend_tests.step);
    test_all_step.dependOn(&run_cross_backend_tests.step);

    // Quick smoke test
    const test_quick_step = b.step("test-quick", "Run quick smoke tests");
    test_quick_step.dependOn(&run_helper_tests.step);

    // =========================================================================
    // Benchmarks
    // =========================================================================

    // Create modules for benchmark framework and runtime
    const benchmark_mod = b.createModule(.{
        .root_source_file = b.path("tests/framework/benchmark.zig"),
    });
    const reporter_mod = b.createModule(.{
        .root_source_file = b.path("tests/framework/reporter.zig"),
        .imports = &.{
            .{ .name = "benchmark", .module = benchmark_mod },
        },
    });
    const runner_mod = b.createModule(.{
        .root_source_file = b.path("tests/framework/runner.zig"),
        .imports = &.{
            .{ .name = "benchmark", .module = benchmark_mod },
            .{ .name = "reporter", .module = reporter_mod },
        },
    });
    const orc_mod = b.createModule(.{
        .root_source_file = b.path("src/runtime/orc.zig"),
    });
    // string.zig imports orc via module
    const string_mod = b.createModule(.{
        .root_source_file = b.path("src/runtime/string.zig"),
        .imports = &.{
            .{ .name = "orc", .module = orc_mod },
        },
    });

    // ORC benchmarks module
    const orc_bench_mod = b.createModule(.{
        .root_source_file = b.path("tests/benchmarks/runtime/orc_benchmarks.zig"),
        .imports = &.{
            .{ .name = "benchmark", .module = benchmark_mod },
            .{ .name = "reporter", .module = reporter_mod },
            .{ .name = "orc", .module = orc_mod },
        },
    });

    // String benchmarks module
    const string_bench_mod = b.createModule(.{
        .root_source_file = b.path("tests/benchmarks/runtime/string_benchmarks.zig"),
        .imports = &.{
            .{ .name = "benchmark", .module = benchmark_mod },
            .{ .name = "reporter", .module = reporter_mod },
            .{ .name = "string", .module = string_mod },
        },
    });

    // ORC benchmarks only
    const benchmark_orc = b.addExecutable(.{
        .name = "benchmark-orc",
        .root_source_file = b.path("tests/benchmarks/runtime/orc_benchmarks.zig"),
        .target = target,
        .optimize = .ReleaseFast,
    });
    benchmark_orc.root_module.addImport("benchmark", benchmark_mod);
    benchmark_orc.root_module.addImport("reporter", reporter_mod);
    benchmark_orc.root_module.addImport("orc", orc_mod);
    benchmark_orc.linkLibC();

    const run_benchmark_orc = b.addRunArtifact(benchmark_orc);

    const benchmark_orc_step = b.step("benchmark-orc", "Run ORC benchmarks only");
    benchmark_orc_step.dependOn(&run_benchmark_orc.step);

    // String benchmarks only
    const benchmark_string = b.addExecutable(.{
        .name = "benchmark-string",
        .root_source_file = b.path("tests/benchmarks/runtime/string_benchmarks.zig"),
        .target = target,
        .optimize = .ReleaseFast,
    });
    benchmark_string.root_module.addImport("benchmark", benchmark_mod);
    benchmark_string.root_module.addImport("reporter", reporter_mod);
    benchmark_string.root_module.addImport("string", string_mod);
    benchmark_string.linkLibC();

    const run_benchmark_string = b.addRunArtifact(benchmark_string);

    const benchmark_string_step = b.step("benchmark-string", "Run string benchmarks only");
    benchmark_string_step.dependOn(&run_benchmark_string.step);

    // Main benchmark runner (all benchmarks)
    const benchmark_main = b.addExecutable(.{
        .name = "benchmark-runner",
        .root_source_file = b.path("tests/benchmarks/main.zig"),
        .target = target,
        .optimize = .ReleaseFast,
    });
    benchmark_main.root_module.addImport("benchmark", benchmark_mod);
    benchmark_main.root_module.addImport("reporter", reporter_mod);
    benchmark_main.root_module.addImport("runner", runner_mod);
    benchmark_main.root_module.addImport("orc_benchmarks", orc_bench_mod);
    benchmark_main.root_module.addImport("string_benchmarks", string_bench_mod);
    benchmark_main.linkLibC();

    const run_benchmark_main = b.addRunArtifact(benchmark_main);

    const benchmark_step = b.step("benchmark", "Run all benchmarks");
    benchmark_step.dependOn(&run_benchmark_main.step);

    // Benchmark with JSON output (for CI/LLM)
    const run_benchmark_json = b.addRunArtifact(benchmark_main);
    run_benchmark_json.addArg("--json");

    const benchmark_json_step = b.step("benchmark-json", "Run benchmarks with JSON output");
    benchmark_json_step.dependOn(&run_benchmark_json.step);

    // Benchmark with Markdown output
    const run_benchmark_md = b.addRunArtifact(benchmark_main);
    run_benchmark_md.addArg("--markdown");

    const benchmark_md_step = b.step("benchmark-md", "Run benchmarks with Markdown output");
    benchmark_md_step.dependOn(&run_benchmark_md.step);
}
