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

    // Default test step
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_lsp_server_tests.step);
    test_step.dependOn(&run_jsonrpc_tests.step);
    test_step.dependOn(&run_filestore_tests.step);
    test_step.dependOn(&run_normalize_tests.step);
    test_step.dependOn(&run_spread_element_tests.step);

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
