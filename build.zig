const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard target and optimization options
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build option: Enable Hermes VM (requires `make hermes` first)
    const enable_vm = b.option(
        bool,
        "enable-vm",
        "Enable Hermes VM for macro execution (requires 'make hermes' first)",
    ) orelse false;

    // Compiler executable
    const exe = b.addExecutable(.{
        .name = "msc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Add Hermes if enabled
    if (enable_vm) {
        addHermes(b, exe);

        // VM Test executable (only with VM enabled)
        const vm_test = b.addExecutable(.{
            .name = "msc-vm-test",
            .root_source_file = b.path("src/vm_test.zig"),
            .target = target,
            .optimize = optimize,
        });
        addHermes(b, vm_test);
        b.installArtifact(vm_test);

        // VM test run command
        const run_vm_test = b.addRunArtifact(vm_test);
        run_vm_test.step.dependOn(b.getInstallStep());

        const vm_test_step = b.step("vm-test", "Run Hermes VM macro test");
        vm_test_step.dependOn(&run_vm_test.step);
    }

    b.installArtifact(exe);

    // Language Server (separate binary - Metascript Language Server)
    const lsp = b.addExecutable(.{
        .name = "mls",
        .root_source_file = b.path("src/lsp_main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lsp);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the msc compiler");
    run_step.dependOn(&run_cmd.step);

    // Unit tests - main compiler
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Unit tests - LSP server
    const lsp_server_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/server.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_lsp_server_tests = b.addRunArtifact(lsp_server_tests);

    // Unit tests - JSON-RPC
    const jsonrpc_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/jsonrpc.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_jsonrpc_tests = b.addRunArtifact(jsonrpc_tests);

    // Unit tests - FileStore
    const filestore_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp/file_store.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_filestore_tests = b.addRunArtifact(filestore_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
    test_step.dependOn(&run_lsp_server_tests.step);
    test_step.dependOn(&run_jsonrpc_tests.step);
    test_step.dependOn(&run_filestore_tests.step);

    // Visibility tools - need access to src modules
    const src_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
    });

    const dump_tokens = b.addExecutable(.{
        .name = "dump_tokens",
        .root_source_file = b.path("tools/dump_tokens.zig"),
        .target = target,
        .optimize = optimize,
    });
    dump_tokens.root_module.addImport("src", src_module);
    b.installArtifact(dump_tokens);

    const compile_pipeline = b.addExecutable(.{
        .name = "compile_pipeline",
        .root_source_file = b.path("tools/compile_pipeline.zig"),
        .target = target,
        .optimize = optimize,
    });
    compile_pipeline.root_module.addImport("src", src_module);
    b.installArtifact(compile_pipeline);
}

/// Add Hermes VM dependencies to a compile step
fn addHermes(b: *std.Build, step: *std.Build.Step.Compile) void {
    // Include paths
    step.addIncludePath(b.path("src/vm"));
    step.addIncludePath(b.path("vendor/hermes/API"));
    step.addIncludePath(b.path("vendor/hermes/API/jsi"));
    step.addIncludePath(b.path("vendor/hermes/public"));

    // C++ source files (compiled with clang++)
    step.addCSourceFile(.{
        .file = b.path("src/vm/hermes_api.cpp"),
        .flags = &[_][]const u8{ "-std=c++17", "-fno-sanitize=all" },
    });

    step.addCSourceFile(.{
        .file = b.path("vendor/hermes/API/jsi/jsi/jsi.cpp"),
        .flags = &[_][]const u8{ "-std=c++17", "-fno-sanitize=all" },
    });

    // Library paths (addLibraryPath also sets RPATH on macOS)
    step.addLibraryPath(b.path("vendor/hermes/build/lib"));
    step.addLibraryPath(b.path("vendor/hermes/build/jsi"));

    // Link Hermes libraries (full VM, not lean - need JS compiler for macros)
    step.linkSystemLibrary("hermesvm");
    step.linkSystemLibrary("jsi");

    // Link C++ standard library
    step.linkLibCpp();
}
