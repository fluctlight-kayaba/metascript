// Metascript Build Configuration
// Modular build system - see build/ for components

const std = @import("std");
const hermes = @import("build/hermes.zig");
const tests = @import("build/tests.zig");
const codegen = @import("build/codegen.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Hermes VM toggle (default: enabled for macro support)
    const enable_vm = b.option(
        bool,
        "enable-vm",
        "Enable Hermes VM for macro execution (default: true)",
    ) orelse true;

    // =========================================================================
    // Core Executables
    // =========================================================================

    // Metascript Compiler (msc)
    const msc = b.addExecutable(.{
        .name = "msc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (enable_vm) hermes.addHermes(b, msc);
    b.installArtifact(msc);

    // Metascript Language Server (mls)
    const mls = b.addExecutable(.{
        .name = "mls",
        .root_source_file = b.path("src/lsp_main.zig"),
        .target = target,
        .optimize = optimize,
    });
    if (enable_vm) hermes.addHermes(b, mls);
    b.installArtifact(mls);

    // VM Test (only with VM enabled)
    if (enable_vm) {
        const vm_test = b.addExecutable(.{
            .name = "msc-vm-test",
            .root_source_file = b.path("src/vm_test.zig"),
            .target = target,
            .optimize = optimize,
        });
        hermes.addHermes(b, vm_test);
        b.installArtifact(vm_test);

        const run_vm_test = b.addRunArtifact(vm_test);
        run_vm_test.step.dependOn(b.getInstallStep());

        const vm_test_step = b.step("vm-test", "Run Hermes VM macro test");
        vm_test_step.dependOn(&run_vm_test.step);
    }

    // =========================================================================
    // Run Command
    // =========================================================================

    const run_cmd = b.addRunArtifact(msc);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "Run the msc compiler");
    run_step.dependOn(&run_cmd.step);

    // =========================================================================
    // Shared Source Module
    // =========================================================================

    const src_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
    });

    // Normalization Test
    const normalize_test = b.addExecutable(.{
        .name = "test-normalize",
        .root_source_file = b.path("test_normalize_direct.zig"),
        .target = target,
        .optimize = optimize,
    });
    normalize_test.root_module.addImport("src", src_module);
    b.installArtifact(normalize_test);

    const run_normalize_test = b.addRunArtifact(normalize_test);
    run_normalize_test.step.dependOn(b.getInstallStep());

    const normalize_test_step = b.step("test-normalize", "Run spread normalization test");
    normalize_test_step.dependOn(&run_normalize_test.step);

    // =========================================================================
    // Setup Subsystems
    // =========================================================================

    tests.setup(b, target, optimize, enable_vm, src_module);
    codegen.setup(b, target, optimize);
}
