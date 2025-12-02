// Hermes VM build configuration
// Links the Hermes JavaScript engine for macro execution

const std = @import("std");

/// Add Hermes VM dependencies to a compile step
pub fn addHermes(b: *std.Build, step: *std.Build.Step.Compile) void {
    // Include paths
    step.addIncludePath(b.path("src/vm"));
    step.addIncludePath(b.path("vendor/hermes/API"));
    step.addIncludePath(b.path("vendor/hermes/API/jsi"));
    step.addIncludePath(b.path("vendor/hermes/public"));

    // C++ source files
    step.addCSourceFile(.{
        .file = b.path("src/vm/hermes_api.cpp"),
        .flags = &.{ "-std=c++17", "-fno-sanitize=all" },
    });

    step.addCSourceFile(.{
        .file = b.path("vendor/hermes/API/jsi/jsi/jsi.cpp"),
        .flags = &.{ "-std=c++17", "-fno-sanitize=all" },
    });

    // Library paths
    step.addLibraryPath(b.path("vendor/hermes/build/lib"));
    step.addLibraryPath(b.path("vendor/hermes/build/jsi"));

    // Link libraries
    step.linkSystemLibrary("hermesvm");
    step.linkSystemLibrary("jsi");
    step.linkLibCpp();
}
