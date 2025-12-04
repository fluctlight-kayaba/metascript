// CLI Command: compile
// Compile to target backend (C/JavaScript/Erlang)
// Uses Trans-Am engine for incremental analysis

const std = @import("std");
const transam = @import("../transam/transam.zig");
const jsgen = @import("../codegen/js/jsgen.zig");
const cgen = @import("../codegen/c/cgen.zig");
const erlgen = @import("../codegen/erlang/erlgen.zig");
const checker = @import("../checker/typechecker.zig");
const vm_expander = @import("../macro/vm_expander.zig");
const normalize = @import("../macro/normalize.zig");
const colors = @import("colors.zig");

// DRC (Deferred Reference Counting) analysis - optional, disabled by default
const Drc = @import("../analysis/drc.zig").Drc;
const DrcAnalyzer = @import("../analysis/drc_analyzer.zig").DrcAnalyzer;

pub const Backend = enum {
    c,
    js,
    erlang,
};

pub fn run(allocator: std.mem.Allocator, input_file: []const u8) !void {
    return runWithArgs(allocator, input_file, .js, null, true);
}

pub fn runWithArgs(allocator: std.mem.Allocator, input_file: []const u8, target: Backend, output_path: ?[]const u8, enable_normalize: bool) !void {
    const start_time = std.time.milliTimestamp();

    // Initialize Trans-Am database (same engine as LSP!)
    var db = try transam.TransAmDatabase.init(allocator);
    defer db.deinit();

    // Phase 1: Load and parse file
    std.debug.print("{s}[1/5]{s} Parsing {s}...\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        input_file,
    });

    const source = std.fs.cwd().readFileAlloc(allocator, input_file, 1024 * 1024) catch |err| {
        std.debug.print("{s}error:{s} Could not read file '{s}': {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            input_file,
            @errorName(err),
        });
        return;
    };
    defer allocator.free(source);

    // Register with Trans-Am and parse
    _ = try db.setFileText(input_file, source);

    // Check for parse errors via Trans-Am
    const diagnostics = db.getDiagnostics(input_file) catch |err| {
        std.debug.print("{s}error:{s} Analysis failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer {
        for (diagnostics) |diag| {
            allocator.free(diag.message);
        }
        allocator.free(diagnostics);
    }

    if (diagnostics.len > 0) {
        std.debug.print("{s}[1/5]{s} Parsing {s}failed{s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            colors.Color.bright_red.code(),
            colors.Color.reset.code(),
        });
        printDiagnostics(input_file, source, diagnostics);
        return;
    }

    std.debug.print("{s}[1/5]{s} Parsed {s}✓{s}\n", .{
        colors.success.code(),
        colors.Color.reset.code(),
        colors.Color.bright_green.code(),
        colors.Color.reset.code(),
    });

    // Phase 2: Get AST and expand macros
    std.debug.print("{s}[2/5]{s} Expanding macros...\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
    });

    const parse_result = db.parse(input_file) catch |err| {
        std.debug.print("{s}error:{s} Parse failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };

    // Run macro expansion with bytecode cache for speed
    const bytecode_cache = if (db.bytecode_cache) |*bc| bc else null;
    const program_ast = blk: {
        const expanded = vm_expander.expandAllMacrosWithCache(
            parse_result.arena,
            allocator,
            parse_result.tree,
            bytecode_cache,
        ) catch |err| {
            std.debug.print("{s}[2/5]{s} Macro expansion {s}warning{s}: {s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                colors.Color.bright_yellow.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
            // Continue with unexpanded AST
            break :blk parse_result.tree;
        };
        break :blk expanded;
    };

    std.debug.print("{s}[2/5]{s} Macros expanded {s}✓{s}\n", .{
        colors.success.code(),
        colors.Color.reset.code(),
        colors.Color.bright_green.code(),
        colors.Color.reset.code(),
    });

    // Phase 3: Type checking
    std.debug.print("{s}[3/5]{s} Type checking...\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
    });

    // Run type checker to populate node.type fields
    var type_checker = checker.TypeChecker.init(allocator) catch |err| {
        std.debug.print("{s}error:{s} Type checker init failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer type_checker.deinit();

    const type_check_ok = type_checker.check(program_ast) catch |err| {
        std.debug.print("{s}error:{s} Type checking failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };

    if (!type_check_ok) {
        std.debug.print("{s}[3/5]{s} Type checking {s}failed{s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            colors.Color.bright_red.code(),
            colors.Color.reset.code(),
        });
        // Print type errors
        for (type_checker.errors.items) |err| {
            std.debug.print("{s}error:{s} {s}\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
                err.message,
            });
            std.debug.print("  {s}-->{s} {s}:{d}:{d}\n", .{
                colors.Color.bright_blue.code(),
                colors.Color.reset.code(),
                input_file,
                err.location.start.line + 1,
                err.location.start.column + 1,
            });
        }
        // Continue anyway for now - type errors shouldn't block codegen
    } else {
        std.debug.print("{s}[3/5]{s} Type checked {s}✓{s}\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    // Phase 3.5: AST Normalization (after type checking so we have type info!)
    const final_ast = if (enable_normalize and type_check_ok) blk: {
        std.debug.print("{s}[3.5/5]{s} Normalizing AST...\n", .{
            colors.info.code(),
            colors.Color.reset.code(),
        });

        var normalize_ctx = normalize.NormalizeContext.init(parse_result.arena, allocator, &type_checker);
        const normalized_ast = normalize.normalizeAST(&normalize_ctx, program_ast) catch |err| {
            std.debug.print("{s}[3.5/5]{s} Normalization {s}warning{s}: {s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                colors.Color.bright_yellow.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
            // Continue with unnormalized AST (normalization is optimization, not required)
            std.debug.print("{s}[3.5/5]{s} Normalization skipped {s}⚠{s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                colors.Color.bright_yellow.code(),
                colors.Color.reset.code(),
            });
            break :blk program_ast;
        };

        // Print normalization stats
        if (normalize_ctx.stats.object_spreads_normalized > 0 or
            normalize_ctx.stats.array_chains_fused > 0 or
            normalize_ctx.stats.closures_inlined > 0)
        {
            std.debug.print("{s}  Normalized:{s} ", .{
                colors.dim_text.code(),
                colors.Color.reset.code(),
            });
            if (normalize_ctx.stats.object_spreads_normalized > 0) {
                std.debug.print("{d} spreads ", .{normalize_ctx.stats.object_spreads_normalized});
            }
            if (normalize_ctx.stats.array_chains_fused > 0) {
                std.debug.print("{d} chains ", .{normalize_ctx.stats.array_chains_fused});
            }
            if (normalize_ctx.stats.closures_inlined > 0) {
                std.debug.print("{d} closures", .{normalize_ctx.stats.closures_inlined});
            }
            std.debug.print("\n", .{});
        }

        std.debug.print("{s}[3.5/5]{s} Normalized {s}✓{s}\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
        break :blk normalized_ast;
    } else blk: {
        if (!enable_normalize) {
            std.debug.print("{s}[3.5/5]{s} Normalization {s}disabled{s} (--no-normalize)\n", .{
                colors.dim_text.code(),
                colors.Color.reset.code(),
                colors.Color.bright_yellow.code(),
                colors.Color.reset.code(),
            });
        }
        break :blk program_ast;
    };

    // Phase 4: DRC analysis (for C backend)
    var drc: ?*Drc = null;
    var drc_storage: Drc = undefined;

    if (target == .c) {
        std.debug.print("{s}[4/5]{s} Running DRC analysis...\n", .{
            colors.info.code(),
            colors.Color.reset.code(),
        });

        // Initialize DRC with move optimization enabled
        // Move optimization detects last-use of variables and elides RC ops
        drc_storage = Drc.initWithConfig(allocator, .{
            .enable_move_optimization = true,
            .enable_cycle_detection = true,
        });
        drc = &drc_storage;

        var analyzer = DrcAnalyzer.init(drc.?);
        analyzer.analyze(final_ast) catch |err| {
            std.debug.print("{s}[4/5]{s} DRC analysis {s}warning{s}: {s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                colors.Color.bright_yellow.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
            drc.?.deinit();
            drc = null; // Fall back to legacy mode
        };

        if (drc != null) {
            drc.?.finalize() catch |err| {
                std.debug.print("{s}[4/5]{s} DRC finalize {s}warning{s}: {s}\n", .{
                    colors.warning.code(),
                    colors.Color.reset.code(),
                    colors.Color.bright_yellow.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
                drc.?.deinit();
                drc = null;
            };
        }

        if (drc != null) {
            const stats = drc.?.getStats();
            std.debug.print("{s}[4/5]{s} DRC analyzed {s}✓{s} ({d} vars, {d} RC ops, {d:.0}% elided)\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                colors.Color.bright_green.code(),
                colors.Color.reset.code(),
                stats.variables_analyzed,
                stats.total_ops,
                stats.elisionRate() * 100,
            });
        }
    } else {
        std.debug.print("{s}[4/5]{s} Preparing codegen...\n", .{
            colors.info.code(),
            colors.Color.reset.code(),
        });

        std.debug.print("{s}[4/5]{s} Ready {s}✓{s}\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    defer if (drc) |d| d.deinit();

    // Phase 5: Backend code generation
    const target_name = switch (target) {
        .c => "C",
        .js => "JavaScript",
        .erlang => "Erlang",
    };

    std.debug.print("{s}[5/5]{s} Generating {s}...\n", .{
        colors.info.code(),
        colors.Color.reset.code(),
        target_name,
    });

    switch (target) {
        .js => {
            var gen = jsgen.JSGenerator.init(allocator);
            defer gen.deinit();

            const js_code = gen.generate(final_ast) catch |err| {
                std.debug.print("{s}error:{s} JS generation failed: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
                return;
            };
            defer allocator.free(js_code);

            // Determine output path
            const out_path = output_path orelse blk: {
                // Replace .ms extension with .js
                if (std.mem.endsWith(u8, input_file, ".ms")) {
                    const base = input_file[0 .. input_file.len - 3];
                    break :blk std.fmt.allocPrint(allocator, "{s}.js", .{base}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                } else {
                    break :blk std.fmt.allocPrint(allocator, "{s}.js", .{input_file}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                }
            };
            defer if (output_path == null) allocator.free(out_path);

            // Write output file
            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = js_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            std.debug.print("{s}[5/5]{s} Generated {s} {s}✓{s}\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                out_path,
                colors.Color.bright_green.code(),
                colors.Color.reset.code(),
            });

            const elapsed = std.time.milliTimestamp() - start_time;
            std.debug.print("\n{s}✓{s} Compilation complete ({d}ms)\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                elapsed,
            });
            std.debug.print("  {s}Output:{s} {s}\n", .{
                colors.dim_text.code(),
                colors.Color.reset.code(),
                out_path,
            });
        },
        .c => {
            // DRC is required for C backend
            if (drc == null) {
                std.debug.print("{s}error:{s} DRC analysis is required for C backend\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                });
                return;
            }

            var gen = cgen.CGenerator.init(allocator, drc.?);
            defer gen.deinit();

            const c_code = gen.generate(final_ast) catch |err| {
                std.debug.print("{s}error:{s} C generation failed: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
                return;
            };
            defer allocator.free(c_code);

            // Determine output path
            const out_path = output_path orelse blk: {
                // Replace .ms extension with .c
                if (std.mem.endsWith(u8, input_file, ".ms")) {
                    const base = input_file[0 .. input_file.len - 3];
                    break :blk std.fmt.allocPrint(allocator, "{s}.c", .{base}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                } else {
                    break :blk std.fmt.allocPrint(allocator, "{s}.c", .{input_file}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                }
            };
            defer if (output_path == null) allocator.free(out_path);

            // Write output file
            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = c_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            std.debug.print("{s}[5/5]{s} Generated {s} {s}✓{s}\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                out_path,
                colors.Color.bright_green.code(),
                colors.Color.reset.code(),
            });

            const elapsed = std.time.milliTimestamp() - start_time;
            std.debug.print("\n{s}✓{s} Compilation complete ({d}ms)\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                elapsed,
            });
            std.debug.print("  {s}Output:{s} {s}\n", .{
                colors.dim_text.code(),
                colors.Color.reset.code(),
                out_path,
            });
        },
        .erlang => {
            var gen = try erlgen.ErlangGenerator.init(allocator, input_file);
            defer gen.deinit();

            const erl_code = gen.generate(final_ast) catch |err| {
                std.debug.print("{s}error:{s} Erlang generation failed: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
                return;
            };
            defer allocator.free(erl_code);

            // Determine output path
            const out_path = output_path orelse blk: {
                // Replace .ms extension with .erl
                if (std.mem.endsWith(u8, input_file, ".ms")) {
                    const base = input_file[0 .. input_file.len - 3];
                    break :blk std.fmt.allocPrint(allocator, "{s}.erl", .{base}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                } else {
                    break :blk std.fmt.allocPrint(allocator, "{s}.erl", .{input_file}) catch {
                        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                            colors.error_color.code(),
                            colors.Color.reset.code(),
                        });
                        return;
                    };
                }
            };
            defer if (output_path == null) allocator.free(out_path);

            // Write output file
            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = erl_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            std.debug.print("{s}[5/5]{s} Generated {s} {s}✓{s}\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                out_path,
                colors.Color.bright_green.code(),
                colors.Color.reset.code(),
            });

            const elapsed = std.time.milliTimestamp() - start_time;
            std.debug.print("\n{s}✓{s} Compilation complete ({d}ms)\n", .{
                colors.success.code(),
                colors.Color.reset.code(),
                elapsed,
            });
            std.debug.print("  {s}Output:{s} {s}\n", .{
                colors.dim_text.code(),
                colors.Color.reset.code(),
                out_path,
            });
        },
    }
}

fn printDiagnostics(path: []const u8, source: []const u8, diagnostics: []const transam.Diagnostic) void {
    std.debug.print("\n", .{});
    for (diagnostics) |diag| {
        const severity_color = switch (diag.severity) {
            .@"error" => colors.error_color,
            .warning => colors.warning,
            .information => colors.info,
            .hint => colors.dim_text,
        };

        std.debug.print("{s}error:{s} {s}\n", .{
            severity_color.code(),
            colors.Color.reset.code(),
            diag.message,
        });

        std.debug.print("  {s}-->{s} {s}:{d}:{d}\n", .{
            colors.Color.bright_blue.code(),
            colors.Color.reset.code(),
            path,
            diag.start_line + 1,
            diag.start_col + 1,
        });

        if (getLine(source, diag.start_line)) |line| {
            std.debug.print("  {s}{d} |{s} {s}\n", .{
                colors.Color.bright_blue.code(),
                diag.start_line + 1,
                colors.Color.reset.code(),
                line,
            });
        }
        std.debug.print("\n", .{});
    }

    std.debug.print("{s}error:{s} aborting due to {d} error{s}\n", .{
        colors.error_color.code(),
        colors.Color.reset.code(),
        diagnostics.len,
        if (diagnostics.len > 1) "s" else "",
    });
}

fn getLine(source: []const u8, line_num: u32) ?[]const u8 {
    var current_line: u32 = 0;
    var line_start: usize = 0;

    for (source, 0..) |c, i| {
        if (c == '\n') {
            if (current_line == line_num) {
                return source[line_start..i];
            }
            current_line += 1;
            line_start = i + 1;
        }
    }

    if (current_line == line_num and line_start < source.len) {
        return source[line_start..];
    }

    return null;
}
