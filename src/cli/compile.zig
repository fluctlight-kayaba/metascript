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
const module_loader = @import("../module/loader.zig");
const transform_pipeline = @import("../transform/pipeline.zig");
const build_config = @import("../build/config.zig");

// DRC (Deferred Reference Counting) analysis - optional, disabled by default
const Drc = @import("../analysis/drc.zig").Drc;
const DrcAnalyzer = @import("../analysis/drc_analyzer.zig").DrcAnalyzer;

pub const Backend = enum {
    c,
    js,
    erlang,
};

/// Compilation options
pub const CompileOptions = struct {
    /// Show verbose output (all phases). Default is quiet/Cargo-style.
    verbose: bool = false,
    /// Enable AST normalization
    enable_normalize: bool = true,
};

pub fn run(allocator: std.mem.Allocator, input_file: []const u8) !void {
    return runWithArgs(allocator, input_file, .js, null, true);
}

/// Run compilation with build config (includes transforms)
pub fn runWithBuildConfig(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    target: Backend,
    output_path: ?[]const u8,
    enable_normalize: bool,
    cfg: *const build_config.BuildConfig,
) !void {
    return compileInternal(allocator, input_file, target, output_path, .{
        .enable_normalize = enable_normalize,
        .verbose = false,
    }, cfg);
}

pub fn runWithArgs(allocator: std.mem.Allocator, input_file: []const u8, target: Backend, output_path: ?[]const u8, enable_normalize: bool) !void {
    return compileInternal(allocator, input_file, target, output_path, .{
        .enable_normalize = enable_normalize,
        .verbose = true, // CLI compile command uses verbose
    }, null);
}

/// Internal compilation implementation - unified pipeline with optional transforms
fn compileInternal(
    allocator: std.mem.Allocator,
    input_file: []const u8,
    target: Backend,
    output_path: ?[]const u8,
    options: CompileOptions,
    cfg: ?*const build_config.BuildConfig,
) !void {
    const has_transforms = cfg != null and cfg.?.transforms.len > 0;
    const total_phases: u8 = if (has_transforms) 6 else 5;
    const start_time = std.time.milliTimestamp();
    const verbose = options.verbose;

    // Phase numbering helper
    var current_phase: u8 = 0;
    const nextPhase = struct {
        fn next(phase: *u8) u8 {
            phase.* += 1;
            return phase.*;
        }
    }.next;

    // Initialize Trans-Am database (same engine as LSP!)
    var db = try transam.TransAmDatabase.init(allocator);
    defer db.deinit();

    // Phase 1: Load and parse file
    const phase1 = nextPhase(&current_phase);
    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Parsing {s}...\n", .{
            colors.info.code(),
            phase1,
            total_phases,
            colors.Color.reset.code(),
            input_file,
        });
    }

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
    _ = try transam.input_queries.setFileText(&db, input_file, source);

    // Check for parse errors via Trans-Am
    const diagnostics = transam.diagnostics_mod.getDiagnostics(&db, input_file) catch |err| {
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
        std.debug.print("{s}error:{s} Parse failed\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
        });
        printDiagnostics(input_file, source, diagnostics);
        return;
    }

    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Parsed {s}✓{s}\n", .{
            colors.success.code(),
            phase1,
            total_phases,
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    // Phase 2: Get AST and expand macros
    const phase2 = nextPhase(&current_phase);
    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Expanding macros...\n", .{
            colors.info.code(),
            phase2,
            total_phases,
            colors.Color.reset.code(),
        });
    }

    const parse_result = db.parse(input_file) catch |err| {
        std.debug.print("{s}error:{s} Parse failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };

    // Initialize module loader for import resolution and standard library macros
    var loader = module_loader.ModuleLoader.init(allocator, parse_result.arena) catch |err| {
        std.debug.print("{s}error:{s} Module loader init failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };
    defer loader.deinit();

    // Load standard library macros (@derive, etc.)
    loader.loadStdMacros() catch |err| {
        if (verbose) {
            std.debug.print("{s}warning:{s} std macros: {s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
        }
        // Continue without std macros - they might not be needed
    };

    // Load the entry module (this triggers loading of all imported dependencies)
    _ = loader.loadModule(input_file) catch |err| {
        if (verbose) {
            std.debug.print("{s}warning:{s} module loading: {s}\n", .{
                colors.warning.code(),
                colors.Color.reset.code(),
                @errorName(err),
            });
        }
        // Continue with just the parsed AST - imports may not be resolvable
    };

    // Run macro expansion with bytecode cache and ModuleLoader for proper import resolution
    const bytecode_cache = if (db.bytecode_cache) |*bc| bc else null;
    const program_ast = blk: {
        const expanded = vm_expander.expandAllMacrosWithLoaderAndCaches(
            parse_result.arena,
            allocator,
            parse_result.tree,
            &loader,
            input_file,
            bytecode_cache,
            null, // network cache not used in CLI compile
        ) catch |err| {
            if (verbose) {
                std.debug.print("{s}warning:{s} macro expansion: {s}\n", .{
                    colors.warning.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
            }
            // Continue with unexpanded AST
            break :blk parse_result.tree;
        };
        break :blk expanded;
    };

    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Macros expanded {s}✓{s}\n", .{
            colors.success.code(),
            phase2,
            total_phases,
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    // Phase 2.5: Transform Pipeline (optional - runs if transforms configured)
    const transformed_ast = if (has_transforms) transform_blk: {
        const phase_transform = nextPhase(&current_phase);
        if (verbose) {
            std.debug.print("{s}[{d}/{d}]{s} Running transforms...\n", .{
                colors.info.code(),
                phase_transform,
                total_phases,
                colors.Color.reset.code(),
            });
        }

        var pipeline = transform_pipeline.Pipeline.init(allocator);
        defer pipeline.deinit();

        // Load transforms from build config
        pipeline.loadFromBuildConfig(cfg.?) catch |err| {
            if (verbose) {
                std.debug.print("{s}warning:{s} transform loading: {s}\n", .{
                    colors.warning.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
            }
        };

        // Sort transforms by dependencies
        pipeline.sortTransforms() catch |err| {
            if (verbose) {
                std.debug.print("{s}warning:{s} transform sort: {s}\n", .{
                    colors.warning.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
            }
            break :transform_blk program_ast;
        };

        if (pipeline.count() > 0) {
            const result = pipeline.run(parse_result.arena, allocator, program_ast, input_file) catch |err| {
                if (verbose) {
                    std.debug.print("{s}warning:{s} transform: {s}\n", .{
                        colors.warning.code(),
                        colors.Color.reset.code(),
                        @errorName(err),
                    });
                }
                break :transform_blk program_ast;
            };

            if (verbose) {
                if (result.changed) {
                    std.debug.print("{s}[{d}/{d}]{s} Transforms applied {s}✓{s} ({d} transforms)\n", .{
                        colors.success.code(),
                        phase_transform,
                        total_phases,
                        colors.Color.reset.code(),
                        colors.Color.bright_green.code(),
                        colors.Color.reset.code(),
                        result.transform_count,
                    });
                } else {
                    std.debug.print("{s}[{d}/{d}]{s} No transforms needed {s}✓{s}\n", .{
                        colors.success.code(),
                        phase_transform,
                        total_phases,
                        colors.Color.reset.code(),
                        colors.Color.bright_green.code(),
                        colors.Color.reset.code(),
                    });
                }
            }

            break :transform_blk result.node;
        } else {
            if (verbose) {
                std.debug.print("{s}[{d}/{d}]{s} No transforms configured {s}✓{s}\n", .{
                    colors.dim_text.code(),
                    phase_transform,
                    total_phases,
                    colors.Color.reset.code(),
                    colors.Color.bright_green.code(),
                    colors.Color.reset.code(),
                });
            }
            break :transform_blk program_ast;
        }
    } else program_ast;

    // Phase 3: Type checking
    const phase3 = nextPhase(&current_phase);
    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Type checking...\n", .{
            colors.info.code(),
            phase3,
            total_phases,
            colors.Color.reset.code(),
        });
    }

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

    // Wire module loader for cross-module import resolution
    type_checker.setModuleLoader(&loader);
    type_checker.setCurrentModulePath(input_file);

    const type_check_ok = type_checker.check(transformed_ast) catch |err| {
        std.debug.print("{s}error:{s} Type checking failed: {s}\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
            @errorName(err),
        });
        return;
    };

    if (!type_check_ok) {
        // Always show type errors
        std.debug.print("{s}error:{s} Type checking failed\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
        });
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
    } else if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Type checked {s}✓{s}\n", .{
            colors.success.code(),
            phase3,
            total_phases,
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    // AST Normalization (integrated into type checking phase - no separate phase number)
    const final_ast = if (options.enable_normalize and type_check_ok) blk: {
        var normalize_ctx = normalize.NormalizeContext.init(parse_result.arena, allocator, &type_checker);
        const normalized_ast = normalize.normalizeAST(&normalize_ctx, transformed_ast) catch {
            // Continue with unnormalized AST (normalization is optimization, not required)
            break :blk transformed_ast;
        };

        // Print normalization stats if any (only in verbose mode)
        if (verbose and (normalize_ctx.stats.object_spreads_normalized > 0 or
            normalize_ctx.stats.array_chains_fused > 0 or
            normalize_ctx.stats.closures_inlined > 0))
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
        break :blk normalized_ast;
    } else transformed_ast;

    // Phase 4: DRC analysis (for C backend)
    const phase4 = nextPhase(&current_phase);
    var drc: ?*Drc = null;
    var drc_storage: Drc = undefined;

    if (target == .c) {
        if (verbose) {
            std.debug.print("{s}[{d}/{d}]{s} Running DRC analysis...\n", .{
                colors.info.code(),
                phase4,
                total_phases,
                colors.Color.reset.code(),
            });
        }

        // Initialize DRC with move optimization enabled
        // Move optimization detects last-use of variables and elides RC ops
        drc_storage = Drc.initWithConfig(allocator, .{
            .enable_move_optimization = true,
            .enable_cycle_detection = true,
        });
        drc = &drc_storage;

        var analyzer = DrcAnalyzer.init(drc.?);
        analyzer.analyze(final_ast) catch |err| {
            if (verbose) {
                std.debug.print("{s}warning:{s} DRC analysis: {s}\n", .{
                    colors.warning.code(),
                    colors.Color.reset.code(),
                    @errorName(err),
                });
            }
            drc.?.deinit();
            drc = null; // Fall back to legacy mode
        };

        if (drc != null) {
            drc.?.finalize() catch |err| {
                if (verbose) {
                    std.debug.print("{s}warning:{s} DRC finalize: {s}\n", .{
                        colors.warning.code(),
                        colors.Color.reset.code(),
                        @errorName(err),
                    });
                }
                drc.?.deinit();
                drc = null;
            };
        }

        if (drc != null and verbose) {
            const stats = drc.?.getStats();
            // Build optimization suffix (RVO, stack alloc)
            var opt_buf: [128]u8 = undefined;
            var opt_suffix: []const u8 = "";
            if (stats.rvo_applied > 0 or stats.stack_allocations > 0) {
                const written = std.fmt.bufPrint(&opt_buf, ", {d} RVO, {d} stack", .{
                    stats.rvo_applied,
                    stats.stack_allocations,
                }) catch "";
                opt_suffix = written;
            }
            std.debug.print("{s}[{d}/{d}]{s} DRC analyzed {s}✓{s} ({d} vars, {d} RC ops, {d:.0}% elided{s})\n", .{
                colors.success.code(),
                phase4,
                total_phases,
                colors.Color.reset.code(),
                colors.Color.bright_green.code(),
                colors.Color.reset.code(),
                stats.variables_analyzed,
                stats.total_ops,
                stats.elisionRate() * 100,
                opt_suffix,
            });

            // Print DRC diagnostics to stderr (TODO-1.3: memory safety warnings)
            const drc_diagnostics = drc.?.getDiagnostics();
            if (drc_diagnostics.len > 0) {
                const stderr = std.io.getStdErr().writer();
                stderr.print("\n{s}DRC Diagnostics:{s}\n", .{
                    colors.Color.bright_yellow.code(),
                    colors.Color.reset.code(),
                }) catch {};
                for (drc_diagnostics) |diag| {
                    const severity_str = switch (diag.severity) {
                        .@"error" => colors.Color.bright_red.code(),
                        .warning => colors.Color.bright_yellow.code(),
                        .hint => colors.Color.bright_cyan.code(),
                    };
                    const severity_name = switch (diag.severity) {
                        .@"error" => "error",
                        .warning => "warning",
                        .hint => "hint",
                    };
                    const code_name = switch (diag.code) {
                        .use_after_move => "use-after-move",
                        .potential_cycle => "potential-cycle",
                        .uninitialized_use => "uninitialized-use",
                        .double_free_risk => "double-free-risk",
                    };
                    stderr.print("{s}{s}{s}[{s}]: line {d}:{d}: {s}\n", .{
                        severity_str,
                        severity_name,
                        colors.Color.reset.code(),
                        code_name,
                        diag.line,
                        diag.column,
                        diag.message,
                    }) catch {};
                }
                stderr.print("\n", .{}) catch {};
            }
        }
    } else if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Preparing codegen {s}✓{s}\n", .{
            colors.success.code(),
            phase4,
            total_phases,
            colors.Color.reset.code(),
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
    }

    defer if (drc) |d| d.deinit();

    // Phase 5: Backend code generation
    const phase5 = nextPhase(&current_phase);
    const target_name = switch (target) {
        .c => "C",
        .js => "JavaScript",
        .erlang => "Erlang",
    };

    if (verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Generating {s}...\n", .{
            colors.info.code(),
            phase5,
            total_phases,
            colors.Color.reset.code(),
            target_name,
        });
    }

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

            const out_path = getOutputPath(allocator, input_file, output_path, ".js") orelse return;
            defer if (output_path == null) allocator.free(out_path);

            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = js_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            printCompletionMessage(phase5, total_phases, out_path, start_time, verbose);
        },
        .c => {
            if (drc == null) {
                std.debug.print("{s}error:{s} DRC analysis is required for C backend\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                });
                return;
            }

            // Pass TypeChecker symbols to CGenerator for type-driven optimizations
            var gen = cgen.CGenerator.initWithSymbols(allocator, drc.?, &type_checker.symbols);
            defer gen.deinit();

            // Check if we have multiple modules loaded (imports present)
            const has_imports = loader.modules.count() > 1;

            const c_code = if (has_imports) blk: {
                if (verbose) {
                    std.debug.print("{s}  Multi-module:{s} bundling {d} modules\n", .{
                        colors.dim_text.code(),
                        colors.Color.reset.code(),
                        loader.modules.count(),
                    });
                }
                break :blk gen.generateMultiModule(&loader, input_file) catch |err| {
                    std.debug.print("{s}error:{s} C generation failed: {s}\n", .{
                        colors.error_color.code(),
                        colors.Color.reset.code(),
                        @errorName(err),
                    });
                    return;
                };
            } else blk: {
                break :blk gen.generate(final_ast) catch |err| {
                    std.debug.print("{s}error:{s} C generation failed: {s}\n", .{
                        colors.error_color.code(),
                        colors.Color.reset.code(),
                        @errorName(err),
                    });
                    return;
                };
            };
            defer allocator.free(c_code);

            const out_path = getOutputPath(allocator, input_file, output_path, ".c") orelse return;
            defer if (output_path == null) allocator.free(out_path);

            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = c_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            printCompletionMessage(phase5, total_phases, out_path, start_time, verbose);
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

            const out_path = getOutputPath(allocator, input_file, output_path, ".erl") orelse return;
            defer if (output_path == null) allocator.free(out_path);

            std.fs.cwd().writeFile(.{ .sub_path = out_path, .data = erl_code }) catch |err| {
                std.debug.print("{s}error:{s} Failed to write {s}: {s}\n", .{
                    colors.error_color.code(),
                    colors.Color.reset.code(),
                    out_path,
                    @errorName(err),
                });
                return;
            };

            printCompletionMessage(phase5, total_phases, out_path, start_time, verbose);
        },
    }
}

/// Helper to get output path with extension
fn getOutputPath(allocator: std.mem.Allocator, input_file: []const u8, output_path: ?[]const u8, ext: []const u8) ?[]const u8 {
    if (output_path) |p| return p;

    if (std.mem.endsWith(u8, input_file, ".ms")) {
        const base = input_file[0 .. input_file.len - 3];
        return std.fmt.allocPrint(allocator, "{s}{s}", .{ base, ext }) catch {
            std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
                colors.error_color.code(),
                colors.Color.reset.code(),
            });
            return null;
        };
    }
    return std.fmt.allocPrint(allocator, "{s}{s}", .{ input_file, ext }) catch {
        std.debug.print("{s}error:{s} Failed to allocate output path\n", .{
            colors.error_color.code(),
            colors.Color.reset.code(),
        });
        return null;
    };
}

/// Helper to print completion message
fn printCompletionMessage(phase: u8, total_phases: u8, out_path: []const u8, start_time: i64, show_verbose: bool) void {
    const elapsed = std.time.milliTimestamp() - start_time;

    if (show_verbose) {
        std.debug.print("{s}[{d}/{d}]{s} Generated {s} {s}✓{s}\n", .{
            colors.success.code(),
            phase,
            total_phases,
            colors.Color.reset.code(),
            out_path,
            colors.Color.bright_green.code(),
            colors.Color.reset.code(),
        });
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
    } else {
        // Cargo-style minimal output
        std.debug.print("{s}✓{s} Compiled {s} ({d}ms)\n", .{
            colors.success.code(),
            colors.Color.reset.code(),
            out_path,
            elapsed,
        });
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
