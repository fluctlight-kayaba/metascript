/// Score Codegen - Comprehensive Code Generation Quality Scoring
///
/// A Zig-native tool for evaluating compilation, execution, and code quality
/// across C, JS, and Erlang backends. Maps issues to compiler infrastructure
/// components for targeted improvements.
///
/// Usage:
///   zig build test-score-codegen              # Run full scoring
///   zig build test-score-codegen -- --backend=c   # C backend only
///   zig build test-score-codegen -- --verbose     # Detailed output
///
/// This replaces shell scripts with proper Zig tooling for:
/// - Type safety and cross-platform consistency
/// - Integration with existing test infrastructure
/// - Zig's excellent caching for fast iteration

const std = @import("std");
const testing = std.testing;
const helpers = @import("backend_test_helpers.zig");
const fixtures = @import("real_world_fixtures.zig");

const Backend = helpers.Backend;
const Allocator = std.mem.Allocator;

// ============================================================================
// Configuration
// ============================================================================

pub const Config = struct {
    backends: BackendSelection = .all,
    verbose: bool = false,
    save_baseline: bool = true,
    compare_baseline: bool = true,
    output_dir: []const u8 = ".score-codegen",

    pub const BackendSelection = enum {
        all,
        c_only,
        js_only,
        erlang_only,
        c_and_js,
        c_and_erlang,
        js_and_erlang,
    };

    pub fn shouldTestBackend(self: Config, backend: Backend) bool {
        return switch (self.backends) {
            .all => true,
            .c_only => backend == .c,
            .js_only => backend == .javascript,
            .erlang_only => backend == .erlang,
            .c_and_js => backend == .c or backend == .javascript,
            .c_and_erlang => backend == .c or backend == .erlang,
            .js_and_erlang => backend == .javascript or backend == .erlang,
        };
    }
};

// ============================================================================
// Scoring Metrics
// ============================================================================

/// Quality dimensions for generated code (1-10 scale)
pub const QualityScores = struct {
    readability: u8 = 0, // Naming, structure, comments
    efficiency: u8 = 0, // Allocations, redundant ops
    idiomaticity: u8 = 0, // Follows backend best practices
    safety: u8 = 0, // Error handling, null checks
    size_efficiency: u8 = 0, // Code bloat, dead code

    pub fn average(self: QualityScores) f64 {
        const sum: f64 = @floatFromInt(self.readability + self.efficiency +
            self.idiomaticity + self.safety + self.size_efficiency);
        return sum / 5.0;
    }

    pub fn weightedScore(self: QualityScores) f64 {
        // Weights: readability 20%, efficiency 25%, idiomaticity 20%, safety 25%, size 10%
        const r: f64 = @floatFromInt(self.readability);
        const e: f64 = @floatFromInt(self.efficiency);
        const i: f64 = @floatFromInt(self.idiomaticity);
        const s: f64 = @floatFromInt(self.safety);
        const z: f64 = @floatFromInt(self.size_efficiency);
        return (r * 0.20 + e * 0.25 + i * 0.20 + s * 0.25 + z * 0.10);
    }
};

/// Result for a single fixture on a single backend
pub const FixtureScore = struct {
    fixture_name: []const u8,
    backend: Backend,

    // Phase 1: Compilation
    compile_success: bool = false,
    compile_time_ns: u64 = 0,
    compile_error: ?[]const u8 = null,
    generated_code: ?[]const u8 = null,
    generated_size_bytes: usize = 0,
    warning_count: usize = 0,

    // Phase 2: External compilation (zig cc / node --check / erlc)
    external_compile_success: bool = false,
    external_compile_error: ?[]const u8 = null,

    // Phase 3: Execution
    execute_success: bool = false,
    execute_time_ns: u64 = 0,
    execute_exit_code: u8 = 0,
    execute_stdout: ?[]const u8 = null,
    execute_stderr: ?[]const u8 = null,
    output_matches_expected: bool = false,

    // Phase 4: Quality analysis
    quality: QualityScores = .{},

    // Phase 5: Component attribution
    likely_component: ?CompilerComponent = null,
    attribution_confidence: f64 = 0.0,

    pub fn overallScore(self: FixtureScore) f64 {
        // Formula: compile*0.3 + execute*0.3 + quality*0.4
        const compile_score: f64 = if (self.compile_success and self.external_compile_success) 100.0 else 0.0;
        const execute_score: f64 = if (self.execute_success) 100.0 else 0.0;
        const quality_score: f64 = self.quality.weightedScore() * 10.0; // Scale to 100

        return compile_score * 0.3 + execute_score * 0.3 + quality_score * 0.4;
    }
};

/// Aggregated scores for a single backend
pub const BackendScore = struct {
    backend: Backend,
    total_fixtures: usize = 0,
    compile_success_count: usize = 0,
    external_compile_success_count: usize = 0,
    execute_success_count: usize = 0,
    total_compile_time_ns: u64 = 0,
    total_execute_time_ns: u64 = 0,
    total_generated_bytes: usize = 0,
    total_warnings: usize = 0,
    quality_sum: QualityScores = .{},

    pub fn compileRate(self: BackendScore) f64 {
        if (self.total_fixtures == 0) return 0.0;
        return @as(f64, @floatFromInt(self.compile_success_count)) /
            @as(f64, @floatFromInt(self.total_fixtures)) * 100.0;
    }

    pub fn externalCompileRate(self: BackendScore) f64 {
        if (self.compile_success_count == 0) return 0.0;
        return @as(f64, @floatFromInt(self.external_compile_success_count)) /
            @as(f64, @floatFromInt(self.compile_success_count)) * 100.0;
    }

    pub fn executeRate(self: BackendScore) f64 {
        if (self.external_compile_success_count == 0) return 0.0;
        return @as(f64, @floatFromInt(self.execute_success_count)) /
            @as(f64, @floatFromInt(self.external_compile_success_count)) * 100.0;
    }

    pub fn averageQuality(self: BackendScore) f64 {
        if (self.compile_success_count == 0) return 0.0;
        const count: f64 = @floatFromInt(self.compile_success_count);
        const r: f64 = @floatFromInt(self.quality_sum.readability);
        const e: f64 = @floatFromInt(self.quality_sum.efficiency);
        const i: f64 = @floatFromInt(self.quality_sum.idiomaticity);
        const s: f64 = @floatFromInt(self.quality_sum.safety);
        const z: f64 = @floatFromInt(self.quality_sum.size_efficiency);
        return (r + e + i + s + z) / (count * 5.0);
    }

    pub fn overallScore(self: BackendScore) f64 {
        return self.compileRate() * 0.3 + self.executeRate() * 0.3 + self.averageQuality() * 10.0 * 0.4;
    }

    pub fn addFixture(self: *BackendScore, score: FixtureScore) void {
        self.total_fixtures += 1;
        if (score.compile_success) {
            self.compile_success_count += 1;
            self.total_generated_bytes += score.generated_size_bytes;
            self.total_warnings += score.warning_count;
            self.quality_sum.readability += score.quality.readability;
            self.quality_sum.efficiency += score.quality.efficiency;
            self.quality_sum.idiomaticity += score.quality.idiomaticity;
            self.quality_sum.safety += score.quality.safety;
            self.quality_sum.size_efficiency += score.quality.size_efficiency;
        }
        if (score.external_compile_success) {
            self.external_compile_success_count += 1;
        }
        if (score.execute_success) {
            self.execute_success_count += 1;
        }
        self.total_compile_time_ns += score.compile_time_ns;
        self.total_execute_time_ns += score.execute_time_ns;
    }
};

// ============================================================================
// Compiler Component Attribution
// ============================================================================

pub const CompilerComponent = enum {
    lexer,
    ast_generation,
    ast_analyzer,
    trans_am_cache,
    drc_orc,
    lobster_optimization,
    macro_system,
    c_backend,
    js_backend,
    erlang_backend,

    pub fn toString(self: CompilerComponent) []const u8 {
        return switch (self) {
            .lexer => "Lexer",
            .ast_generation => "AST Generation (Parser)",
            .ast_analyzer => "AST Analyzer (Type Checker)",
            .trans_am_cache => "Trans-Am Cache",
            .drc_orc => "DRC/ORC (Dependency Resolution)",
            .lobster_optimization => "Lobster Optimization",
            .macro_system => "Macro System",
            .c_backend => "C Backend Codegen",
            .js_backend => "JavaScript Backend Codegen",
            .erlang_backend => "Erlang Backend Codegen",
        };
    }

    pub fn sourceDir(self: CompilerComponent) []const u8 {
        return switch (self) {
            .lexer => "src/lexer/",
            .ast_generation => "src/parser/, src/ast/",
            .ast_analyzer => "src/analyzer/, src/checker/",
            .trans_am_cache => "src/cache/, src/trans_am/",
            .drc_orc => "src/drc/, src/resolver/",
            .lobster_optimization => "src/optimizer/, src/lobster/",
            .macro_system => "src/macro/, src/metaprogramming/",
            .c_backend => "src/backend/c/",
            .js_backend => "src/backend/js/",
            .erlang_backend => "src/backend/erlang/",
        };
    }
};

/// Issue with component attribution
pub const ComponentIssue = struct {
    component: CompilerComponent,
    severity: Severity,
    fixture_name: []const u8,
    backend: Backend,
    description: []const u8,
    error_message: ?[]const u8,
    confidence: f64, // 0.0 - 1.0

    pub const Severity = enum {
        critical, // Compilation failure
        major, // Execution failure
        minor, // Quality issue
        warning, // Non-blocking concern
    };
};

/// Attribute an error to a compiler component based on error patterns
pub fn attributeError(
    error_msg: ?[]const u8,
    backend: Backend,
    phase: enum { compile, external_compile, execute },
) struct { component: CompilerComponent, confidence: f64 } {
    const msg = error_msg orelse return .{ .component = backendToComponent(backend), .confidence = 0.3 };

    // Lexer patterns
    if (containsAny(msg, &.{ "unexpected token", "invalid character", "unterminated string", "unterminated comment" })) {
        return .{ .component = .lexer, .confidence = 0.9 };
    }

    // Parser patterns
    if (containsAny(msg, &.{ "parse error", "syntax error", "unexpected EOF", "expected" })) {
        return .{ .component = .ast_generation, .confidence = 0.85 };
    }

    // Type checker patterns
    if (containsAny(msg, &.{ "type mismatch", "undefined variable", "incompatible types", "cannot assign" })) {
        return .{ .component = .ast_analyzer, .confidence = 0.85 };
    }

    // Macro patterns
    if (containsAny(msg, &.{ "macro expansion", "@derive", "@comptime", "hygiene" })) {
        return .{ .component = .macro_system, .confidence = 0.9 };
    }

    // Backend-specific during external compile
    if (phase == .external_compile or phase == .execute) {
        return .{ .component = backendToComponent(backend), .confidence = 0.8 };
    }

    // Default to backend
    return .{ .component = backendToComponent(backend), .confidence = 0.5 };
}

fn containsAny(haystack: []const u8, needles: []const []const u8) bool {
    for (needles) |needle| {
        if (std.mem.indexOf(u8, haystack, needle) != null) return true;
    }
    return false;
}

fn backendToComponent(backend: Backend) CompilerComponent {
    return switch (backend) {
        .c => .c_backend,
        .javascript => .js_backend,
        .erlang => .erlang_backend,
    };
}

// ============================================================================
// Code Quality Analyzer
// ============================================================================

pub const QualityAnalyzer = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) QualityAnalyzer {
        return .{ .allocator = allocator };
    }

    /// Analyze generated code quality for a specific backend
    pub fn analyze(self: *QualityAnalyzer, code: []const u8, backend: Backend) QualityScores {
        _ = self;
        return switch (backend) {
            .c => analyzeC(code),
            .javascript => analyzeJS(code),
            .erlang => analyzeErlang(code),
        };
    }

    fn analyzeC(code: []const u8) QualityScores {
        var scores = QualityScores{};

        // Readability: Check for reasonable line lengths, naming
        scores.readability = 7; // Base score
        if (std.mem.indexOf(u8, code, "/* ") != null) scores.readability += 1; // Has comments
        if (countLines(code) < 500) scores.readability += 1; // Not too long

        // Efficiency: Check for obvious inefficiencies
        scores.efficiency = 7;
        if (std.mem.indexOf(u8, code, "malloc") != null and
            std.mem.indexOf(u8, code, "free") != null)
        {
            scores.efficiency += 1; // Memory managed
        }
        if (std.mem.indexOf(u8, code, "goto") == null) scores.efficiency += 1; // No goto

        // Idiomaticity: C best practices
        scores.idiomaticity = 6;
        if (std.mem.indexOf(u8, code, "const ") != null) scores.idiomaticity += 1;
        if (std.mem.indexOf(u8, code, "static ") != null) scores.idiomaticity += 1;
        if (std.mem.indexOf(u8, code, "#include") != null) scores.idiomaticity += 1;

        // Safety: Null checks, bounds
        scores.safety = 6;
        if (std.mem.indexOf(u8, code, "NULL") != null or
            std.mem.indexOf(u8, code, "nullptr") != null)
        {
            scores.safety += 1; // Uses null
        }
        if (std.mem.indexOf(u8, code, "if (") != null) scores.safety += 1; // Has conditionals

        // Size efficiency
        scores.size_efficiency = 8;
        const lines = countLines(code);
        if (lines < 100) scores.size_efficiency = 9;
        if (lines < 50) scores.size_efficiency = 10;

        return scores;
    }

    fn analyzeJS(code: []const u8) QualityScores {
        var scores = QualityScores{};

        // Readability
        scores.readability = 7;
        if (std.mem.indexOf(u8, code, "// ") != null) scores.readability += 1;
        if (countLines(code) < 300) scores.readability += 1;

        // Efficiency
        scores.efficiency = 7;
        if (std.mem.indexOf(u8, code, "const ") != null) scores.efficiency += 1;
        if (std.mem.indexOf(u8, code, "var ") == null) scores.efficiency += 1; // No var

        // Idiomaticity: Modern JS
        scores.idiomaticity = 6;
        if (std.mem.indexOf(u8, code, "const ") != null or
            std.mem.indexOf(u8, code, "let ") != null)
        {
            scores.idiomaticity += 2;
        }
        if (std.mem.indexOf(u8, code, "function ") != null) scores.idiomaticity += 1;

        // Safety
        scores.safety = 6;
        if (std.mem.indexOf(u8, code, "===") != null) scores.safety += 2; // Strict equality
        if (std.mem.indexOf(u8, code, "try") != null) scores.safety += 1;

        // Size
        scores.size_efficiency = 8;
        const lines = countLines(code);
        if (lines < 100) scores.size_efficiency = 9;
        if (lines < 50) scores.size_efficiency = 10;

        return scores;
    }

    fn analyzeErlang(code: []const u8) QualityScores {
        var scores = QualityScores{};

        // Readability
        scores.readability = 7;
        if (std.mem.indexOf(u8, code, "%%") != null) scores.readability += 1;
        if (countLines(code) < 200) scores.readability += 1;

        // Efficiency: Tail recursion
        scores.efficiency = 6;
        // Look for tail-recursive patterns (function call as last expression)
        if (std.mem.indexOf(u8, code, "lists:") != null) scores.efficiency += 1;
        if (std.mem.indexOf(u8, code, "tail") != null) scores.efficiency += 1;

        // Idiomaticity: Erlang patterns
        scores.idiomaticity = 6;
        if (std.mem.indexOf(u8, code, "-module") != null) scores.idiomaticity += 1;
        if (std.mem.indexOf(u8, code, "-export") != null) scores.idiomaticity += 1;
        if (std.mem.indexOf(u8, code, "case ") != null) scores.idiomaticity += 1;

        // Safety: Pattern matching
        scores.safety = 7;
        if (std.mem.indexOf(u8, code, "->") != null) scores.safety += 1;
        if (std.mem.indexOf(u8, code, "catch") != null) scores.safety += 1;

        // Size
        scores.size_efficiency = 8;
        const lines = countLines(code);
        if (lines < 100) scores.size_efficiency = 9;
        if (lines < 50) scores.size_efficiency = 10;

        return scores;
    }

    fn countLines(code: []const u8) usize {
        var count: usize = 1;
        for (code) |c| {
            if (c == '\n') count += 1;
        }
        return count;
    }
};

// ============================================================================
// External Compilation (using zig cc for C)
// ============================================================================

pub const ExternalCompiler = struct {
    allocator: Allocator,
    temp_dir: []const u8 = "/tmp/metascript_score",

    pub fn init(allocator: Allocator) ExternalCompiler {
        return .{ .allocator = allocator };
    }

    /// Compile C code using zig cc (fast + cached)
    pub fn compileC(self: *ExternalCompiler, c_code: []const u8) !helpers.ExternalCompileResult {
        // Ensure temp dir exists
        std.fs.makeDirAbsolute(self.temp_dir) catch {};

        // Write C source
        const c_file = try std.fmt.allocPrint(self.allocator, "{s}/test_{d}.c", .{
            self.temp_dir,
            std.time.milliTimestamp(),
        });
        defer self.allocator.free(c_file);

        const file = try std.fs.createFileAbsolute(c_file, .{});
        try file.writeAll(c_code);
        file.close();

        // Compile with zig cc (excellent caching!)
        const out_file = try std.fmt.allocPrint(self.allocator, "{s}/test.o", .{self.temp_dir});
        defer self.allocator.free(out_file);

        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{
                "zig", "cc",
                "-c", // Compile only
                "-O2",
                "-Wall",
                "-Wextra",
                c_file,
                "-o", out_file,
            },
        }) catch |err| {
            return helpers.ExternalCompileResult{
                .success = false,
                .stdout = "",
                .stderr = try std.fmt.allocPrint(self.allocator, "Failed to run zig cc: {s}", .{@errorName(err)}),
                .exit_code = 1,
                .allocator = self.allocator,
            };
        };

        return helpers.ExternalCompileResult{
            .success = result.term.Exited == 0,
            .stdout = result.stdout,
            .stderr = result.stderr,
            .exit_code = @intCast(result.term.Exited),
            .allocator = self.allocator,
        };
    }

    /// Check JS syntax with node --check
    pub fn checkJS(self: *ExternalCompiler, js_code: []const u8) !helpers.ExternalCompileResult {
        std.fs.makeDirAbsolute(self.temp_dir) catch {};

        const js_file = try std.fmt.allocPrint(self.allocator, "{s}/test_{d}.js", .{
            self.temp_dir,
            std.time.milliTimestamp(),
        });
        defer self.allocator.free(js_file);

        const file = try std.fs.createFileAbsolute(js_file, .{});
        try file.writeAll(js_code);
        file.close();

        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{ "node", "--check", js_file },
        }) catch |err| {
            return helpers.ExternalCompileResult{
                .success = false,
                .stdout = "",
                .stderr = try std.fmt.allocPrint(self.allocator, "Failed to run node: {s}", .{@errorName(err)}),
                .exit_code = 1,
                .allocator = self.allocator,
            };
        };

        return helpers.ExternalCompileResult{
            .success = result.term.Exited == 0,
            .stdout = result.stdout,
            .stderr = result.stderr,
            .exit_code = @intCast(result.term.Exited),
            .allocator = self.allocator,
        };
    }

    /// Compile Erlang with erlc
    pub fn compileErlang(self: *ExternalCompiler, erl_code: []const u8) !helpers.ExternalCompileResult {
        std.fs.makeDirAbsolute(self.temp_dir) catch {};

        // Extract module name from code
        const module_name = extractErlangModule(erl_code) orelse "test_module";

        const erl_file = try std.fmt.allocPrint(self.allocator, "{s}/{s}.erl", .{
            self.temp_dir,
            module_name,
        });
        defer self.allocator.free(erl_file);

        const file = try std.fs.createFileAbsolute(erl_file, .{});
        try file.writeAll(erl_code);
        file.close();

        const result = std.process.Child.run(.{
            .allocator = self.allocator,
            .argv = &[_][]const u8{ "erlc", "-o", self.temp_dir, erl_file },
        }) catch |err| {
            return helpers.ExternalCompileResult{
                .success = false,
                .stdout = "",
                .stderr = try std.fmt.allocPrint(self.allocator, "Failed to run erlc: {s}", .{@errorName(err)}),
                .exit_code = 1,
                .allocator = self.allocator,
            };
        };

        return helpers.ExternalCompileResult{
            .success = result.term.Exited == 0,
            .stdout = result.stdout,
            .stderr = result.stderr,
            .exit_code = @intCast(result.term.Exited),
            .allocator = self.allocator,
        };
    }

    fn extractErlangModule(code: []const u8) ?[]const u8 {
        // Find -module(name).
        const start = std.mem.indexOf(u8, code, "-module(") orelse return null;
        const name_start = start + 8;
        const end = std.mem.indexOfPos(u8, code, name_start, ")") orelse return null;
        return code[name_start..end];
    }
};

// ============================================================================
// Score Runner
// ============================================================================

pub const ScoreRunner = struct {
    allocator: Allocator,
    config: Config,
    quality_analyzer: QualityAnalyzer,
    external_compiler: ExternalCompiler,
    fixture_scores: std.ArrayList(FixtureScore),
    backend_scores: struct {
        c: BackendScore,
        js: BackendScore,
        erlang: BackendScore,
    },
    component_issues: std.ArrayList(ComponentIssue),

    pub fn init(allocator: Allocator, config: Config) ScoreRunner {
        return .{
            .allocator = allocator,
            .config = config,
            .quality_analyzer = QualityAnalyzer.init(allocator),
            .external_compiler = ExternalCompiler.init(allocator),
            .fixture_scores = std.ArrayList(FixtureScore).init(allocator),
            .backend_scores = .{
                .c = BackendScore{ .backend = .c },
                .js = BackendScore{ .backend = .javascript },
                .erlang = BackendScore{ .backend = .erlang },
            },
            .component_issues = std.ArrayList(ComponentIssue).init(allocator),
        };
    }

    pub fn deinit(self: *ScoreRunner) void {
        self.fixture_scores.deinit();
        self.component_issues.deinit();
    }

    /// Score a single fixture on a single backend
    pub fn scoreFixture(
        self: *ScoreRunner,
        name: []const u8,
        source: []const u8,
        backend: Backend,
    ) !FixtureScore {
        var score = FixtureScore{
            .fixture_name = name,
            .backend = backend,
        };

        // Phase 1: MetaScript compilation
        const compile_start = std.time.nanoTimestamp();
        var compile_result = helpers.compile(self.allocator, source, backend) catch |err| {
            score.compile_error = @errorName(err);
            const attr = attributeError(score.compile_error, backend, .compile);
            score.likely_component = attr.component;
            score.attribution_confidence = attr.confidence;
            return score;
        };
        score.compile_time_ns = @intCast(std.time.nanoTimestamp() - compile_start);

        if (!compile_result.success) {
            score.compile_error = compile_result.error_message;
            const attr = attributeError(compile_result.error_message, backend, .compile);
            score.likely_component = attr.component;
            score.attribution_confidence = attr.confidence;
            compile_result.deinit();
            return score;
        }

        score.compile_success = true;
        score.generated_code = compile_result.output;
        score.generated_size_bytes = compile_result.output.len;

        // Phase 2: External compilation
        defer compile_result.deinit(); // Clean up after external compile uses the output

        var ext_result = switch (backend) {
            .c => try self.external_compiler.compileC(compile_result.output),
            .javascript => try self.external_compiler.checkJS(compile_result.output),
            .erlang => try self.external_compiler.compileErlang(compile_result.output),
        };
        defer ext_result.deinit();

        score.external_compile_success = ext_result.success;
        if (!ext_result.success) {
            score.external_compile_error = try self.allocator.dupe(u8, ext_result.stderr);
            const attr = attributeError(ext_result.stderr, backend, .external_compile);
            score.likely_component = attr.component;
            score.attribution_confidence = attr.confidence;
        }

        // Phase 3: Quality analysis (only if compilation succeeded)
        if (score.compile_success) {
            score.quality = self.quality_analyzer.analyze(compile_result.output, backend);
        }

        // TODO: Phase 4: Execution (requires building full binary)

        return score;
    }

    /// Run scoring on all fixtures
    pub fn run(self: *ScoreRunner) !void {
        const fixture_list = getFixtureList();

        for (fixture_list) |fixture| {
            // Score on each enabled backend
            if (self.config.shouldTestBackend(.c)) {
                const score = try self.scoreFixture(fixture.name, fixture.source, .c);
                try self.fixture_scores.append(score);
                self.backend_scores.c.addFixture(score);

                if (score.likely_component) |comp| {
                    try self.component_issues.append(.{
                        .component = comp,
                        .severity = if (!score.compile_success) .critical else if (!score.external_compile_success) .major else .minor,
                        .fixture_name = fixture.name,
                        .backend = .c,
                        .description = "Compilation or quality issue",
                        .error_message = score.compile_error orelse score.external_compile_error,
                        .confidence = score.attribution_confidence,
                    });
                }
            }

            if (self.config.shouldTestBackend(.javascript)) {
                const score = try self.scoreFixture(fixture.name, fixture.source, .javascript);
                try self.fixture_scores.append(score);
                self.backend_scores.js.addFixture(score);
            }

            if (self.config.shouldTestBackend(.erlang)) {
                const score = try self.scoreFixture(fixture.name, fixture.source, .erlang);
                try self.fixture_scores.append(score);
                self.backend_scores.erlang.addFixture(score);
            }
        }
    }

    /// Print the score report
    pub fn printReport(self: *ScoreRunner) void {
        const sep = "═══════════════════════════════════════════════════════════════════════";
        const thin_sep = "───────────────────────────────────────────────────────────────────────";

        std.debug.print("\n{s}\n", .{sep});
        std.debug.print("                    METASCRIPT CODEGEN SCORE REPORT\n", .{});
        std.debug.print("                    {d}\n", .{std.time.timestamp()});
        std.debug.print("{s}\n\n", .{sep});

        // Backend Scores Table
        std.debug.print("BACKEND SCORES\n", .{});
        std.debug.print("{s}\n", .{thin_sep});
        std.debug.print("│ Backend    │ Compile% │ ExtComp% │ Quality │ Overall │\n", .{});
        std.debug.print("{s}\n", .{thin_sep});

        if (self.config.shouldTestBackend(.c)) {
            const c = &self.backend_scores.c;
            std.debug.print("│ C          │  {d:5.1}%  │  {d:5.1}%  │  {d:.1}/10 │  {d:5.1}  │\n", .{
                c.compileRate(),
                c.externalCompileRate(),
                c.averageQuality(),
                c.overallScore(),
            });
        }

        if (self.config.shouldTestBackend(.javascript)) {
            const js = &self.backend_scores.js;
            std.debug.print("│ JavaScript │  {d:5.1}%  │  {d:5.1}%  │  {d:.1}/10 │  {d:5.1}  │\n", .{
                js.compileRate(),
                js.externalCompileRate(),
                js.averageQuality(),
                js.overallScore(),
            });
        }

        if (self.config.shouldTestBackend(.erlang)) {
            const erl = &self.backend_scores.erlang;
            std.debug.print("│ Erlang     │  {d:5.1}%  │  {d:5.1}%  │  {d:.1}/10 │  {d:5.1}  │\n", .{
                erl.compileRate(),
                erl.externalCompileRate(),
                erl.averageQuality(),
                erl.overallScore(),
            });
        }

        std.debug.print("{s}\n\n", .{thin_sep});

        // Component Issues Summary
        std.debug.print("COMPONENT ISSUES\n", .{});
        std.debug.print("{s}\n", .{thin_sep});

        // Count issues by component
        const component_count = @typeInfo(CompilerComponent).@"enum".fields.len;
        var component_counts: [component_count]usize = .{0} ** component_count;
        for (self.component_issues.items) |issue| {
            component_counts[@intFromEnum(issue.component)] += 1;
        }

        // Print components with issues
        inline for (@typeInfo(CompilerComponent).@"enum".fields, 0..) |field, i| {
            if (component_counts[i] > 0) {
                const comp: CompilerComponent = @enumFromInt(i);
                std.debug.print("│ {s}: {d} issues\n", .{ comp.toString(), component_counts[i] });
            }
            _ = field;
        }

        std.debug.print("{s}\n\n", .{thin_sep});

        // Health assessment
        std.debug.print("HEALTH ASSESSMENT\n", .{});
        std.debug.print("{s}\n", .{thin_sep});

        if (self.config.shouldTestBackend(.c)) {
            printHealthStatus(.c, self.backend_scores.c.overallScore());
        }
        if (self.config.shouldTestBackend(.javascript)) {
            printHealthStatus(.javascript, self.backend_scores.js.overallScore());
        }
        if (self.config.shouldTestBackend(.erlang)) {
            printHealthStatus(.erlang, self.backend_scores.erlang.overallScore());
        }

        std.debug.print("\n{s}\n\n", .{sep});
    }

    fn printHealthStatus(backend: Backend, score: f64) void {
        const status = if (score >= 80.0)
            "PRODUCTION-READY"
        else if (score >= 50.0)
            "BETA"
        else
            "ALPHA";

        const emoji = if (score >= 80.0) "+" else if (score >= 50.0) "~" else "!";

        std.debug.print("[{s}] {s} Backend: {s} ({d:.1}/100)\n", .{
            emoji,
            backend.toString(),
            status,
            score,
        });
    }
};

// ============================================================================
// Fixture List
// ============================================================================

const Fixture = struct {
    name: []const u8,
    source: []const u8,
};

fn getFixtureList() []const Fixture {
    return &[_]Fixture{
        .{ .name = "SIMPLE_FUNCTION", .source = fixtures.SIMPLE_FUNCTION },
        .{ .name = "FACTORIAL_RECURSIVE", .source = fixtures.FACTORIAL_RECURSIVE },
        .{ .name = "FACTORIAL_ITERATIVE", .source = fixtures.FACTORIAL_ITERATIVE },
        .{ .name = "FIBONACCI", .source = fixtures.FIBONACCI },
        .{ .name = "WHILE_LOOP_COUNTER", .source = fixtures.WHILE_LOOP_COUNTER },
        .{ .name = "FOR_LOOP_SUM", .source = fixtures.FOR_LOOP_SUM },
        .{ .name = "NESTED_LOOPS", .source = fixtures.NESTED_LOOPS },
        .{ .name = "EARLY_RETURN", .source = fixtures.EARLY_RETURN },
        .{ .name = "VARIABLE_SHADOWING_SIMPLE", .source = fixtures.VARIABLE_SHADOWING_SIMPLE },
        .{ .name = "VARIABLE_SHADOWING_MULTIPLE", .source = fixtures.VARIABLE_SHADOWING_MULTIPLE },
        .{ .name = "OBJECT_LITERAL", .source = fixtures.OBJECT_LITERAL },
        .{ .name = "OBJECT_MEMBER_ACCESS", .source = fixtures.OBJECT_MEMBER_ACCESS },
        .{ .name = "ARRAY_OPERATIONS", .source = fixtures.ARRAY_OPERATIONS },
        .{ .name = "SIMPLE_CLASS", .source = fixtures.SIMPLE_CLASS },
        .{ .name = "CLASS_WITH_METHODS", .source = fixtures.CLASS_WITH_METHODS },
        .{ .name = "QUICKSORT", .source = fixtures.QUICKSORT },
        .{ .name = "BINARY_SEARCH", .source = fixtures.BINARY_SEARCH },
        .{ .name = "IS_PRIME", .source = fixtures.IS_PRIME },
    };
}

// ============================================================================
// Test Entry Point
// ============================================================================

test "score-codegen: comprehensive backend quality scoring" {
    var runner = ScoreRunner.init(testing.allocator, .{
        .backends = .all,
        .verbose = true,
    });
    defer runner.deinit();

    try runner.run();
    runner.printReport();

    // This test always passes - it's for metrics reporting
}

test "score-codegen: C backend only" {
    var runner = ScoreRunner.init(testing.allocator, .{
        .backends = .c_only,
        .verbose = false,
    });
    defer runner.deinit();

    try runner.run();
    runner.printReport();
}

test "score-codegen: JavaScript backend only" {
    var runner = ScoreRunner.init(testing.allocator, .{
        .backends = .js_only,
        .verbose = false,
    });
    defer runner.deinit();

    try runner.run();
    runner.printReport();
}

test "score-codegen: Erlang backend only" {
    var runner = ScoreRunner.init(testing.allocator, .{
        .backends = .erlang_only,
        .verbose = false,
    });
    defer runner.deinit();

    try runner.run();
    runner.printReport();
}
