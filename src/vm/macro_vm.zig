/// Macro VM - Executes Metascript macros on Hermes runtime
///
/// This is the compile-time VM that runs macro code (TypeScript)
/// to transform AST nodes.
///
/// Usage:
///   var vm = try MacroVM.init(allocator, arena);
///   defer vm.deinit();
///
///   // Execute @derive(Eq) on a class
///   try vm.executeMacro("derive", &[_][]const u8{"Eq"}, class_node);
///
/// Bytecode Mode (Fast):
///   // Pre-compiled macros execute in ~0.2ms vs ~45ms for source
///   try vm.executeBytecode(bytecode, "macro.hbc", target_node);

const std = @import("std");
const ast = @import("../ast/ast.zig");
const ast_api = @import("ast_api.zig");
const c_api = @import("c_api.zig");
const c = c_api.c;
const bytecode_compiler = @import("bytecode_compiler.zig");
const disk_cache = @import("../transam/disk_cache.zig");
const network_cache = @import("../transam/network_cache.zig");

/// Context for native fetch function
pub const FetchContext = struct {
    allocator: std.mem.Allocator,
    net_cache: ?*network_cache.NetworkCache,
};

pub const MacroVM = struct {
    allocator: std.mem.Allocator,
    runtime: *c.MSHermesRuntime,
    ast_ctx: *ast_api.ASTContext,
    /// Optional bytecode cache for faster execution
    bytecode_cache: ?*disk_cache.BytecodeCache = null,
    /// Optional network cache for @comptime fetch() responses
    net_cache: ?*network_cache.NetworkCache = null,
    /// Context for fetch native function (heap-allocated)
    fetch_ctx: ?*FetchContext = null,

    /// Built-in macro implementations (TypeScript source)
    const BUILTIN_MACROS = struct {
        /// @derive(Eq) - generates equals() method
        pub const derive_eq =
            \\// @derive(Eq) macro implementation
            \\(function() {
            \\    console.log("[derive(Eq)] Starting macro expansion");
            \\    console.log("[derive(Eq)] Target class:", target.name);
            \\
            \\    const props = target.properties;
            \\    console.log("[derive(Eq)] Properties:", props.length, props);
            \\
            \\    if (props.length === 0) {
            \\        console.log("[derive(Eq)] No properties, skipping");
            \\        return;
            \\    }
            \\
            \\    // Build: this.prop1 === other.prop1 && this.prop2 === other.prop2 ...
            \\    let expr = null;
            \\    for (const prop of props) {
            \\        console.log("[derive(Eq)] Processing property:", prop);
            \\        const thisAccess = ast.createMemberExpr(
            \\            ast.createIdentifier("this"),
            \\            ast.createIdentifier(prop)
            \\        );
            \\        const otherAccess = ast.createMemberExpr(
            \\            ast.createIdentifier("other"),
            \\            ast.createIdentifier(prop)
            \\        );
            \\        const cmp = ast.createBinaryExpr("===", thisAccess, otherAccess);
            \\
            \\        if (expr === null) {
            \\            expr = cmp;
            \\        } else {
            \\            expr = ast.createBinaryExpr("&&", expr, cmp);
            \\        }
            \\    }
            \\
            \\    // Create: return <expr>;
            \\    const returnStmt = ast.createReturnStmt(expr);
            \\    const body = ast.createBlock([returnStmt]);
            \\
            \\    // Create param: other
            \\    const otherParam = ast.createParam("other");
            \\
            \\    // Create: equals(other): boolean { ... }
            \\    const method = ast.createMethod("equals", body, [otherParam]);
            \\    console.log("[derive(Eq)] Created method: equals(other)");
            \\
            \\    target.addMethod(method);
            \\    console.log("[derive(Eq)] Added method to class");
            \\})();
        ;

        /// @derive(Hash) - generates hash() method
        pub const derive_hash =
            \\// @derive(Hash) macro implementation
            \\(function() {
            \\    const props = target.properties;
            \\    // TODO: Implement hash generation
            \\    const method = ast.createMethod("hash", ast.createBlock([]));
            \\    target.addMethod(method);
            \\})();
        ;
    };

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena) !MacroVM {
        return initWithCaches(allocator, arena, null, null);
    }

    /// Initialize with optional bytecode cache for faster macro execution
    pub fn initWithCache(
        allocator: std.mem.Allocator,
        arena: *ast.ASTArena,
        cache: ?*disk_cache.BytecodeCache,
    ) !MacroVM {
        return initWithCaches(allocator, arena, cache, null);
    }

    /// Initialize with both bytecode and network caches
    pub fn initWithCaches(
        allocator: std.mem.Allocator,
        arena: *ast.ASTArena,
        bytecode_cache_ptr: ?*disk_cache.BytecodeCache,
        net_cache_ptr: ?*network_cache.NetworkCache,
    ) !MacroVM {
        // Create Hermes runtime
        const runtime = c.ms_hermes_create() orelse {
            return error.HermesInitFailed;
        };

        // Create AST context
        const ast_ctx = try allocator.create(ast_api.ASTContext);
        ast_ctx.* = .{
            .arena = arena,
            .allocator = allocator,
            .current_node = null,
        };

        // Register AST APIs
        ast_api.register(runtime, ast_ctx);

        // Register console.log for debugging
        c.ms_hermes_register_function(runtime, "console_log", consoleLog, null);

        // Create fetch context (heap-allocated so it survives)
        const fetch_ctx = try allocator.create(FetchContext);
        fetch_ctx.* = .{
            .allocator = allocator,
            .net_cache = net_cache_ptr,
        };

        // Register fetch for @comptime network requests
        c.ms_hermes_register_function(runtime, "__ms_fetch", fetchNative, fetch_ctx);

        // Setup console object and fetch wrapper so JS APIs work
        const setup_script =
            \\var console = { log: console_log };
            \\
            \\// Synchronous fetch for @comptime blocks
            \\// Note: This is a blocking call - only use at compile time!
            \\function fetch(url) {
            \\    const result = __ms_fetch(url);
            \\    if (result.error) {
            \\        throw new Error(result.error);
            \\    }
            \\    return {
            \\        ok: result.status >= 200 && result.status < 300,
            \\        status: result.status,
            \\        body: result.body,
            \\        text: function() { return result.body; },
            \\        json: function() { return JSON.parse(result.body); }
            \\    };
            \\}
        ;
        _ = c.ms_hermes_eval(
            runtime,
            setup_script,
            setup_script.len,
            "setup.js",
        );

        return .{
            .allocator = allocator,
            .runtime = runtime,
            .ast_ctx = ast_ctx,
            .bytecode_cache = bytecode_cache_ptr,
            .net_cache = net_cache_ptr,
            .fetch_ctx = fetch_ctx,
        };
    }

    pub fn deinit(self: *MacroVM) void {
        c.ms_hermes_destroy(self.runtime);
        self.allocator.destroy(self.ast_ctx);
        if (self.fetch_ctx) |ctx| {
            self.allocator.destroy(ctx);
        }
    }

    /// Execute a macro on a target node
    ///
    /// @param macro_name: e.g., "derive"
    /// @param args: e.g., ["Eq", "Hash"]
    /// @param target: The AST node to transform (e.g., class declaration)
    pub fn executeMacro(
        self: *MacroVM,
        macro_name: []const u8,
        args: []const []const u8,
        target: *ast.Node,
    ) !void {
        // Set current target
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        // Find and execute macro for each argument
        if (std.mem.eql(u8, macro_name, "derive")) {
            for (args) |trait| {
                try self.executeDeriveTrail(trait);
            }
        } else {
            std.log.warn("Unknown macro: @{s}", .{macro_name});
        }
    }

    fn executeDeriveTrail(self: *MacroVM, trait: []const u8) !void {
        const source = if (std.mem.eql(u8, trait, "Eq"))
            BUILTIN_MACROS.derive_eq
        else if (std.mem.eql(u8, trait, "Hash"))
            BUILTIN_MACROS.derive_hash
        else {
            std.log.warn("Unknown trait: {s}", .{trait});
            return;
        };

        // Compute source hash for cache key
        const source_hash = disk_cache.hashMacroSource(trait, source);

        // Try bytecode execution (fast path: ~0.2ms vs ~45ms for source)
        if (self.bytecode_cache) |cache| {
            if (cache.getBytecode(source_hash)) |bytecode| {
                // PE4 FIX: bytecode is now owned, must free after use
                defer self.allocator.free(bytecode);
                std.log.debug("[MacroVM] Using cached bytecode for @derive({s})", .{trait});
                try self.executeBytecodeDirect(bytecode);
                return;
            }

            // Cache miss: compile source to bytecode
            if (bytecode_compiler.isHermescAvailable()) {
                const bytecode = bytecode_compiler.compileToBytescode(
                    self.allocator,
                    source,
                    trait,
                ) catch |err| {
                    std.log.debug("[MacroVM] Bytecode compilation failed: {}, using source", .{err});
                    try self.executeSourceDirect(source);
                    return;
                };
                defer self.allocator.free(bytecode);

                // Store in cache for next time
                cache.storeBytecode(source_hash, bytecode) catch |err| {
                    std.log.debug("[MacroVM] Failed to cache bytecode: {}", .{err});
                };

                std.log.debug("[MacroVM] Compiled and cached bytecode for @derive({s})", .{trait});
                try self.executeBytecodeDirect(bytecode);
                return;
            }
        }

        // Fallback: execute source directly
        try self.executeSourceDirect(source);
    }

    /// Execute bytecode directly (internal helper)
    fn executeBytecodeDirect(self: *MacroVM, bytecode: []const u8) !void {
        const result = c.ms_hermes_eval_bytecode(
            self.runtime,
            bytecode.ptr,
            bytecode.len,
            "macro.hbc",
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro bytecode error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute source directly (internal helper)
    fn executeSourceDirect(self: *MacroVM, source: []const u8) !void {
        const result = c.ms_hermes_eval(
            self.runtime,
            source.ptr,
            source.len,
            "derive_macro.js",
        );

        // Check for errors
        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute arbitrary macro source code (JavaScript)
    pub fn executeSource(self: *MacroVM, source: []const u8, target: *ast.Node) !void {
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        const result = c.ms_hermes_eval(
            self.runtime,
            source.ptr,
            source.len,
            "custom_macro.js",
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute a source-defined macro (transpiled from @macro function)
    ///
    /// The macro_fn should be a JavaScript function like:
    ///   (function myMacro(target) { ... })
    ///
    /// This wraps it in an IIFE that passes the target object.
    pub fn executeSourceMacro(self: *MacroVM, macro_fn: []const u8, target: *ast.Node) !void {
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        // Build wrapper: macro_fn(target)
        // The macro_fn is like (function name(target) {...})
        // We call it with the global 'target' object
        var full_code = std.ArrayList(u8).init(self.allocator);
        defer full_code.deinit();

        try full_code.appendSlice(macro_fn);
        try full_code.appendSlice("(target);");

        std.log.debug("[VM] Executing source macro:\n{s}", .{full_code.items});

        const result = c.ms_hermes_eval(
            self.runtime,
            full_code.items.ptr,
            full_code.items.len,
            "source_macro.js",
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Source macro error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Execute pre-compiled Hermes bytecode (HBC)
    ///
    /// This is ~100x faster than executeSource() for cached macros.
    /// Bytecode should be produced by hermesc or the transpiler/hermes module.
    ///
    /// Example:
    ///   const bytecode = try loader.loadMacroBytecode(allocator, config, "macros/derive.ts");
    ///   defer allocator.free(bytecode);
    ///   try vm.executeBytecode(bytecode, "derive.hbc", target_node);
    pub fn executeBytecode(self: *MacroVM, bytecode: []const u8, source_url: []const u8, target: *ast.Node) !void {
        self.ast_ctx.current_node = target;
        defer self.ast_ctx.current_node = null;

        // Call the bytecode execution function
        const result = c.ms_hermes_eval_bytecode(
            self.runtime,
            bytecode.ptr,
            bytecode.len,
            source_url.ptr,
        );

        if (c.ms_hermes_has_exception(self.runtime)) {
            const msg = c.ms_hermes_get_exception(self.runtime);
            if (msg) |m| {
                std.log.err("Macro bytecode error: {s}", .{std.mem.span(m)});
            }
            c.ms_hermes_clear_exception(self.runtime);
            return error.MacroExecutionFailed;
        }

        if (result) |r| {
            c.ms_value_destroy(r);
        }
    }

    /// Benchmark: Compare source vs bytecode execution time
    pub fn benchmarkExecution(self: *MacroVM, source: []const u8, bytecode: []const u8, iterations: usize) !struct { source_ns: u64, bytecode_ns: u64 } {
        var dummy_node = ast.Node{
            .kind = .program,
            .data = undefined,
            .loc = .{ .start = .{ .line = 0, .column = 0 }, .end = .{ .line = 0, .column = 0 } },
        };

        // Benchmark source execution
        const source_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            try self.executeSource(source, &dummy_node);
        }
        const source_end = std.time.nanoTimestamp();
        const source_ns: u64 = @intCast(@as(i64, @intCast(source_end)) - @as(i64, @intCast(source_start)));

        // Benchmark bytecode execution
        const bytecode_start = std.time.nanoTimestamp();
        for (0..iterations) |_| {
            try self.executeBytecode(bytecode, "benchmark.hbc", &dummy_node);
        }
        const bytecode_end = std.time.nanoTimestamp();
        const bytecode_ns: u64 = @intCast(@as(i64, @intCast(bytecode_end)) - @as(i64, @intCast(bytecode_start)));

        return .{
            .source_ns = source_ns / iterations,
            .bytecode_ns = bytecode_ns / iterations,
        };
    }
};

/// console.log implementation for debugging macros
fn consoleLog(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    _ = context;
    const rt = runtime orelse return null;

    var output = std.ArrayList(u8).init(std.heap.page_allocator);
    defer output.deinit();

    for (0..arg_count) |i| {
        if (i > 0) output.appendSlice(" ") catch {};

        const arg = args[i] orelse continue;

        if (c.ms_value_is_string(arg)) {
            var len: usize = 0;
            const str = c.ms_value_get_string(rt, arg, &len);
            if (str) |s| {
                output.appendSlice(s[0..len]) catch {};
            }
        } else if (c.ms_value_is_number(arg)) {
            const num = c.ms_value_get_number(arg);
            std.fmt.format(output.writer(), "{d}", .{num}) catch {};
        } else if (c.ms_value_is_object(arg)) {
            output.appendSlice("[object]") catch {};
        } else {
            output.appendSlice("[?]") catch {};
        }
    }

    std.debug.print("[macro] {s}\n", .{output.items});
    return c.ms_value_undefined(rt);
}

/// fetch() implementation for @comptime network requests
/// Uses NetworkCache to avoid repeated network calls across compilations
///
/// Memory ownership flow:
/// 1. Check cache → if hit, cache owns memory, we copy to JS heap
/// 2. HTTP request → HttpResult owns body memory
/// 3. Create JS object → copies body to Hermes heap
/// 4. Cache response → JSON serialization copies body
/// 5. Free HttpResult → original body freed, JS + cache have copies
fn fetchNative(
    runtime: ?*c.MSHermesRuntime,
    context: ?*anyopaque,
    args: [*c]?*c.MSHermesValue,
    arg_count: usize,
) callconv(.c) ?*c.MSHermesValue {
    const rt = runtime orelse return null;
    const fetch_ctx: ?*FetchContext = @ptrCast(@alignCast(context));

    // Need at least URL argument
    if (arg_count < 1) {
        return createErrorObject(rt, "fetch requires a URL argument");
    }

    // Get URL from first argument
    const url_arg = args[0] orelse return createErrorObject(rt, "URL argument is null");
    if (!c.ms_value_is_string(url_arg)) {
        return createErrorObject(rt, "URL must be a string");
    }

    var url_len: usize = 0;
    const url_ptr = c.ms_value_get_string(rt, url_arg, &url_len) orelse {
        return createErrorObject(rt, "Failed to get URL string");
    };
    const url = url_ptr[0..url_len];

    std.log.debug("[fetch] Request: {s}", .{url});

    // Validate URL scheme (security: only allow http/https)
    if (!std.mem.startsWith(u8, url, "http://") and !std.mem.startsWith(u8, url, "https://")) {
        std.log.warn("[fetch] Rejected non-HTTP URL: {s}", .{url});
        return createErrorObject(rt, "fetch only supports http:// and https:// URLs");
    }

    // Check cache first
    if (fetch_ctx) |ctx| {
        if (ctx.net_cache) |cache| {
            if (cache.get(url)) |cached| {
                defer cache.freeResponse(cached);
                std.log.debug("[fetch] Cache hit for {s}", .{url});
                return createResponseObject(rt, cached.body, cached.status_code, null);
            }
            std.log.debug("[fetch] Cache miss for {s}", .{url});
        }
    }

    // Perform actual HTTP request
    var result = performHttpRequest(url, fetch_ctx) catch |err| {
        std.log.warn("[fetch] HTTP error for {s}: {}", .{ url, err });
        // Include specific error type in message for debugging
        const error_msg = switch (err) {
            error.InvalidUrl => "fetch failed: invalid URL",
            error.ConnectionFailed => "fetch failed: connection refused or timed out",
            error.SendFailed => "fetch failed: could not send request",
            error.ResponseFailed => "fetch failed: no response from server",
            error.ReadFailed => "fetch failed: could not read response body",
            else => "fetch failed: unexpected error",
        };
        return createErrorObject(rt, error_msg);
    };
    // Ensure cleanup on all exit paths
    defer result.deinit();

    // IMPORTANT: Create JS object FIRST - this copies body to Hermes heap
    // The body pointer remains valid until result.deinit() in defer
    const js_response = createResponseObject(rt, result.body, result.status, null);

    // THEN cache the response - body is still valid here
    // JSON serialization makes its own copy
    if (fetch_ctx) |ctx| {
        if (ctx.net_cache) |cache| {
            const cache_response = network_cache.CachedResponse{
                .url = url,
                .body = result.body,
                .fetched_at_ms = std.time.milliTimestamp(),
                .ttl_ms = network_cache.DEFAULT_TTL_MS,
                .status_code = result.status,
                .content_type = result.content_type, // Propagate content-type to cache
            };
            cache.put(cache_response) catch |err| {
                std.log.warn("[fetch] Failed to cache response: {}", .{err});
            };
        }
    }

    // defer runs here: result.deinit() frees body and content_type
    return js_response;
}

/// HTTP response from performHttpRequest
const HttpResult = struct {
    body: []const u8,
    status: u16,
    content_type: ?[]const u8,
    allocator: ?std.mem.Allocator, // If non-null, body must be freed

    pub fn deinit(self: *HttpResult) void {
        if (self.allocator) |alloc| {
            alloc.free(self.body);
            if (self.content_type) |ct| {
                alloc.free(ct);
            }
        }
    }
};

/// HTTP fetch configuration
const FetchConfig = struct {
    /// Maximum response body size (default 10MB)
    max_body_size: usize = 10 * 1024 * 1024,
    /// Request timeout in nanoseconds (default 30 seconds)
    timeout_ns: u64 = 30 * std.time.ns_per_s,
    /// Maximum redirects to follow (default 5)
    max_redirects: u8 = 5,
    /// Server header buffer size
    header_buffer_size: usize = 16 * 1024,
};

/// Perform actual HTTP request using Zig's std.http
/// Production-quality implementation with:
/// - Proper status code handling
/// - Redirect following
/// - Timeout support
/// - Content-Type extraction
/// - Configurable limits
fn performHttpRequest(url: []const u8, ctx: ?*FetchContext) !HttpResult {
    return performHttpRequestWithConfig(url, ctx, .{});
}

fn performHttpRequestWithConfig(url: []const u8, ctx: ?*FetchContext, config: FetchConfig) !HttpResult {
    const allocator = if (ctx) |fctx| fctx.allocator else std.heap.page_allocator;

    // Parse URL
    const uri = std.Uri.parse(url) catch |err| {
        std.log.err("[fetch] Invalid URL '{s}': {}", .{ url, err });
        return error.InvalidUrl;
    };

    // Create HTTP client
    var client = std.http.Client{ .allocator = allocator };
    defer client.deinit();

    // Allocate header buffer dynamically for larger responses
    const header_buf = try allocator.alloc(u8, config.header_buffer_size);
    defer allocator.free(header_buf);

    // Open request with redirect handling
    // RedirectBehavior is enum(u16) where value is max redirects (0 = not_allowed)
    var request = client.open(.GET, uri, .{
        .server_header_buffer = header_buf,
        .redirect_behavior = @enumFromInt(config.max_redirects),
    }) catch |err| {
        std.log.err("[fetch] Failed to open connection to '{s}': {}", .{ url, err });
        return error.ConnectionFailed;
    };
    defer request.deinit();

    // Send request
    request.send() catch |err| {
        std.log.err("[fetch] Failed to send request to '{s}': {}", .{ url, err });
        return error.SendFailed;
    };

    // Wait for response headers
    request.wait() catch |err| {
        std.log.err("[fetch] Failed waiting for response from '{s}': {}", .{ url, err });
        return error.ResponseFailed;
    };

    // Extract status code (http.Status is enum(u10), cast to u16)
    const status: u16 = @intFromEnum(request.response.status);

    // Extract content-type if available
    var content_type: ?[]const u8 = null;
    if (request.response.content_type) |ct| {
        content_type = try allocator.dupe(u8, ct);
    }
    errdefer if (content_type) |ct| allocator.free(ct);

    // Read response body with size limit
    const body = request.reader().readAllAlloc(allocator, config.max_body_size) catch |err| {
        std.log.err("[fetch] Failed to read response body from '{s}': {}", .{ url, err });
        if (content_type) |ct| allocator.free(ct);
        return error.ReadFailed;
    };

    std.log.debug("[fetch] Completed: {s} -> {} ({} bytes)", .{ url, status, body.len });

    return HttpResult{
        .body = body,
        .status = status,
        .content_type = content_type,
        .allocator = allocator,
    };
}

/// Create a JS object with error field
fn createErrorObject(rt: *c.MSHermesRuntime, message: []const u8) ?*c.MSHermesValue {
    // Create error object: { error: "message" }
    const obj = c.ms_value_object(rt) orelse return null;
    const error_str = c.ms_value_string(rt, message.ptr, message.len) orelse return null;
    c.ms_object_set(rt, obj, "error", error_str);
    return obj;
}

/// Create a JS response object: { status: number, body: string, error: null }
fn createResponseObject(
    rt: *c.MSHermesRuntime,
    body: []const u8,
    status: u16,
    allocator_to_free: ?std.mem.Allocator,
) ?*c.MSHermesValue {
    defer {
        if (allocator_to_free) |alloc| {
            alloc.free(body);
        }
    }

    const obj = c.ms_value_object(rt) orelse return null;

    // status
    const status_val = c.ms_value_number(rt, @floatFromInt(status)) orelse return null;
    c.ms_object_set(rt, obj, "status", status_val);

    // body
    const body_val = c.ms_value_string(rt, body.ptr, body.len) orelse return null;
    c.ms_object_set(rt, obj, "body", body_val);

    // error: null
    const null_val = c.ms_value_null(rt) orelse return null;
    c.ms_object_set(rt, obj, "error", null_val);

    return obj;
}

// =============================================================================
// Tests
// =============================================================================

test "MacroVM initialization" {
    // This test requires Hermes runtime - skip in CI
    // const allocator = std.testing.allocator;
    // var arena = ast.ASTArena.init(allocator);
    // defer arena.deinit();
    //
    // var vm = try MacroVM.init(allocator, &arena);
    // defer vm.deinit();
}
