const std = @import("std");
const ast = @import("../ast/ast.zig");
const Lexer = @import("../lexer/lexer.zig").Lexer;
const Token = @import("../lexer/token.zig").Token;
const TokenKind = @import("../lexer/token.zig").TokenKind;

/// Parse error with location information
pub const ParseError = struct {
    message: []const u8,
    loc: ast.SourceLocation,
    severity: Severity,

    pub const Severity = enum { @"error", warning };
};

/// Error set for parser operations (explicit to break recursive inference cycles)
pub const Error = error{
    ParseError,
    OutOfMemory,
};

/// Recursive descent parser for TypeScript strict subset + macros
pub const Parser = struct {
    allocator: std.mem.Allocator,
    arena: *ast.ASTArena,
    lexer: *Lexer,
    file_id: ast.FileId,

    // Current and previous tokens
    current: Token,
    previous: Token,

    // Error handling
    errors: std.ArrayList(ParseError),
    panic_mode: bool,

    // JSDoc comment tracking - the most recent doc comment seen before a declaration
    // Consumed when attached to a declaration node, reset to null after use
    pending_doc_comment: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator, arena: *ast.ASTArena, lexer: *Lexer, file_id: ast.FileId) Parser {
        const dummy_loc = ast.SourceLocation.dummy();
        const dummy_token = Token.init(.end_of_file, dummy_loc, "");

        return .{
            .allocator = allocator,
            .arena = arena,
            .lexer = lexer,
            .file_id = file_id,
            .current = dummy_token,
            .previous = dummy_token,
            .errors = std.ArrayList(ParseError).init(allocator),
            .panic_mode = false,
            .pending_doc_comment = null,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    /// Parse source code into AST
    pub fn parse(self: *Parser) !*ast.Node {
        // Prime the parser with first token
        self.advance();

        var statements = std.ArrayList(*ast.Node).init(self.allocator);
        defer statements.deinit();

        while (!self.check(.end_of_file)) {
            if (self.parseTopLevelDeclaration()) |stmt| {
                try statements.append(stmt);
            } else |err| {
                switch (err) {
                    error.ParseError => self.synchronize(),
                    else => return err,
                }
            }
        }

        const stmts_slice = try self.arena.allocator().dupe(*ast.Node, statements.items);

        return try self.arena.createNode(
            .program,
            self.makeLoc(),
            .{
                .program = .{
                    .statements = stmts_slice,
                    .file_id = self.file_id,
                },
            },
        );
    }

    // ===== Top-Level Declarations =====

    fn parseTopLevelDeclaration(self: *Parser) !*ast.Node {
        // IMPORTANT: Capture JSDoc BEFORE parsing decorators
        // JSDoc applies to the declaration, not to the decorators
        // Example: /** docs */ @derive(Eq) class User {}
        const doc_comment = self.consumeDocComment();

        // Check for extern keyword (extern macro @target(...): void;)
        if (self.check(.keyword_extern)) {
            self.advance(); // consume 'extern'
            // Re-attach doc comment for extern declaration to consume
            self.pending_doc_comment = doc_comment;
            return self.parseExternDeclaration();
        }

        // Check for macro keyword: macro function name(params) { ... }
        if (self.check(.keyword_macro)) {
            self.advance(); // consume 'macro'
            // Expect 'function' keyword after 'macro'
            if (!self.check(.keyword_function)) {
                return self.reportError("Expected 'function' after 'macro'");
            }
            self.advance(); // consume 'function'
            // Re-attach doc comment for macro declaration to consume
            self.pending_doc_comment = doc_comment;
            return self.parseMacroDeclaration();
        }

        // Check for @comptime block at top level
        if (self.check(.at_sign) and self.peekNextIsIdentifier("comptime")) {
            return self.parseComptimeBlock();
        }

        // Parse decorators first
        var decorators = std.ArrayList(*ast.Node).init(self.allocator);
        defer decorators.deinit();

        while (self.checkMacro()) {
            const decorator = try self.parseDecorator();
            try decorators.append(decorator);
        }

        // Re-attach doc comment for declaration parsing functions to consume
        self.pending_doc_comment = doc_comment;

        // Parse the declaration
        if (self.check(.keyword_class)) {
            return self.parseClassDeclaration(&decorators);
        } else if (self.check(.keyword_function) or self.check(.keyword_async)) {
            return self.parseFunctionDeclaration(&decorators);
        } else if (self.check(.keyword_interface)) {
            return self.parseInterfaceDeclaration();
        } else if (self.check(.keyword_type)) {
            return self.parseTypeAliasDeclaration();
        } else if (self.check(.keyword_enum)) {
            return self.parseEnumDeclaration();
        } else if (self.check(.keyword_const) or self.check(.keyword_let) or self.check(.keyword_var)) {
            return self.parseVariableDeclaration();
        } else if (self.check(.keyword_import)) {
            return self.parseImportDeclaration();
        } else if (self.check(.keyword_export)) {
            return self.parseExportDeclaration();
        } else {
            // Expression statement
            return self.parseStatement();
        }
    }

    /// Parse a system macro invocation (@target, @emit, @extern, etc.)
    ///
    /// System macros:
    /// - Always require @ prefix
    /// - Always available without import
    /// - Used for: @target("c"), @emit("code"), @extern("fn"), @comptime, @inline
    ///
    /// Context determines behavior:
    /// - as_decorator=true: Used before class/function, no block body allowed
    /// - as_decorator=false: Used as statement, optional block body (@target("c") { ... })
    fn parseSystemMacro(self: *Parser, as_decorator: bool) !*ast.Node {
        const start_loc = self.current.loc;

        // System macros require @ prefix
        if (!self.check(.at_sign)) {
            return self.reportError("System macros require '@' prefix");
        }
        self.advance(); // consume @

        if (!self.check(.identifier)) {
            return self.reportError("Expected macro name after '@'");
        }
        const macro_name = self.current.text;
        self.advance();

        // Parse arguments: @target("c"), @emit("code"), @derive(Eq, Hash)
        var args = std.ArrayList(ast.node.MacroInvocation.MacroArgument).init(self.allocator);
        defer args.deinit();

        if (self.match(.left_paren)) {
            while (!self.check(.right_paren) and !self.check(.end_of_file)) {
                // Parse argument - can be string literal, identifier, or expression
                if (self.check(.string)) {
                    // Strip quotes from string literal: "c" -> c
                    const text = self.current.text;
                    const stripped = if (text.len >= 2 and (text[0] == '"' or text[0] == '\''))
                        text[1 .. text.len - 1]
                    else
                        text;
                    try args.append(.{ .string_literal = stripped });
                    self.advance();
                } else if (self.check(.identifier)) {
                    try args.append(.{ .identifier = self.current.text });
                    self.advance();
                } else {
                    // Skip complex arguments for now
                    self.advance();
                }

                if (!self.match(.comma)) break;
            }
            try self.consume(.right_paren, "Expected ')' after macro arguments");
        }

        // Block body only allowed for statement macros (not decorators)
        var body: ?*ast.Node = null;
        if (!as_decorator) {
            if (self.check(.left_brace)) {
                body = try self.parseBlock();
            } else {
                // Statement ending with semicolon: @emit("code");
                _ = self.match(.semicolon);
            }
        }

        const args_slice = try self.arena.allocator().dupe(ast.node.MacroInvocation.MacroArgument, args.items);

        return try self.arena.createNode(
            .macro_invocation,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .macro_invocation = .{
                    .name = macro_name,
                    .arguments = args_slice,
                    .target = body,
                },
            },
        );
    }

    /// Parse a decorator (system macro before class/function)
    fn parseDecorator(self: *Parser) !*ast.Node {
        return self.parseSystemMacro(true);
    }

    /// Parse a macro statement inside a block (@target("c") { ... }, @emit("code"))
    fn parseMacroStatement(self: *Parser) !*ast.Node {
        return self.parseSystemMacro(false);
    }

    fn parseClassDeclaration(self: *Parser, decorators: *std.ArrayList(*ast.Node)) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;
        try self.consume(.keyword_class, "Expected 'class'");

        // Class name
        if (!self.check(.identifier)) {
            return self.reportError("Expected class name");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters <T, U>
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Extends clause
        var extends: ?*ast.types.Type = null;
        if (self.match(.keyword_extends)) {
            extends = try self.parseTypeReference();
        }

        // Implements clause
        var implements = std.ArrayList(*ast.types.Type).init(self.allocator);
        defer implements.deinit();
        if (self.match(.keyword_implements)) {
            try implements.append(try self.parseTypeReference());
            while (self.match(.comma)) {
                try implements.append(try self.parseTypeReference());
            }
        }

        // Class body
        try self.consume(.left_brace, "Expected '{' before class body");

        var members = std.ArrayList(*ast.Node).init(self.allocator);
        defer members.deinit();

        while (!self.check(.right_brace) and !self.check(.end_of_file)) {
            const member = try self.parseClassMember();
            try members.append(member);
        }

        try self.consume(.right_brace, "Expected '}' after class body");

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const implements_slice = try self.arena.allocator().dupe(*ast.types.Type, implements.items);
        const members_slice = try self.arena.allocator().dupe(*ast.Node, members.items);

        // Convert decorator nodes to Decorator structs
        var class_decorators = std.ArrayList(ast.node.ClassDecl.Decorator).init(self.allocator);
        defer class_decorators.deinit();

        for (decorators.items) |dec_node| {
            if (dec_node.kind == .macro_invocation) {
                const macro = &dec_node.data.macro_invocation;

                // Convert MacroArgument to Node references
                var arg_nodes = std.ArrayList(*ast.Node).init(self.allocator);
                defer arg_nodes.deinit();

                for (macro.arguments) |arg| {
                    switch (arg) {
                        .identifier => |id_name| {
                            const id_node = try self.arena.createNode(
                                .identifier,
                                dec_node.location,
                                .{ .identifier = id_name },
                            );
                            try arg_nodes.append(id_node);
                        },
                        .expression => |expr| {
                            try arg_nodes.append(expr);
                        },
                        else => {},
                    }
                }

                try class_decorators.append(.{
                    .name = macro.name,
                    .arguments = try self.arena.allocator().dupe(*ast.Node, arg_nodes.items),
                });
            }
        }

        const decorators_slice = try self.arena.allocator().dupe(ast.node.ClassDecl.Decorator, class_decorators.items);

        const node = try self.arena.createNode(
            .class_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .class_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .extends = extends,
                    .implements = implements_slice,
                    .members = members_slice,
                    .decorators = decorators_slice,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseClassMember(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (for property/method/constructor)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;

        // Check for modifiers
        var is_static = false;
        var is_readonly = false;
        var visibility: ?[]const u8 = null;

        while (true) {
            if (self.match(.keyword_public)) {
                visibility = "public";
            } else if (self.match(.keyword_private)) {
                visibility = "private";
            } else if (self.match(.keyword_protected)) {
                visibility = "protected";
            } else if (self.match(.keyword_static)) {
                is_static = true;
            } else if (self.match(.keyword_readonly)) {
                is_readonly = true;
            } else {
                break;
            }
        }

        // TODO: Use visibility and is_static in property/method nodes
        _ = .{ visibility, is_static };

        // Constructor
        if (self.match(.keyword_constructor)) {
            return self.parseConstructor(start_loc, doc_comment);
        }

        // Method or property
        if (!self.check(.identifier)) {
            return self.reportError("Expected member name");
        }
        const name = self.current.text;
        self.advance();

        // Method: name(...) { ... }
        if (self.check(.left_paren) or self.check(.less_than)) {
            return self.parseMethod(start_loc, name, doc_comment);
        }

        // Property: name: Type = value;
        return self.parseProperty(start_loc, name, is_readonly, doc_comment);
    }

    fn parseConstructor(self: *Parser, start_loc: ast.SourceLocation, doc_comment: ?[]const u8) !*ast.Node {
        // Parameters
        try self.consume(.left_paren, "Expected '(' after 'constructor'");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')' after parameters");

        // Body
        const body = try self.parseBlock();

        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        const node = try self.arena.createNode(
            .constructor_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .constructor_decl = .{
                    .params = params_slice,
                    .body = body,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseMethod(self: *Parser, start_loc: ast.SourceLocation, name: []const u8, doc_comment: ?[]const u8) !*ast.Node {
        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Parameters
        try self.consume(.left_paren, "Expected '('");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')'");

        // Return type
        var return_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            return_type = try self.parseTypeReference();
        }

        // Body (optional for interface methods)
        var body: ?*ast.Node = null;
        if (self.check(.left_brace)) {
            body = try self.parseBlock();
        } else {
            _ = self.match(.semicolon);
        }

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        const node = try self.arena.createNode(
            .method_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .method_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .params = params_slice,
                    .return_type = return_type,
                    .body = body,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseProperty(self: *Parser, start_loc: ast.SourceLocation, name: []const u8, readonly: bool, doc_comment: ?[]const u8) !*ast.Node {
        // Type annotation
        var prop_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            prop_type = try self.parseTypeReference();
        }

        // Initializer
        var prop_init: ?*ast.Node = null;
        if (self.match(.equals)) {
            prop_init = try self.parseExpression();
        }

        _ = self.match(.semicolon);

        const node = try self.arena.createNode(
            .property_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .property_decl = .{
                    .name = name,
                    .type = prop_type,
                    .init = prop_init,
                    .readonly = readonly,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseFunctionDeclaration(self: *Parser, decorators: *std.ArrayList(*ast.Node)) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;
        _ = decorators;

        // async function?
        const is_async = self.match(.keyword_async);
        _ = is_async;

        try self.consume(.keyword_function, "Expected 'function'");

        // Function name
        if (!self.check(.identifier)) {
            return self.reportError("Expected function name");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Parameters
        try self.consume(.left_paren, "Expected '(' after function name");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')' after parameters");

        // Return type
        var return_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            return_type = try self.parseTypeReference();
        }

        // Body
        var body: ?*ast.Node = null;
        if (self.check(.left_brace)) {
            body = try self.parseBlock();
        }

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        const node = try self.arena.createNode(
            .function_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .function_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .params = params_slice,
                    .return_type = return_type,
                    .body = body,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    /// Parse macro declaration
    /// Syntax: macro function name(params) { ... }
    /// Usage: name(args) - called like normal function
    fn parseMacroDeclaration(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;

        // Macro name
        if (!self.check(.identifier)) {
            return self.reportError("Expected macro name");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Parameters (e.g., ctx: MacroContext)
        try self.consume(.left_paren, "Expected '(' after macro name");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')' after parameters");

        // Return type
        var return_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            return_type = try self.parseTypeReference();
        }

        // Body is required for macros
        if (!self.check(.left_brace)) {
            return self.reportError("Macro must have a body");
        }
        const body = try self.parseBlock();

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        const node = try self.arena.createNode(
            .macro_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .macro_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .params = params_slice,
                    .return_type = return_type,
                    .body = body,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    /// Parse extern declaration (extern macro @target(...): void;)
    fn parseExternDeclaration(self: *Parser) !*ast.Node {
        const doc_comment = self.consumeDocComment();
        const start_loc = self.previous.loc; // 'extern' was already consumed

        // Currently only 'extern macro' is supported
        if (!self.check(.keyword_macro)) {
            return self.reportError("Expected 'macro' after 'extern'");
        }
        self.advance(); // consume 'macro'

        // Macro name must start with @
        if (!self.check(.at_sign)) {
            return self.reportError("Extern macro name must start with '@'");
        }
        self.advance(); // consume @

        // Get the identifier
        if (!self.check(.identifier)) {
            return self.reportError("Expected macro name after '@'");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters (e.g., <T>)
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Parameters
        try self.consume(.left_paren, "Expected '(' after macro name");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')' after parameters");

        // Return type
        var return_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            return_type = try self.parseTypeReference();
        }

        // Extern macros must end with semicolon (no body)
        try self.consume(.semicolon, "Expected ';' after extern macro declaration");

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        const node = try self.arena.createNode(
            .extern_macro_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .extern_macro_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .params = params_slice,
                    .return_type = return_type,
                },
            },
        );

        node.doc_comment = doc_comment;
        return node;
    }

    fn parseInterfaceDeclaration(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;
        try self.consume(.keyword_interface, "Expected 'interface'");

        // Interface name
        if (!self.check(.identifier)) {
            return self.reportError("Expected interface name");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Extends clause
        var extends = std.ArrayList(*ast.types.Type).init(self.allocator);
        defer extends.deinit();
        if (self.match(.keyword_extends)) {
            try extends.append(try self.parseTypeReference());
            while (self.match(.comma)) {
                try extends.append(try self.parseTypeReference());
            }
        }

        // Body
        try self.consume(.left_brace, "Expected '{' before interface body");
        var members = std.ArrayList(*ast.Node).init(self.allocator);
        defer members.deinit();

        while (!self.check(.right_brace) and !self.check(.end_of_file)) {
            const member = try self.parseInterfaceMember();
            try members.append(member);
        }

        try self.consume(.right_brace, "Expected '}' after interface body");

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const extends_slice = try self.arena.allocator().dupe(*ast.types.Type, extends.items);
        const members_slice = try self.arena.allocator().dupe(*ast.Node, members.items);

        const node = try self.arena.createNode(
            .interface_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .interface_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .extends = extends_slice,
                    .members = members_slice,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseInterfaceMember(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (for interface member)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;

        // Method signature or property
        if (!self.check(.identifier)) {
            return self.reportError("Expected member name");
        }
        const name = self.current.text;
        self.advance();

        // Optional marker
        const optional = self.match(.question);
        _ = optional;

        // Method or property?
        if (self.check(.left_paren) or self.check(.less_than)) {
            return self.parseMethod(start_loc, name, doc_comment);
        }

        // Property signature
        try self.consume(.colon, "Expected ':' after property name");
        const prop_type = try self.parseTypeReference();
        _ = self.match(.semicolon);

        const node = try self.arena.createNode(
            .property_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .property_decl = .{
                    .name = name,
                    .type = prop_type,
                    .init = null,
                    .readonly = false,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseTypeAliasDeclaration(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;
        try self.consume(.keyword_type, "Expected 'type'");

        if (!self.check(.identifier)) {
            return self.reportError("Expected type name");
        }
        const name = self.current.text;
        self.advance();

        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        try self.consume(.equals, "Expected '=' in type alias");
        const aliased_type = try self.parseTypeReference();
        _ = self.match(.semicolon);

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);

        const node = try self.arena.createNode(
            .type_alias_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .type_alias_decl = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .type = aliased_type,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseEnumDeclaration(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();
        _ = doc_comment; // TODO: Implement enum nodes with doc_comment support

        const start_loc = self.current.loc;
        try self.consume(.keyword_enum, "Expected 'enum'");

        if (!self.check(.identifier)) {
            return self.reportError("Expected enum name");
        }
        const name = self.current.text;
        self.advance();

        // For now, parse enum body as a simple block and return as class_decl
        // TODO: Add proper enum node type
        try self.consume(.left_brace, "Expected '{' before enum body");

        while (!self.check(.right_brace) and !self.check(.end_of_file)) {
            // Skip enum members for now
            self.advance();
            _ = self.match(.comma);
        }

        try self.consume(.right_brace, "Expected '}' after enum body");

        // Return as empty class for now
        const empty_slice: []*ast.Node = try self.arena.allocator().alloc(*ast.Node, 0);
        const empty_type_params: []ast.types.GenericParam = try self.arena.allocator().alloc(ast.types.GenericParam, 0);
        const empty_implements: []*ast.types.Type = try self.arena.allocator().alloc(*ast.types.Type, 0);

        return try self.arena.createNode(
            .class_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .class_decl = .{
                    .name = name,
                    .type_params = empty_type_params,
                    .extends = null,
                    .implements = empty_implements,
                    .members = empty_slice,
                },
            },
        );
    }

    fn parseVariableDeclaration(self: *Parser) !*ast.Node {
        // Capture JSDoc comment before parsing (must be done first)
        const doc_comment = self.consumeDocComment();

        const start_loc = self.current.loc;

        const kind: ast.node.VariableStmt.VariableKind = if (self.match(.keyword_const))
            .@"const"
        else if (self.match(.keyword_let))
            .let
        else if (self.match(.keyword_var))
            .@"var"
        else
            return self.reportError("Expected 'const', 'let', or 'var'");

        var declarations = std.ArrayList(ast.node.VariableStmt.VariableDeclarator).init(self.allocator);
        defer declarations.deinit();

        // Parse first declarator
        try declarations.append(try self.parseVariableDeclarator());

        // Parse additional declarators
        while (self.match(.comma)) {
            try declarations.append(try self.parseVariableDeclarator());
        }

        _ = self.match(.semicolon);

        const decls_slice = try self.arena.allocator().dupe(ast.node.VariableStmt.VariableDeclarator, declarations.items);

        const node = try self.arena.createNode(
            .variable_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .variable_stmt = .{
                    .kind = kind,
                    .declarations = decls_slice,
                },
            },
        );

        // Attach JSDoc comment to the node
        node.doc_comment = doc_comment;

        return node;
    }

    fn parseVariableDeclarator(self: *Parser) !ast.node.VariableStmt.VariableDeclarator {
        if (!self.check(.identifier)) {
            return self.reportError("Expected variable name");
        }
        const name = self.current.text;
        self.advance();

        // Type annotation
        var var_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            var_type = try self.parseTypeReference();
        }

        // Initializer
        var var_init: ?*ast.Node = null;
        if (self.match(.equals)) {
            var_init = try self.parseExpression();
        }

        return .{
            .name = name,
            .type = var_type,
            .init = var_init,
        };
    }

    fn parseImportDeclaration(self: *Parser) !*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_import, "Expected 'import'");

        var specifiers = std.ArrayList(ast.node.ImportDecl.ImportSpecifier).init(self.allocator);
        defer specifiers.deinit();

        // import { x, y } from "mod"
        // import x from "mod"
        // import * as x from "mod"

        if (self.match(.left_brace)) {
            // Named imports
            while (!self.check(.right_brace) and !self.check(.end_of_file)) {
                if (!self.check(.identifier)) {
                    return self.reportError("Expected import name");
                }
                const imported = self.current.text;
                self.advance();

                var local = imported;
                if (self.match(.keyword_as)) {
                    if (!self.check(.identifier)) {
                        return self.reportError("Expected local name after 'as'");
                    }
                    local = self.current.text;
                    self.advance();
                }

                try specifiers.append(.{ .imported = imported, .local = local });

                if (!self.match(.comma)) break;
            }
            try self.consume(.right_brace, "Expected '}' after import specifiers");
        } else if (self.check(.identifier)) {
            // Default import
            const name = self.current.text;
            self.advance();
            try specifiers.append(.{ .imported = "default", .local = name });
        }

        try self.consume(.keyword_from, "Expected 'from' after import specifiers");

        if (!self.check(.string)) {
            return self.reportError("Expected module path string");
        }
        const source = self.current.text;
        self.advance();

        _ = self.match(.semicolon);

        const specs_slice = try self.arena.allocator().dupe(ast.node.ImportDecl.ImportSpecifier, specifiers.items);

        return try self.arena.createNode(
            .import_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .import_decl = .{
                    .specifiers = specs_slice,
                    .source = source,
                },
            },
        );
    }

    fn parseExportDeclaration(self: *Parser) !*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_export, "Expected 'export'");

        var declaration: ?*ast.Node = null;
        var specifiers = std.ArrayList(ast.node.ExportDecl.ExportSpecifier).init(self.allocator);
        defer specifiers.deinit();

        // export default ...
        // export { x, y }
        // export const/function/class ...

        if (self.match(.keyword_default)) {
            // export default expr
            declaration = try self.parseExpression();
            _ = self.match(.semicolon);
        } else if (self.match(.left_brace)) {
            // Named exports: export { x, y } or export { x, y } from "module"
            while (!self.check(.right_brace) and !self.check(.end_of_file)) {
                if (!self.check(.identifier)) {
                    return self.reportError("Expected export name");
                }
                const local = self.current.text;
                self.advance();

                var exported = local;
                if (self.match(.keyword_as)) {
                    if (!self.check(.identifier)) {
                        return self.reportError("Expected exported name after 'as'");
                    }
                    exported = self.current.text;
                    self.advance();
                }

                try specifiers.append(.{ .local = local, .exported = exported });

                if (!self.match(.comma)) break;
            }
            try self.consume(.right_brace, "Expected '}' after export specifiers");

            // Check for re-export: export { x } from "module"
            var source: ?[]const u8 = null;
            if (self.match(.keyword_from)) {
                if (!self.check(.string)) {
                    return self.reportError("Expected module path after 'from'");
                }
                source = self.current.text;
                self.advance();
            }

            _ = self.match(.semicolon);

            const specs_slice = try self.arena.allocator().dupe(ast.node.ExportDecl.ExportSpecifier, specifiers.items);

            return try self.arena.createNode(
                .export_decl,
                self.mergeLoc(start_loc, self.previous.loc),
                .{
                    .export_decl = .{
                        .declaration = null,
                        .specifiers = specs_slice,
                        .source = source,
                    },
                },
            );
        } else {
            // export declaration
            var empty_decorators = std.ArrayList(*ast.Node).init(self.allocator);
            defer empty_decorators.deinit();

            if (self.check(.keyword_const) or self.check(.keyword_let) or self.check(.keyword_var)) {
                declaration = try self.parseVariableDeclaration();
            } else if (self.check(.keyword_function)) {
                declaration = try self.parseFunctionDeclaration(&empty_decorators);
            } else if (self.check(.keyword_class)) {
                declaration = try self.parseClassDeclaration(&empty_decorators);
            } else if (self.check(.keyword_interface)) {
                declaration = try self.parseInterfaceDeclaration();
            } else if (self.check(.keyword_type)) {
                declaration = try self.parseTypeAliasDeclaration();
            } else if (self.check(.keyword_enum)) {
                declaration = try self.parseEnumDeclaration();
            } else if (self.check(.keyword_macro)) {
                self.advance(); // consume 'macro'
                // Expect 'function' keyword after 'macro'
                if (!self.check(.keyword_function)) {
                    return self.reportError("Expected 'function' after 'macro'");
                }
                self.advance(); // consume 'function'
                declaration = try self.parseMacroDeclaration();
            } else {
                return self.reportError("Expected declaration after 'export'");
            }
        }

        const specs_slice = try self.arena.allocator().dupe(ast.node.ExportDecl.ExportSpecifier, specifiers.items);

        return try self.arena.createNode(
            .export_decl,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .export_decl = .{
                    .declaration = declaration,
                    .specifiers = specs_slice,
                    .source = null,
                },
            },
        );
    }

    fn parseComptimeBlock(self: *Parser) !*ast.Node {
        const start_loc = self.current.loc;

        // @comptime is now @ + identifier("comptime")
        try self.consume(.at_sign, "Expected '@'");
        if (!self.check(.identifier) or !std.mem.eql(u8, self.current.text, "comptime")) {
            return self.reportError("Expected 'comptime' after '@'");
        }
        self.advance(); // consume "comptime"

        const body = try self.parseBlock();

        return try self.arena.createNode(
            .comptime_block,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .comptime_block = .{
                    .body = body,
                },
            },
        );
    }

    /// Parse quote expression: quote { ... }
    /// Used inside macros to generate AST from code templates
    ///
    /// CURRENT LIMITATION: Interpolation (${...}) is NOT YET IMPLEMENTED.
    /// The quote block is parsed as a regular block statement.
    /// To use dynamic values in quoted code, you must use the AST API directly:
    ///
    ///   // Instead of: quote { return ${expr}; }
    ///   // Use: ast.createReturnStmt(expr)
    ///
    /// TODO: Implement interpolation scanning:
    /// 1. Scan block for ${...} patterns during parsing
    /// 2. Replace with placeholder identifiers (__ms_interp_0, etc.)
    /// 3. Record mapping in interpolations array
    /// 4. Macro expander substitutes placeholders with actual expressions
    ///
    fn parseQuoteExpr(self: *Parser) !*ast.Node {
        const start_loc = self.current.loc;

        // 'quote' keyword already consumed by caller
        const body = try self.parseBlock();

        // FIXME: Interpolation not implemented - always empty
        // When implemented, this should scan body for ${...} patterns
        const empty_interpolations = try self.arena.allocator().alloc(ast.QuoteExpr.Interpolation, 0);

        return try self.arena.createNode(
            .quote_expr,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .quote_expr = .{
                    .body = body,
                    .interpolations = empty_interpolations,
                },
            },
        );
    }

    // ===== Statements =====
    // Note: These functions also use explicit Error type for recursive call chains

    fn parseStatement(self: *Parser) Error!*ast.Node {
        if (self.check(.left_brace)) {
            return self.parseBlock();
        } else if (self.check(.keyword_if)) {
            return self.parseIfStatement();
        } else if (self.check(.keyword_while)) {
            return self.parseWhileStatement();
        } else if (self.check(.keyword_for)) {
            return self.parseForStatement();
        } else if (self.check(.keyword_return)) {
            return self.parseReturnStatement();
        } else if (self.check(.keyword_break)) {
            return self.parseBreakStatement();
        } else if (self.check(.keyword_continue)) {
            return self.parseContinueStatement();
        } else if (self.check(.keyword_throw)) {
            return self.parseThrowStatement();
        } else if (self.check(.keyword_try)) {
            return self.parseTryStatement();
        } else {
            return self.parseExpressionStatement();
        }
    }

    fn parseBlock(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.left_brace, "Expected '{'");

        var statements = std.ArrayList(*ast.Node).init(self.allocator);
        defer statements.deinit();

        while (!self.check(.right_brace) and !self.check(.end_of_file)) {
            if (self.check(.keyword_const) or self.check(.keyword_let) or self.check(.keyword_var)) {
                try statements.append(try self.parseVariableDeclaration());
            } else if (self.checkMacro()) {
                // Handle macro invocations like target("c") { ... } or emit("code")
                try statements.append(try self.parseMacroStatement());
            } else {
                try statements.append(try self.parseStatement());
            }
        }

        try self.consume(.right_brace, "Expected '}'");

        const stmts_slice = try self.arena.allocator().dupe(*ast.Node, statements.items);

        return try self.arena.createNode(
            .block_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .block_stmt = .{
                    .statements = stmts_slice,
                },
            },
        );
    }

    fn parseIfStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_if, "Expected 'if'");
        try self.consume(.left_paren, "Expected '(' after 'if'");
        const condition = try self.parseExpression();
        try self.consume(.right_paren, "Expected ')' after condition");

        const consequent = try self.parseStatement();

        var alternate: ?*ast.Node = null;
        if (self.match(.keyword_else)) {
            alternate = try self.parseStatement();
        }

        return try self.arena.createNode(
            .if_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .if_stmt = .{
                    .condition = condition,
                    .consequent = consequent,
                    .alternate = alternate,
                },
            },
        );
    }

    fn parseWhileStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_while, "Expected 'while'");
        try self.consume(.left_paren, "Expected '(' after 'while'");
        const condition = try self.parseExpression();
        try self.consume(.right_paren, "Expected ')' after condition");

        const body = try self.parseStatement();

        return try self.arena.createNode(
            .while_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .while_stmt = .{
                    .condition = condition,
                    .body = body,
                },
            },
        );
    }

    fn parseForStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_for, "Expected 'for'");
        try self.consume(.left_paren, "Expected '(' after 'for'");

        // Init
        var for_init: ?*ast.Node = null;
        if (!self.check(.semicolon)) {
            if (self.check(.keyword_const) or self.check(.keyword_let) or self.check(.keyword_var)) {
                for_init = try self.parseVariableDeclaration();
            } else {
                for_init = try self.parseExpression();
                try self.consume(.semicolon, "Expected ';'");
            }
        } else {
            self.advance();
        }

        // Condition
        var condition: ?*ast.Node = null;
        if (!self.check(.semicolon)) {
            condition = try self.parseExpression();
        }
        try self.consume(.semicolon, "Expected ';'");

        // Update
        var update: ?*ast.Node = null;
        if (!self.check(.right_paren)) {
            update = try self.parseExpression();
        }
        try self.consume(.right_paren, "Expected ')' after for clauses");

        const body = try self.parseStatement();

        return try self.arena.createNode(
            .for_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .for_stmt = .{
                    .init = for_init,
                    .condition = condition,
                    .update = update,
                    .body = body,
                },
            },
        );
    }

    fn parseReturnStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_return, "Expected 'return'");

        var argument: ?*ast.Node = null;
        if (!self.check(.semicolon) and !self.check(.right_brace)) {
            argument = try self.parseExpression();
        }

        _ = self.match(.semicolon);

        return try self.arena.createNode(
            .return_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .return_stmt = .{
                    .argument = argument,
                },
            },
        );
    }

    fn parseBreakStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_break, "Expected 'break'");
        _ = self.match(.semicolon);

        return try self.arena.createNode(
            .break_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{ .break_stmt = {} },
        );
    }

    fn parseContinueStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_continue, "Expected 'continue'");
        _ = self.match(.semicolon);

        return try self.arena.createNode(
            .continue_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{ .continue_stmt = {} },
        );
    }

    fn parseThrowStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_throw, "Expected 'throw'");
        const argument = try self.parseExpression();
        _ = self.match(.semicolon);

        // Return as expression statement for now
        return try self.arena.createNode(
            .expression_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{ .expression_stmt = argument },
        );
    }

    fn parseTryStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        try self.consume(.keyword_try, "Expected 'try'");

        const try_block = try self.parseBlock();

        // Parse catch if present
        if (self.match(.keyword_catch)) {
            if (self.match(.left_paren)) {
                // Catch parameter
                _ = self.match(.identifier);
                _ = self.match(.colon);
                if (self.check(.identifier)) self.advance();
                try self.consume(.right_paren, "Expected ')'");
            }
            _ = try self.parseBlock();
        }

        // Parse finally if present
        if (self.match(.keyword_finally)) {
            _ = try self.parseBlock();
        }

        // Return try block for now (simplified)
        return try self.arena.createNode(
            .block_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{ .block_stmt = try_block.data.block_stmt },
        );
    }

    fn parseExpressionStatement(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        const expr = try self.parseExpression();
        _ = self.match(.semicolon);

        return try self.arena.createNode(
            .expression_stmt,
            self.mergeLoc(start_loc, self.previous.loc),
            .{ .expression_stmt = expr },
        );
    }

    // ===== Expressions (Pratt Parser with Precedence) =====
    // Note: These functions use explicit Error type to break recursive inference cycle

    fn parseExpression(self: *Parser) Error!*ast.Node {
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) Error!*ast.Node {
        var expr = try self.parseNullishCoalesce();

        if (self.match(.equals) or self.match(.plus_equals) or self.match(.minus_equals) or
            self.match(.star_equals) or self.match(.slash_equals))
        {
            const value = try self.parseAssignment();
            // Create a binary expression for assignment
            expr = try self.arena.createNode(
                .binary_expr,
                self.mergeLoc(expr.location, value.location),
                .{
                    .binary_expr = .{
                        .op = .assign,
                        .left = expr,
                        .right = value,
                    },
                },
            );
        }

        return expr;
    }

    fn parseNullishCoalesce(self: *Parser) Error!*ast.Node {
        var expr = try self.parseLogicalOr();

        while (self.match(.question_question)) {
            const right = try self.parseLogicalOr();
            expr = try self.arena.createNode(
                .binary_expr,
                self.mergeLoc(expr.location, right.location),
                .{
                    .binary_expr = .{
                        .op = .nullish_coalesce,
                        .left = expr,
                        .right = right,
                    },
                },
            );
        }

        return expr;
    }

    fn parseLogicalOr(self: *Parser) Error!*ast.Node {
        var expr = try self.parseLogicalAnd();

        while (self.match(.pipe_pipe)) {
            const right = try self.parseLogicalAnd();
            expr = try self.arena.createNode(
                .binary_expr,
                self.mergeLoc(expr.location, right.location),
                .{
                    .binary_expr = .{
                        .op = .@"or",
                        .left = expr,
                        .right = right,
                    },
                },
            );
        }

        return expr;
    }

    fn parseLogicalAnd(self: *Parser) Error!*ast.Node {
        var expr = try self.parseEquality();

        while (self.match(.ampersand_ampersand)) {
            const right = try self.parseEquality();
            expr = try self.arena.createNode(
                .binary_expr,
                self.mergeLoc(expr.location, right.location),
                .{
                    .binary_expr = .{
                        .op = .@"and",
                        .left = expr,
                        .right = right,
                    },
                },
            );
        }

        return expr;
    }

    fn parseEquality(self: *Parser) Error!*ast.Node {
        var expr = try self.parseComparison();

        while (true) {
            const op: ?ast.node.BinaryOp = if (self.match(.equals_equals_equals))
                .eq
            else if (self.match(.bang_equals_equals))
                .ne
            else if (self.match(.equals_equals))
                .eq
            else if (self.match(.bang_equals))
                .ne
            else
                null;

            if (op) |binary_op| {
                const right = try self.parseComparison();
                expr = try self.arena.createNode(
                    .binary_expr,
                    self.mergeLoc(expr.location, right.location),
                    .{
                        .binary_expr = .{
                            .op = binary_op,
                            .left = expr,
                            .right = right,
                        },
                    },
                );
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseComparison(self: *Parser) Error!*ast.Node {
        var expr = try self.parseAdditive();

        while (true) {
            const op: ?ast.node.BinaryOp = if (self.match(.less_than))
                .lt
            else if (self.match(.less_equals))
                .le
            else if (self.match(.greater_than))
                .gt
            else if (self.match(.greater_equals))
                .ge
            else
                null;

            if (op) |binary_op| {
                const right = try self.parseAdditive();
                expr = try self.arena.createNode(
                    .binary_expr,
                    self.mergeLoc(expr.location, right.location),
                    .{
                        .binary_expr = .{
                            .op = binary_op,
                            .left = expr,
                            .right = right,
                        },
                    },
                );
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseAdditive(self: *Parser) Error!*ast.Node {
        var expr = try self.parseMultiplicative();

        while (true) {
            const op: ?ast.node.BinaryOp = if (self.match(.plus))
                .add
            else if (self.match(.minus))
                .sub
            else
                null;

            if (op) |binary_op| {
                const right = try self.parseMultiplicative();
                expr = try self.arena.createNode(
                    .binary_expr,
                    self.mergeLoc(expr.location, right.location),
                    .{
                        .binary_expr = .{
                            .op = binary_op,
                            .left = expr,
                            .right = right,
                        },
                    },
                );
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseMultiplicative(self: *Parser) Error!*ast.Node {
        var expr = try self.parseUnary();

        while (true) {
            const op: ?ast.node.BinaryOp = if (self.match(.star))
                .mul
            else if (self.match(.slash))
                .div
            else if (self.match(.percent))
                .mod
            else
                null;

            if (op) |binary_op| {
                const right = try self.parseUnary();
                expr = try self.arena.createNode(
                    .binary_expr,
                    self.mergeLoc(expr.location, right.location),
                    .{
                        .binary_expr = .{
                            .op = binary_op,
                            .left = expr,
                            .right = right,
                        },
                    },
                );
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseUnary(self: *Parser) Error!*ast.Node {
        if (self.match(.bang)) {
            const start_loc = self.previous.loc;
            const argument = try self.parseUnary();
            return try self.arena.createNode(
                .unary_expr,
                self.mergeLoc(start_loc, argument.location),
                .{
                    .unary_expr = .{
                        .op = .not,
                        .argument = argument,
                    },
                },
            );
        }

        if (self.match(.minus)) {
            const start_loc = self.previous.loc;
            const argument = try self.parseUnary();
            return try self.arena.createNode(
                .unary_expr,
                self.mergeLoc(start_loc, argument.location),
                .{
                    .unary_expr = .{
                        .op = .neg,
                        .argument = argument,
                    },
                },
            );
        }

        if (self.match(.keyword_typeof)) {
            const start_loc = self.previous.loc;
            const argument = try self.parseUnary();
            return try self.arena.createNode(
                .unary_expr,
                self.mergeLoc(start_loc, argument.location),
                .{
                    .unary_expr = .{
                        .op = .typeof,
                        .argument = argument,
                    },
                },
            );
        }

        // move expr - ownership transfer (DRC optimization)
        // RESTRICTED: move only works with identifiers (variables)
        // This prevents confusing semantics like `move obj.field` or `move getUser()`
        if (self.match(.keyword_move)) {
            const start_loc = self.previous.loc;

            // Must be followed by an identifier
            if (self.current.kind != .identifier) {
                return self.reportError("'move' can only be applied to a variable name");
            }

            const operand = try self.parsePrimary(); // Parse just the identifier
            return try self.arena.createNode(
                .move_expr,
                self.mergeLoc(start_loc, operand.location),
                .{
                    .move_expr = .{
                        .operand = operand,
                    },
                },
            );
        }

        return self.parsePostfix();
    }

    fn parsePostfix(self: *Parser) Error!*ast.Node {
        var expr = try self.parsePrimary();

        while (true) {
            if (self.match(.left_paren)) {
                // Function call
                expr = try self.parseCallExpression(expr);
            } else if (self.match(.dot)) {
                // Member access
                if (!self.check(.identifier)) {
                    return self.reportError("Expected property name after '.'");
                }
                const property = try self.arena.createNode(
                    .identifier,
                    self.current.loc,
                    .{ .identifier = self.current.text },
                );
                self.advance();

                expr = try self.arena.createNode(
                    .member_expr,
                    self.mergeLoc(expr.location, property.location),
                    .{
                        .member_expr = .{
                            .object = expr,
                            .property = property,
                            .computed = false,
                        },
                    },
                );
            } else if (self.match(.left_bracket)) {
                // Computed member access
                const index = try self.parseExpression();
                try self.consume(.right_bracket, "Expected ']'");

                expr = try self.arena.createNode(
                    .member_expr,
                    self.mergeLoc(expr.location, self.previous.loc),
                    .{
                        .member_expr = .{
                            .object = expr,
                            .property = index,
                            .computed = true,
                        },
                    },
                );
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseCallExpression(self: *Parser, callee: *ast.Node) Error!*ast.Node {
        var arguments = std.ArrayList(*ast.Node).init(self.allocator);
        defer arguments.deinit();

        if (!self.check(.right_paren)) {
            try arguments.append(try self.parseExpression());
            while (self.match(.comma)) {
                try arguments.append(try self.parseExpression());
            }
        }

        try self.consume(.right_paren, "Expected ')' after arguments");

        const args_slice = try self.arena.allocator().dupe(*ast.Node, arguments.items);
        const empty_type_args: []*ast.types.Type = try self.arena.allocator().alloc(*ast.types.Type, 0);

        return try self.arena.createNode(
            .call_expr,
            self.mergeLoc(callee.location, self.previous.loc),
            .{
                .call_expr = .{
                    .callee = callee,
                    .arguments = args_slice,
                    .type_args = empty_type_args,
                },
            },
        );
    }

    fn parsePrimary(self: *Parser) Error!*ast.Node {
        const loc = self.current.loc;

        // Number literal
        if (self.match(.number)) {
            const value = std.fmt.parseFloat(f64, self.previous.text) catch 0.0;
            return try self.arena.createNode(
                .number_literal,
                loc,
                .{ .number_literal = value },
            );
        }

        // String literal
        if (self.match(.string)) {
            // Remove quotes from string
            const text = self.previous.text;
            const content = if (text.len >= 2) text[1 .. text.len - 1] else text;
            return try self.arena.createNode(
                .string_literal,
                loc,
                .{ .string_literal = content },
            );
        }

        // Template string literal (treat as string for now)
        // TODO: Parse interpolations ${...} properly
        if (self.match(.template_string)) {
            const text = self.previous.text;
            // Remove backticks
            const content = if (text.len >= 2) text[1 .. text.len - 1] else text;
            return try self.arena.createNode(
                .string_literal,
                loc,
                .{ .string_literal = content },
            );
        }

        // Boolean literals
        if (self.match(.keyword_true)) {
            return try self.arena.createNode(
                .boolean_literal,
                loc,
                .{ .boolean_literal = true },
            );
        }
        if (self.match(.keyword_false)) {
            return try self.arena.createNode(
                .boolean_literal,
                loc,
                .{ .boolean_literal = false },
            );
        }

        // null
        if (self.match(.keyword_null)) {
            return try self.arena.createNode(
                .null_literal,
                loc,
                .{ .null_literal = {} },
            );
        }

        // this
        if (self.match(.keyword_this)) {
            return try self.arena.createNode(
                .identifier,
                loc,
                .{ .identifier = "this" },
            );
        }

        // super
        if (self.match(.keyword_super)) {
            return try self.arena.createNode(
                .identifier,
                loc,
                .{ .identifier = "super" },
            );
        }

        // Identifier
        if (self.match(.identifier)) {
            return try self.arena.createNode(
                .identifier,
                loc,
                .{ .identifier = self.previous.text },
            );
        }

        // Parenthesized expression or arrow function
        if (self.match(.left_paren)) {
            const expr = try self.parseExpression();
            try self.consume(.right_paren, "Expected ')' after expression");
            return expr;
        }

        // Array literal
        if (self.match(.left_bracket)) {
            return self.parseArrayLiteral(loc);
        }

        // Object literal
        if (self.match(.left_brace)) {
            return self.parseObjectLiteral(loc);
        }

        // new expression
        if (self.match(.keyword_new)) {
            return self.parseNewExpression(loc);
        }

        // function expression
        if (self.check(.keyword_function) or self.check(.keyword_async)) {
            return self.parseFunctionExpression();
        }

        // quote expression (used inside macros): quote { ... }
        if (self.match(.keyword_quote)) {
            return self.parseQuoteExpr();
        }

        return self.reportError("Expected expression");
    }

    fn parseArrayLiteral(self: *Parser, start_loc: ast.SourceLocation) Error!*ast.Node {
        var elements = std.ArrayList(*ast.Node).init(self.allocator);
        defer elements.deinit();

        while (!self.check(.right_bracket) and !self.check(.end_of_file)) {
            try elements.append(try self.parseExpression());
            if (!self.match(.comma)) break;
        }

        try self.consume(.right_bracket, "Expected ']' after array elements");

        const elems_slice = try self.arena.allocator().dupe(*ast.Node, elements.items);

        return try self.arena.createNode(
            .array_expr,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .array_expr = .{
                    .elements = elems_slice,
                },
            },
        );
    }

    fn parseObjectLiteral(self: *Parser, start_loc: ast.SourceLocation) Error!*ast.Node {
        var properties = std.ArrayList(ast.node.ObjectExpr.ObjectProperty).init(self.allocator);
        defer properties.deinit();

        while (!self.check(.right_brace) and !self.check(.end_of_file)) {
            // Check for spread: ...expr
            if (self.match(.dot_dot_dot)) {
                const spread_loc = self.previous.loc;
                const argument = try self.parseExpression();
                const spread_node = try self.arena.createNode(
                    .spread_element,
                    self.mergeLoc(spread_loc, self.previous.loc),
                    .{ .spread_element = .{ .argument = argument } },
                );
                try properties.append(.{ .spread = spread_node });
            } else {
                // Regular property
                // Property name
                if (!self.check(.identifier) and !self.check(.string)) {
                    return self.reportError("Expected property name");
                }

                const key = if (self.check(.identifier))
                    try self.arena.createNode(.identifier, self.current.loc, .{ .identifier = self.current.text })
                else
                    try self.arena.createNode(.string_literal, self.current.loc, .{ .string_literal = self.current.text });
                self.advance();

                // Shorthand { x } or full { x: value }
                var value: *ast.Node = undefined;
                var shorthand = false;

                if (self.match(.colon)) {
                    value = try self.parseExpression();
                } else {
                    // Shorthand property
                    value = key;
                    shorthand = true;
                }

                try properties.append(.{
                    .property = .{
                        .key = key,
                        .value = value,
                        .shorthand = shorthand,
                    },
                });
            }

            if (!self.match(.comma)) break;
        }

        try self.consume(.right_brace, "Expected '}' after object properties");

        const props_slice = try self.arena.allocator().dupe(ast.node.ObjectExpr.ObjectProperty, properties.items);

        return try self.arena.createNode(
            .object_expr,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .object_expr = .{
                    .properties = props_slice,
                },
            },
        );
    }

    fn parseNewExpression(self: *Parser, start_loc: ast.SourceLocation) Error!*ast.Node {
        const callee = try self.parsePrimary();

        var arguments = std.ArrayList(*ast.Node).init(self.allocator);
        defer arguments.deinit();

        if (self.match(.left_paren)) {
            while (!self.check(.right_paren) and !self.check(.end_of_file)) {
                try arguments.append(try self.parseExpression());
                if (!self.match(.comma)) break;
            }
            try self.consume(.right_paren, "Expected ')' after arguments");
        }

        const args_slice = try self.arena.allocator().dupe(*ast.Node, arguments.items);
        const empty_type_args: []*ast.types.Type = try self.arena.allocator().alloc(*ast.types.Type, 0);

        return try self.arena.createNode(
            .new_expr,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .new_expr = .{
                    .callee = callee,
                    .arguments = args_slice,
                    .type_args = empty_type_args,
                },
            },
        );
    }

    fn parseFunctionExpression(self: *Parser) Error!*ast.Node {
        const start_loc = self.current.loc;
        const is_async = self.match(.keyword_async);
        _ = is_async;

        try self.consume(.keyword_function, "Expected 'function'");

        // Optional name
        var name: ?[]const u8 = null;
        if (self.check(.identifier)) {
            name = self.current.text;
            self.advance();
        }

        // Type parameters
        var type_params = std.ArrayList(ast.types.GenericParam).init(self.allocator);
        defer type_params.deinit();
        if (self.match(.less_than)) {
            try self.parseTypeParameters(&type_params);
        }

        // Parameters
        try self.consume(.left_paren, "Expected '('");
        var params = std.ArrayList(ast.node.FunctionExpr.FunctionParam).init(self.allocator);
        defer params.deinit();
        try self.parseParameters(&params);
        try self.consume(.right_paren, "Expected ')'");

        // Return type
        var return_type: ?*ast.types.Type = null;
        if (self.match(.colon)) {
            return_type = try self.parseTypeReference();
        }

        // Body
        const body = try self.parseBlock();

        const type_params_slice = try self.arena.allocator().dupe(ast.types.GenericParam, type_params.items);
        const params_slice = try self.arena.allocator().dupe(ast.node.FunctionExpr.FunctionParam, params.items);

        return try self.arena.createNode(
            .function_expr,
            self.mergeLoc(start_loc, self.previous.loc),
            .{
                .function_expr = .{
                    .name = name,
                    .type_params = type_params_slice,
                    .params = params_slice,
                    .return_type = return_type,
                    .body = body,
                    .is_arrow = false,
                },
            },
        );
    }

    // ===== Type Parsing =====

    fn parseTypeReference(self: *Parser) Error!*ast.types.Type {
        const loc = self.current.loc;

        // Parse the base type first, then check for array suffix
        const base_type = try self.parseBaseType(loc);

        // Check for array type suffix: Type[]
        // Can be chained: int32[][] for 2D arrays
        var result_type = base_type;
        while (self.match(.left_bracket)) {
            try self.consume(.right_bracket, "Expected ']' for array type");
            result_type = try self.arena.createType(
                .array,
                loc,
                .{ .array = result_type },
            );
        }

        return result_type;
    }

    /// Parse a base type without array suffix
    /// Uses direct returns to avoid undefined initialization
    fn parseBaseType(self: *Parser, loc: ast.SourceLocation) Error!*ast.types.Type {
        // Simple type name (identifier)
        if (self.check(.identifier)) {
            const name = self.current.text;
            self.advance();

            // Check for generic type args: Type<T, U>
            var type_args = std.ArrayList(*ast.types.Type).init(self.allocator);
            defer type_args.deinit();

            if (self.match(.less_than)) {
                try type_args.append(try self.parseTypeReference());
                while (self.match(.comma)) {
                    try type_args.append(try self.parseTypeReference());
                }
                try self.consume(.greater_than, "Expected '>' after type arguments");
            }

            // Create TypeReference struct
            const type_ref = try self.arena.allocator().create(ast.types.TypeReference);
            type_ref.* = .{
                .name = name,
                .type_args = try self.arena.allocator().dupe(*ast.types.Type, type_args.items),
            };

            return try self.arena.createType(
                .type_reference,
                loc,
                .{ .type_reference = type_ref },
            );
        }

        // Primitive types
        if (self.match(.keyword_void)) {
            return try self.arena.createType(.void, loc, .{ .void = {} });
        }
        if (self.match(.keyword_never)) {
            return try self.arena.createType(.never, loc, .{ .never = {} });
        }
        if (self.match(.keyword_unknown)) {
            return try self.arena.createType(.unknown, loc, .{ .unknown = {} });
        }

        // Sized integer types
        if (self.match(.keyword_int8)) {
            return try self.arena.createType(.int8, loc, .{ .int8 = {} });
        }
        if (self.match(.keyword_int16)) {
            return try self.arena.createType(.int16, loc, .{ .int16 = {} });
        }
        if (self.match(.keyword_int32)) {
            return try self.arena.createType(.int32, loc, .{ .int32 = {} });
        }
        if (self.match(.keyword_int64)) {
            return try self.arena.createType(.int64, loc, .{ .int64 = {} });
        }
        if (self.match(.keyword_uint8)) {
            return try self.arena.createType(.uint8, loc, .{ .uint8 = {} });
        }
        if (self.match(.keyword_uint16)) {
            return try self.arena.createType(.uint16, loc, .{ .uint16 = {} });
        }
        if (self.match(.keyword_uint32)) {
            return try self.arena.createType(.uint32, loc, .{ .uint32 = {} });
        }
        if (self.match(.keyword_uint64)) {
            return try self.arena.createType(.uint64, loc, .{ .uint64 = {} });
        }

        // Sized floating-point types
        if (self.match(.keyword_float32)) {
            return try self.arena.createType(.float32, loc, .{ .float32 = {} });
        }
        if (self.match(.keyword_float64)) {
            return try self.arena.createType(.float64, loc, .{ .float64 = {} });
        }

        // Type aliases (map to base types)
        if (self.match(.keyword_int)) {
            return try self.arena.createType(.int32, loc, .{ .int32 = {} });
        }
        if (self.match(.keyword_float)) {
            return try self.arena.createType(.float32, loc, .{ .float32 = {} });
        }
        if (self.match(.keyword_double)) {
            return try self.arena.createType(.float64, loc, .{ .float64 = {} });
        }

        // Fallback: Return a placeholder type for unknown tokens
        const placeholder_ref = try self.arena.allocator().create(ast.types.TypeReference);
        placeholder_ref.* = .{
            .name = "unknown",
            .type_args = &[_]*ast.types.Type{},
        };
        return try self.arena.createType(.type_reference, loc, .{ .type_reference = placeholder_ref });
    }

    fn parseTypeParameters(self: *Parser, params: *std.ArrayList(ast.types.GenericParam)) !void {
        while (!self.check(.greater_than) and !self.check(.end_of_file)) {
            if (!self.check(.identifier)) {
                return self.reportError("Expected type parameter name");
            }
            const name = self.current.text;
            self.advance();

            // Constraint: T extends U
            var constraint: ?*ast.types.Type = null;
            if (self.match(.keyword_extends)) {
                constraint = try self.parseTypeReference();
            }

            // Default: T = U
            var default: ?*ast.types.Type = null;
            if (self.match(.equals)) {
                default = try self.parseTypeReference();
            }

            try params.append(.{
                .name = name,
                .constraint = constraint,
                .default = default,
            });

            if (!self.match(.comma)) break;
        }

        try self.consume(.greater_than, "Expected '>' after type parameters");
    }

    fn parseParameters(self: *Parser, params: *std.ArrayList(ast.node.FunctionExpr.FunctionParam)) !void {
        while (!self.check(.right_paren) and !self.check(.end_of_file)) {
            // Check for rest parameter (...name)
            const rest = self.match(.dot_dot_dot);

            // Parameter name
            if (!self.check(.identifier)) {
                return self.reportError("Expected parameter name");
            }
            const name = self.current.text;
            self.advance();

            // Optional marker
            const optional = self.match(.question);

            // Type annotation
            var param_type: ?*ast.types.Type = null;
            if (self.match(.colon)) {
                param_type = try self.parseTypeReference();
            }

            // Default value
            var default_value: ?*ast.Node = null;
            if (self.match(.equals)) {
                default_value = try self.parseExpression();
            }

            try params.append(.{
                .name = name,
                .type = param_type,
                .optional = optional,
                .default_value = default_value,
                .rest = rest,
            });

            if (!self.match(.comma)) break;
        }
    }

    // ===== Token Helpers =====

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = self.lexer.next() catch {
                // On lexer error, return EOF token
                self.current = Token.init(.end_of_file, ast.SourceLocation.dummy(), "");
                break;
            };

            // Skip newlines (not significant in most contexts)
            if (self.current.kind == .newline) continue;

            // Capture JSDoc comments for attachment to following declarations
            // Store the most recent doc comment; it will be consumed when attached to a node
            if (self.current.kind == .doc_comment) {
                self.pending_doc_comment = self.current.text;
                continue;
            }

            // Skip syntax errors but record them
            if (self.current.kind == .syntax_error) {
                self.errors.append(.{
                    .message = self.current.text,
                    .loc = self.current.loc,
                    .severity = .@"error",
                }) catch {};
                continue;
            }

            break;
        }
    }

    /// Consume the pending doc comment and return it (for attaching to a declaration node)
    /// Returns null if no doc comment was pending
    fn consumeDocComment(self: *Parser) ?[]const u8 {
        const doc = self.pending_doc_comment;
        self.pending_doc_comment = null;
        return doc;
    }

    fn check(self: *Parser, kind: TokenKind) bool {
        return self.current.kind == kind;
    }

    /// Check if current position is a system macro (requires @ prefix)
    /// System macros: @target, @emit, @extern, @comptime, @inline
    /// These are always available without import.
    ///
    /// User macros (derive, serialize, etc.) don't use @ and are resolved
    /// via symbol table after import.
    fn checkSystemMacro(self: *Parser) bool {
        return self.current.kind == .at_sign;
    }

    /// Check if current token starts a system macro invocation
    /// Used for decorator position (before class/function)
    fn checkMacro(self: *Parser) bool {
        return self.checkSystemMacro();
    }

    /// Peek at the next token and check if it's an identifier with the given name
    /// We do this by temporarily advancing the lexer and then backing up
    fn peekNextIsIdentifier(self: *Parser, name: []const u8) bool {
        // Save lexer state
        const saved_current = self.lexer.current;
        const saved_start = self.lexer.start;
        const saved_line = self.lexer.line;
        const saved_column = self.lexer.column;
        const saved_start_column = self.lexer.start_column;
        const saved_start_line = self.lexer.start_line;

        // Peek the next token
        const next_tok = self.lexer.next() catch {
            // Restore on error
            self.lexer.current = saved_current;
            self.lexer.start = saved_start;
            self.lexer.line = saved_line;
            self.lexer.column = saved_column;
            self.lexer.start_column = saved_start_column;
            self.lexer.start_line = saved_start_line;
            return false;
        };

        // Restore lexer state
        self.lexer.current = saved_current;
        self.lexer.start = saved_start;
        self.lexer.line = saved_line;
        self.lexer.column = saved_column;
        self.lexer.start_column = saved_start_column;
        self.lexer.start_line = saved_start_line;

        return next_tok.kind == .identifier and std.mem.eql(u8, next_tok.text, name);
    }

    fn match(self: *Parser, kind: TokenKind) bool {
        if (!self.check(kind)) return false;
        self.advance();
        return true;
    }

    fn consume(self: *Parser, kind: TokenKind, message: []const u8) !void {
        if (self.check(kind)) {
            self.advance();
            return;
        }
        return self.reportError(message);
    }

    fn reportError(self: *Parser, message: []const u8) error{ParseError} {
        if (!self.panic_mode) {
            self.panic_mode = true;
            self.errors.append(.{
                .message = message,
                .loc = self.current.loc,
                .severity = .@"error",
            }) catch {};
        }
        return error.ParseError;
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (!self.check(.end_of_file)) {
            // Check if current token starts a new statement
            switch (self.current.kind) {
                .keyword_class,
                .keyword_function,
                .keyword_const,
                .keyword_let,
                .keyword_var,
                .keyword_if,
                .keyword_while,
                .keyword_for,
                .keyword_return,
                .keyword_import,
                .keyword_export,
                .keyword_interface,
                .keyword_type,
                .keyword_enum,
                .at_sign, // All macros start with @
                => return,
                // Skip closing braces - we've gone past the error
                .right_brace => {
                    self.advance();
                    return;
                },
                else => {},
            }

            // Advance and check if we just passed a semicolon
            self.advance();
            if (self.previous.kind == .semicolon) return;
        }
    }

    fn makeLoc(self: *Parser) ast.SourceLocation {
        return self.current.loc;
    }

    fn mergeLoc(self: *Parser, start: ast.SourceLocation, end: ast.SourceLocation) ast.SourceLocation {
        _ = self;
        return ast.SourceLocation.init(
            start.file_id,
            start.start,
            end.end,
        );
    }
};

// ===== Tests =====

test "parser initialization" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "const x = 42;", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(ast.NodeKind.program, program.kind);
}

test "parse variable declaration" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "const x = 42;", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    try std.testing.expectEqual(ast.NodeKind.variable_stmt, program.data.program.statements[0].kind);
}

test "parse function declaration" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "function greet(name: string): string { return name; }", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    try std.testing.expectEqual(ast.NodeKind.function_decl, program.data.program.statements[0].kind);

    const func = &program.data.program.statements[0].data.function_decl;
    try std.testing.expectEqualStrings("greet", func.name);
    try std.testing.expectEqual(@as(usize, 1), func.params.len);
}

test "parse class declaration" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "class User { name: string; }", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    try std.testing.expectEqual(ast.NodeKind.class_decl, program.data.program.statements[0].kind);

    const class = &program.data.program.statements[0].data.class_decl;
    try std.testing.expectEqualStrings("User", class.name);
}

test "parse decorator" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "@derive(Eq, Hash) class User { }", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    // Should parse decorator + class
    try std.testing.expect(program.data.program.statements.len >= 1);
}

test "parse binary expression" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "1 + 2 * 3;", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const stmt = program.data.program.statements[0];
    try std.testing.expectEqual(ast.NodeKind.expression_stmt, stmt.kind);

    // 1 + (2 * 3) - multiplication binds tighter
    const expr = stmt.data.expression_stmt;
    try std.testing.expectEqual(ast.NodeKind.binary_expr, expr.kind);
    try std.testing.expectEqual(ast.node.BinaryOp.add, expr.data.binary_expr.op);
}

test "parse if statement" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "if (x > 0) { return x; }", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);
    try std.testing.expectEqual(ast.NodeKind.if_stmt, program.data.program.statements[0].kind);
}

test "parse call expression" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, "foo(1, 2, 3);", file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const stmt = program.data.program.statements[0];
    const expr = stmt.data.expression_stmt;
    try std.testing.expectEqual(ast.NodeKind.call_expr, expr.kind);
    try std.testing.expectEqual(@as(usize, 3), expr.data.call_expr.arguments.len);
}

test "parser: JSDoc comment attached to function" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const source =
        \\/** This is a test function */
        \\function foo() {}
    ;

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const func = program.data.program.statements[0];
    try std.testing.expectEqual(ast.NodeKind.function_decl, func.kind);

    // JSDoc should be attached
    try std.testing.expect(func.doc_comment != null);
    try std.testing.expect(std.mem.indexOf(u8, func.doc_comment.?, "test function") != null);
}

test "parser: JSDoc comment attached to class" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const source =
        \\/** User class documentation */
        \\class User {}
    ;

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const class = program.data.program.statements[0];
    try std.testing.expectEqual(ast.NodeKind.class_decl, class.kind);

    // JSDoc should be attached
    try std.testing.expect(class.doc_comment != null);
    try std.testing.expect(std.mem.indexOf(u8, class.doc_comment.?, "User class") != null);
}

test "parser: JSDoc comment attached to variable" {
    const allocator = std.testing.allocator;
    var arena = ast.ASTArena.init(allocator);
    defer arena.deinit();

    const source =
        \\/** Important constant */
        \\const MAX_SIZE = 100;
    ;

    const file_id = try arena.addFile("test.ms");
    var lexer = try Lexer.init(allocator, source, file_id);
    defer lexer.deinit();
    var parser = Parser.init(allocator, &arena, &lexer, file_id);
    defer parser.deinit();

    const program = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), program.data.program.statements.len);

    const var_stmt = program.data.program.statements[0];
    try std.testing.expectEqual(ast.NodeKind.variable_stmt, var_stmt.kind);

    // JSDoc should be attached
    try std.testing.expect(var_stmt.doc_comment != null);
    try std.testing.expect(std.mem.indexOf(u8, var_stmt.doc_comment.?, "Important constant") != null);
}
