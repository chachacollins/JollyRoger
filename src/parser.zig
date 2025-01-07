const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const std = @import("std");

const prefixParseFn = *const fn (*Parser) anyerror!ast.Expression;
const infixParseFn = *const fn (*Parser, ast.Expression) anyerror!ast.Expression;

const Precedence = enum(u8) {
    LOWEST = 0,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

pub const Parser = struct {
    lexer: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    errors: std.ArrayList([]u8),
    prefixParseFns: std.AutoHashMap(token.TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMap(token.TokenType, infixParseFn),
    allocator: std.mem.Allocator,
    pub fn init(lex: lexer.Lexer, allocator: std.mem.Allocator) !Parser {
        var p = Parser{
            //zls
            .lexer = lex,
            .curToken = undefined,
            .peekToken = undefined,
            .allocator = allocator,
            .errors = std.ArrayList([]u8).init(allocator),
            .prefixParseFns = std.AutoHashMap(token.TokenType, prefixParseFn).init(allocator),
            .infixParseFns = std.AutoHashMap(token.TokenType, infixParseFn).init(allocator),
        };
        try p.registerPrefix(token.TokenType.IDENT, parseIdentifier);
        try p.registerPrefix(token.TokenType.INT, parseIntegerLiteral);

        try p.nextToken();
        try p.nextToken();
        return p;
    }
    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |msg| {
            self.allocator.free(msg);
        }
        self.errors.deinit();
        self.prefixParseFns.deinit();
        self.infixParseFns.deinit();
    }

    pub fn errors(self: *Parser) []const u8 {
        return self.errors;
    }
    fn registerPrefix(self: *Parser, tokenType: token.TokenType, func: prefixParseFn) !void {
        try self.prefixParseFns.put(tokenType, func);
    }

    fn registerInfix(self: *Parser, tokenType: token.TokenType, func: infixParseFn) !void {
        try self.infixParseFns.put(tokenType, func);
    }

    fn peekError(self: *Parser, tok: token.TokenType) !void {
        const msg = try std.fmt.allocPrint(self.allocator, "expected next token to be {}, got {} instead", .{ tok, self.peekToken.Type });
        try self.errors.append(msg);
    }
    pub fn nextToken(self: *Parser) !void {
        self.curToken = self.peekToken;
        self.peekToken = try self.lexer.nextToken();
    }
    pub fn parseProgram(self: *Parser) !?ast.Program {
        var program = ast.Program{
            .statements = undefined,
        };
        var programStatements = std.ArrayList(ast.Statement).init(self.allocator);
        while (self.curToken.Type != token.TokenType.EOF) {
            const stmt = try self.parseStatement();
            try programStatements.append(stmt.?);
            try self.nextToken();
        }
        program.statements = try programStatements.toOwnedSlice();
        return program;
    }
    fn parseStatement(self: *Parser) !?ast.Statement {
        switch (self.curToken.Type) {
            token.TokenType.LET => {
                return try self.parseLetStatement();
            },
            token.TokenType.RETURN => {
                return try self.parseReturnStament();
            },
            else => {
                return try self.parseExpressionStatement();
            },
        }
    }
    fn parseReturnStament(self: *Parser) !?ast.Statement {
        const returnStmt = ast.ReturnStatementStruct{ .token = self.curToken, .returnValue = undefined };
        try self.nextToken();
        while (!self.curTokenIs(token.TokenType.SEMICOLON)) {
            try self.nextToken();
        }
        const stmt = ast.Statement{ .returnStatement = returnStmt };
        return stmt;
    }
    fn parseLetStatement(self: *Parser) !?ast.Statement {
        var letstmt = ast.LetStatementStruct{ .token = self.curToken, .name = undefined, .value = undefined };
        if (!try self.expectPeek(token.TokenType.IDENT)) {
            return null;
        }
        const astIdent = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };
        letstmt.name = astIdent;
        if (!try self.expectPeek(token.TokenType.ASSIGN)) {
            return null;
        }
        while (self.curToken.Type != token.TokenType.SEMICOLON) {
            try self.nextToken();
        }
        const stmt = ast.Statement{ .letStatement = letstmt };
        return stmt;
    }
    fn parseExpressionStatement(self: *Parser) !ast.Statement {
        var exprstmt = ast.ExpressionStatementStruct{ .token = self.curToken, .expression = undefined };
        exprstmt.expression = try self.parseExpression(Precedence.LOWEST);
        if (self.peekTokenIs(token.TokenType.SEMICOLON)) {
            try self.nextToken();
        }
        const stmt = ast.Statement{ .expressionStatement = exprstmt };
        return stmt;
    }
    fn parseExpression(self: *Parser, precedence: Precedence) !ast.Expression {
        _ = precedence;
        const prefix = self.prefixParseFns.get(self.curToken.Type) orelse {
            self.errors.append(try std.fmt.allocPrint(self.allocator, "no prefix parse function for {s} found", .{@tagName(self.curToken.Type)})) catch unreachable;
            return error.ParsingError;
        };

        const leftExpression = try prefix(self);

        return leftExpression;
    }
    pub fn parseIdentifier(self: *Parser) !ast.Expression {
        const pi = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };
        const stmt = ast.Expression{ .identifier = pi };
        return stmt;
    }
    pub fn parseIntegerLiteral(self: *Parser) !ast.Expression {
        var lit = ast.IntegerLiteralStruct{ .token = self.curToken, .value = undefined };
        const value = try std.fmt.parseInt(i64, self.curToken.Literal, 10);
        lit.value = value;
        const stmt = ast.Expression{ .integerLiteral = lit };
        return stmt;
    }

    fn curTokenIs(self: *Parser, t: token.TokenType) bool {
        return self.curToken.Type == t;
    }

    fn peekTokenIs(self: *Parser, t: token.TokenType) bool {
        return self.peekToken.Type == t;
    }

    fn expectPeek(self: *Parser, t: token.TokenType) !bool {
        if (self.peekTokenIs(t)) {
            try self.nextToken();
            return true;
        } else {
            try self.peekError(t);
            return false;
        }
    }
};

test "TestLetStatement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram() orelse {
        if (parser.errors.items.len > 0) {
            std.log.warn("Parser has encountered an error\n", .{});
            for (parser.errors.items) |msg| {
                std.debug.print("parser errors {s}\n", .{msg});
            }
        }
        std.zig.fatal("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);
    if (program.statements.len != 3) {
        std.zig.fatal("expected 3 program statements but got {d}\n", .{program.statements.len});
    }
    const testStruct = struct {
        expectedIdentifier: []const u8,
    };
    const tests = [_]testStruct{
        testStruct{ .expectedIdentifier = "x" },
        testStruct{ .expectedIdentifier = "y" },
        testStruct{ .expectedIdentifier = "foobar" },
    };
    for (tests, 0..) |tt, i| {
        const stmt = program.statements[i];
        testLetStatement(stmt, tt.expectedIdentifier);
    }
}
fn testLetStatement(s: ast.Statement, name: []const u8) void {
    if (!std.mem.eql(u8, s.tokenLiteral(), "let")) {
        std.zig.fatal("s not ast.LetStatement literal got {s}\n ", .{s.tokenLiteral()});
    }
    switch (s) {
        .letStatement => |letsmt| {
            if (!std.mem.eql(u8, letsmt.name.value, name)) {
                std.zig.fatal("expected name : {s} but got {s}\n", .{ name, letsmt.name.value });
            }
            if (!std.mem.eql(u8, letsmt.name.tokenLiteral(), name)) {
                std.zig.fatal("expected token literal  : {s} but got {s}\n", .{ name, letsmt.name.tokenLiteral() });
            }
        },
        else => {
            std.zig.fatal("s not ast.LetStatement got {}\n ", .{s});
        },
    }
}

test "TestReturnStatement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram() orelse {
        std.zig.fatal("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 3) {
        std.zig.fatal("expected 3 program statements but got {d}\n", .{program.statements.len});
    }
    for (program.statements) |stmt| {
        switch (stmt) {
            .returnStatement => |returnStmt| {
                try std.testing.expectEqualStrings("return", returnStmt.tokenLiteral());
            },
            else => {
                std.zig.fatal("s not ast.returnStatement got {}\n ", .{stmt});
            },
        }
    }
}

test "TestIdentifierExpression" {
    const input = "foobar;";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram() orelse {
        std.zig.fatal("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.zig.fatal("expected 1 program statements but got {d}\n", .{program.statements.len});
    }
    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            try std.testing.expectEqualStrings("foobar", exprstmt.expression.identifier.value);
            try std.testing.expectEqualStrings("foobar", exprstmt.expression.identifier.tokenLiteral());
        },
        else => {
            std.zig.fatal("s not ast.expressionStatement got {}\n ", .{stmt});
        },
    }
}

test "TestIntegerExpression" {
    const input = "5;";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram() orelse {
        std.zig.fatal("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.zig.fatal("expected 1 program statements but got {d}\n", .{program.statements.len});
    }
    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            try std.testing.expectEqual(5, exprstmt.expression.integerLiteral.value);
            try std.testing.expectEqualStrings("5", exprstmt.expression.integerLiteral.tokenLiteral());
        },
        else => {
            std.zig.fatal("s not ast.expressionStatement got {}\n ", .{stmt});
        },
    }
}
