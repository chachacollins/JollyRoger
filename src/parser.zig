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
    precedences: std.AutoHashMap(token.TokenType, Precedence),
    prefixParseFns: std.AutoHashMap(token.TokenType, prefixParseFn),
    infixParseFns: std.AutoHashMap(token.TokenType, infixParseFn),
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    pub fn init(lex: lexer.Lexer, allocator: std.mem.Allocator) !Parser {
        const arena = std.heap.ArenaAllocator.init(allocator);
        var p = Parser{
            //zls
            .lexer = lex,
            .curToken = undefined,
            .peekToken = undefined,
            .allocator = allocator,
            .errors = std.ArrayList([]u8).init(allocator),
            .prefixParseFns = std.AutoHashMap(token.TokenType, prefixParseFn).init(allocator),
            .infixParseFns = std.AutoHashMap(token.TokenType, infixParseFn).init(allocator),
            .precedences = std.AutoHashMap(token.TokenType, Precedence).init(allocator),
            .arena = arena,
        };
        try p.registerPrefix(token.TokenType.IDENT, parseIdentifier);
        try p.registerPrefix(token.TokenType.INT, parseIntegerLiteral);
        try p.registerPrefix(token.TokenType.BANG, parsePrefixExpression);
        try p.registerPrefix(token.TokenType.MINUS, parsePrefixExpression);
        try p.registerPrefix(token.TokenType.TRUE, parseBool);
        try p.registerPrefix(token.TokenType.FALSE, parseBool);
        try p.registerPrefix(token.TokenType.LPAREN, parseGroupedExpression);
        try p.registerPrefix(token.TokenType.IF, parseIfExpression);
        try p.registerPrefix(token.TokenType.FUNCTION, parseFunctionLiteral);
        try p.registerInfix(token.TokenType.PLUS, parseInfixExpression);
        try p.registerInfix(token.TokenType.MINUS, parseInfixExpression);
        try p.registerInfix(token.TokenType.SLASH, parseInfixExpression);
        try p.registerInfix(token.TokenType.ASTERIS, parseInfixExpression);
        try p.registerInfix(token.TokenType.EQ, parseInfixExpression);
        try p.registerInfix(token.TokenType.NOT_EQ, parseInfixExpression);
        try p.registerInfix(token.TokenType.LT, parseInfixExpression);
        try p.registerInfix(token.TokenType.GT, parseInfixExpression);
        try p.registerInfix(token.TokenType.LPAREN, parseCallExpression);

        try p.precedences.put(token.TokenType.EQ, Precedence.EQUALS);
        try p.precedences.put(token.TokenType.NOT_EQ, Precedence.EQUALS);
        try p.precedences.put(token.TokenType.LT, Precedence.LESSGREATER);
        try p.precedences.put(token.TokenType.GT, Precedence.LESSGREATER);
        try p.precedences.put(token.TokenType.PLUS, Precedence.SUM);
        try p.precedences.put(token.TokenType.MINUS, Precedence.SUM);
        try p.precedences.put(token.TokenType.SLASH, Precedence.SUM);
        try p.precedences.put(token.TokenType.ASTERIS, Precedence.SUM);
        try p.precedences.put(token.TokenType.LPAREN, Precedence.CALL);

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
        self.precedences.deinit();
        self.arena.deinit();
    }

    pub fn errors(self: *Parser) []const u8 {
        return self.errors;
    }
    fn noPrefixFnError(self: *Parser, tok: token.TokenType) void {
        _ = self;
        std.debug.print("no prefix function found for {}\n", .{tok});
    }
    fn registerPrefix(self: *Parser, tokenType: token.TokenType, func: prefixParseFn) !void {
        try self.prefixParseFns.put(tokenType, func);
    }

    fn registerInfix(self: *Parser, tokenType: token.TokenType, func: infixParseFn) !void {
        try self.infixParseFns.put(tokenType, func);
    }
    fn peekPrecedence(self: *Parser) !Precedence {
        const prec = self.precedences.get(self.peekToken.Type) orelse Precedence.LOWEST;
        return prec;
    }

    fn curPrecedence(self: *Parser) !u8 {
        const prec = try self.precedences.get(self.curToken.Type) orelse Precedence.LOWEST;
        return @intFromEnum(prec);
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
        const prefix = self.prefixParseFns.get(self.curToken.Type) orelse {
            self.noPrefixFnError(self.curToken.Type);
            return error.ParsingError;
        };
        var leftExpression = try prefix(self);
        while (!self.peekTokenIs(token.TokenType.SEMICOLON) and @intFromEnum(precedence) < @intFromEnum(try self.peekPrecedence())) {
            const infix = self.infixParseFns.get(self.peekToken.Type) orelse return leftExpression;
            try self.nextToken();
            leftExpression = try infix(self, leftExpression);
        }

        return leftExpression;
    }
    fn parseBlockStatement(self: *Parser) !ast.BlockStatementStruct {
        var block = ast.BlockStatementStruct{ .token = self.curToken, .statement = std.ArrayList(ast.Statement).init(self.arena.allocator()) };
        try self.nextToken();
        while (!self.curTokenIs(token.TokenType.RBRACE) and !self.curTokenIs(token.TokenType.EOF)) {
            const stmt = try self.parseStatement() orelse undefined;
            try block.statement.append(stmt);
            try self.nextToken();
        }
        return block;
    }
    pub fn parseIfExpression(self: *Parser) !ast.Expression {
        var ifExp = ast.IfExpressionStruct{ .token = self.curToken, .condition = undefined, .consequence = undefined, .alternative = null };
        if (!try self.expectPeek(token.TokenType.LPAREN)) {
            return error.MissingLPAREN;
        }
        try self.nextToken();
        const expCond = try self.arena.allocator().create(ast.Expression);
        expCond.* = try self.parseExpression(Precedence.LOWEST);
        ifExp.condition = expCond;
        if (!try self.expectPeek(token.TokenType.RPAREN)) {
            return error.MissingRPAREN;
        }
        if (!try self.expectPeek(token.TokenType.LBRACE)) {
            return error.MissingLBRACE;
        }
        ifExp.consequence = try self.parseBlockStatement();
        if (self.peekTokenIs(token.TokenType.ELSE)) {
            try self.nextToken();
            if (!try self.expectPeek(token.TokenType.LBRACE)) {
                return error.MissingLBRACE;
            }
            ifExp.alternative = try self.parseBlockStatement();
        }
        return ast.Expression{ .ifExpression = ifExp };
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
    pub fn parsePrefixExpression(self: *Parser) !ast.Expression {
        var exps = ast.PrefixExpressionStruct{
            .token = self.curToken,
            .operator = self.curToken.Literal,
            .right = undefined,
        };
        try self.nextToken();
        const right_exp = try self.arena.allocator().create(ast.Expression);
        right_exp.* = try self.parseExpression(Precedence.PREFIX);
        exps.right = right_exp;
        return ast.Expression{ .prefixExpression = exps };
    }
    pub fn parseInfixExpression(self: *Parser, left: ast.Expression) !ast.Expression {
        var exps = ast.InfixExpressionStruct{
            .token = self.curToken,
            .operator = self.curToken.Literal,
            .left = undefined,
            .right = undefined,
        };
        const leftExp = try self.arena.allocator().create(ast.Expression);
        leftExp.* = left;
        exps.left = leftExp;
        const precedence = try self.peekPrecedence();
        try self.nextToken();
        const rightExp = try self.arena.allocator().create(ast.Expression);
        rightExp.* = try self.parseExpression(precedence);
        exps.right = rightExp;
        return ast.Expression{ .infixExpression = exps };
    }
    pub fn parseBool(self: *Parser) !ast.Expression {
        const exp = ast.BooleanLiteralStruct{ .token = self.curToken, .value = self.curTokenIs(token.TokenType.TRUE) };
        const stmt = ast.Expression{ .boolean = exp };
        return stmt;
    }
    pub fn parseGroupedExpression(self: *Parser) !ast.Expression {
        try self.nextToken();
        const exp = try self.parseExpression(Precedence.LOWEST);
        if (!try self.expectPeek(token.TokenType.RPAREN)) {
            return error.ExpectedRparen;
        }
        return exp;
    }
    pub fn parseFunctionLiteral(self: *Parser) !ast.Expression {
        var lit = ast.FunctionLiteralStruct{ .token = self.curToken, .body = undefined, .parameters = std.ArrayList(ast.Identifier).init(self.arena.allocator()) };

        if (!try self.expectPeek(token.TokenType.LPAREN)) {
            return error.MissingLPAREN;
        }
        try self.parseFunctionParameters(&lit.parameters);
        if (!try self.expectPeek(token.TokenType.LBRACE)) {
            return error.MissingLBRACE;
        }

        lit.body = try self.parseBlockStatement();
        return ast.Expression{ .functionLiteral = lit };
    }

    fn parseFunctionParameters(self: *Parser, params: *std.ArrayList(ast.Identifier)) !void {
        if (self.peekTokenIs(token.TokenType.RPAREN)) {
            try self.nextToken();
            return;
        }

        try self.nextToken();
        const ident = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };
        try params.append(ident);

        while (self.peekTokenIs(token.TokenType.COMMA)) {
            try self.nextToken();
            try self.nextToken();
            const idents = ast.Identifier{ .token = self.curToken, .value = self.curToken.Literal };
            try params.append(idents);
        }

        if (!try self.expectPeek(token.TokenType.RPAREN)) {
            return error.MissingRPAREN;
        }
    }
    pub fn parseCallExpression(self: *Parser, func: ast.Expression) !ast.Expression {
        const funct = try self.arena.allocator().create(ast.Expression);
        funct.* = func;
        var exp = ast.CallExpressionStruct{ .token = self.curToken, .function = funct, .arguements = std.ArrayList(ast.Expression).init(self.arena.allocator()) };
        try self.parseCallArguements(&exp.arguements);
        return ast.Expression{ .callExpression = exp };
    }
    fn parseCallArguements(self: *Parser, args: *std.ArrayList(ast.Expression)) !void {
        if (self.peekTokenIs(token.TokenType.RPAREN)) {
            try self.nextToken();
            return;
        }

        try self.nextToken();
        try args.append(try self.parseExpression(Precedence.LOWEST));
        while (self.peekTokenIs(token.TokenType.COMMA)) {
            try self.nextToken();
            try self.nextToken();
            try args.append(try self.parseExpression(Precedence.LOWEST));
        }

        if (!try self.expectPeek(token.TokenType.RPAREN)) {
            return error.MissingRPAREN;
        }
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

test "TestPrefixParsing" {
    const testStruct = struct {
        input: []const u8,
        operator: []const u8,
        integerValue: i64,
    };
    const tests = [_]testStruct{
        testStruct{ .input = "!5", .operator = "!", .integerValue = 5 },
        testStruct{ .input = "-15", .operator = "-", .integerValue = 15 },
    };
    for (tests) |tt| {
        const lex = lexer.Lexer.init(tt.input);
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
                const exp = exprstmt.expression.prefixExpression;
                try std.testing.expectEqualStrings(tt.operator, exp.operator);
                try testIntegerLiteral(exp.right.*, tt.integerValue);
            },
            else => {
                std.zig.fatal("s not ast.expressionStatement got {}\n ", .{stmt});
            },
        }
    }
}

fn testIntegerLiteral(exp: ast.Expression, value: i64) !void {
    switch (exp) {
        .integerLiteral => |integ| {
            if (integ.value != value) {
                std.zig.fatal("got value {d} but expected {d}\n", .{ integ.value, value });
            }
            var buffer: [256]u8 = undefined;
            const str = try std.fmt.bufPrint(&buffer, "{d}", .{value});
            if (!std.mem.eql(u8, str, integ.tokenLiteral())) {
                std.zig.fatal("got value {s} but expected {s}\n", .{ integ.tokenLiteral(), str });
            }
        },
        else => {
            std.zig.fatal("Not integerLiteral {}\n", .{exp});
        },
    }
}

test "TestParsingInfixOperations" {
    const testStruct = struct {
        input: []const u8,
        leftValue: i64,
        operator: []const u8,
        rightValue: i64,
    };
    const tests = [_]testStruct{
        testStruct{ .input = "5 + 5", .operator = "+", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 - 5", .operator = "-", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 * 5", .operator = "*", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 / 5", .operator = "/", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 > 5", .operator = ">", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 < 5", .operator = "<", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 == 5", .operator = "==", .leftValue = 5, .rightValue = 5 },
        testStruct{ .input = "5 != 5", .operator = "!=", .leftValue = 5, .rightValue = 5 },
    };
    for (tests) |tt| {
        const lex = lexer.Lexer.init(tt.input);
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
                const exp = exprstmt.expression.infixExpression;
                try std.testing.expectEqualStrings(tt.operator, exp.operator);
                try testIntegerLiteral(exp.left.*, tt.leftValue);
                try testIntegerLiteral(exp.right.*, tt.rightValue);
            },
            else => {
                std.zig.fatal("s not ast.expressionStatement got {}\n ", .{stmt});
            },
        }
    }
}

test "TestBooleanParsing" {
    const testStruct = struct { input: []const u8, value: bool };
    const tests = [_]testStruct{
        testStruct{ .input = "true", .value = true },
        testStruct{ .input = "false", .value = false },
    };
    for (tests) |tt| {
        const lex = lexer.Lexer.init(tt.input);
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
                const exp = exprstmt.expression;
                try testBool(exp, tt.value);
            },
            else => {
                std.zig.fatal("s not ast.expressionStatement got {}\n ", .{stmt});
            },
        }
    }
}

fn testBool(exp: ast.Expression, value: bool) !void {
    switch (exp) {
        .boolean => |booln| {
            if (booln.value != value) {
                std.zig.fatal("got value {} but expected {}\n", .{ booln.value, value });
            }
        },
        else => {
            std.zig.fatal("Not boolnerLiteral {}\n", .{exp});
        },
    }
}

test "TestIfExpression" {
    const input = "if (x < y) { x }";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram() orelse {
        std.debug.panic("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.debug.panic("expected 1 program statement but got {d}\n", .{program.statements.len});
    }

    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            const if_exp = switch (exprstmt.expression) {
                .ifExpression => |if_exp| if_exp,
                else => {
                    std.debug.panic("expression not ifExpression, got {}\n", .{exprstmt.expression});
                },
            };

            const condition = switch (if_exp.condition.*) {
                .infixExpression => |infix| infix,
                else => {
                    std.debug.panic("condition not infixExpression, got {}\n", .{if_exp.condition.*});
                },
            };

            try std.testing.expectEqualStrings("<", condition.operator);

            switch (condition.left.*) {
                .identifier => |ident| {
                    try std.testing.expectEqualStrings("x", ident.value);
                },
                else => std.debug.panic("left not identifier, got {}\n", .{condition.left.*}),
            }

            switch (condition.right.*) {
                .identifier => |ident| {
                    try std.testing.expectEqualStrings("y", ident.value);
                },
                else => std.debug.panic("right not identifier, got {}\n", .{condition.right.*}),
            }

            if (if_exp.consequence.statement.items.len != 1) {
                std.debug.panic("consequence should have 1 statement, got {d}\n", .{if_exp.consequence.statement.items.len});
            }

            switch (if_exp.consequence.statement.items[0]) {
                .expressionStatement => |conseq_expr| {
                    switch (conseq_expr.expression) {
                        .identifier => |ident| {
                            try std.testing.expectEqualStrings("x", ident.value);
                        },
                        else => std.debug.panic("consequence not identifier, got {}\n", .{conseq_expr.expression}),
                    }
                },
                else => std.debug.panic("consequence statement not expressionStatement, got {}\n", .{if_exp.consequence.statement.items[0]}),
            }

            try std.testing.expect(if_exp.alternative == null);
        },
        else => {
            std.debug.panic("statement not expressionStatement, got {}\n", .{stmt});
        },
    }
}
test "TestIfElseExpression" {
    const input = "if (x < y) { x } else { y }";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram() orelse {
        std.debug.panic("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.debug.panic("expected 1 program statement but got {d}\n", .{program.statements.len});
    }

    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            const if_exp = switch (exprstmt.expression) {
                .ifExpression => |if_exp| if_exp,
                else => {
                    std.debug.panic("expression not ifExpression, got {}\n", .{exprstmt.expression});
                },
            };

            // Test condition
            const condition = switch (if_exp.condition.*) {
                .infixExpression => |infix| infix,
                else => {
                    std.debug.panic("condition not infixExpression, got {}\n", .{if_exp.condition.*});
                },
            };

            try std.testing.expectEqualStrings("<", condition.operator);

            // Test left side of condition
            switch (condition.left.*) {
                .identifier => |ident| {
                    try std.testing.expectEqualStrings("x", ident.value);
                },
                else => std.debug.panic("left not identifier, got {}\n", .{condition.left.*}),
            }

            // Test right side of condition
            switch (condition.right.*) {
                .identifier => |ident| {
                    try std.testing.expectEqualStrings("y", ident.value);
                },
                else => std.debug.panic("right not identifier, got {}\n", .{condition.right.*}),
            }

            // Test consequence
            if (if_exp.consequence.statement.items.len != 1) {
                std.debug.panic("consequence should have 1 statement, got {d}\n", .{if_exp.consequence.statement.items.len});
            }

            switch (if_exp.consequence.statement.items[0]) {
                .expressionStatement => |conseq_expr| {
                    switch (conseq_expr.expression) {
                        .identifier => |ident| {
                            try std.testing.expectEqualStrings("x", ident.value);
                        },
                        else => std.debug.panic("consequence not identifier, got {}\n", .{conseq_expr.expression}),
                    }
                },
                else => std.debug.panic("consequence statement not expressionStatement, got {}\n", .{if_exp.consequence.statement.items[0]}),
            }

            // Test alternative (else block)
            if (if_exp.alternative) |alt| {
                if (alt.statement.items.len != 1) {
                    std.debug.panic("alternative should have 1 statement, got {d}\n", .{alt.statement.items.len});
                }

                switch (alt.statement.items[0]) {
                    .expressionStatement => |alt_expr| {
                        switch (alt_expr.expression) {
                            .identifier => |ident| {
                                try std.testing.expectEqualStrings("y", ident.value);
                            },
                            else => std.debug.panic("alternative not identifier, got {}\n", .{alt_expr.expression}),
                        }
                    },
                    else => std.debug.panic("alternative statement not expressionStatement, got {}\n", .{alt.statement.items[0]}),
                }
            } else {
                std.debug.panic("expected alternative to be present\n", .{});
            }
        },
        else => {
            std.debug.panic("statement not expressionStatement, got {}\n", .{stmt});
        },
    }
}

test "TestFunctionLiteralParsing" {
    const input = "fn(x, y) { x + y; }";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram() orelse {
        std.debug.panic("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.debug.panic("expected 1 program statement but got {d}\n", .{program.statements.len});
    }

    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            const function = switch (exprstmt.expression) {
                .functionLiteral => |fn_lit| fn_lit,
                else => {
                    std.debug.panic("expression not functionLiteral, got {}\n", .{exprstmt.expression});
                },
            };

            // Test parameters
            if (function.parameters.items.len != 2) {
                std.debug.panic("function literal should have 2 parameters, got {d}\n", .{function.parameters.items.len});
            }

            try std.testing.expectEqualStrings("x", function.parameters.items[0].value);
            try std.testing.expectEqualStrings("y", function.parameters.items[1].value);

            // Test function body
            if (function.body.statement.items.len != 1) {
                std.debug.panic("function body should have 1 statement, got {d}\n", .{function.body.statement.items.len});
            }

            const bodyStmt = function.body.statement.items[0];
            switch (bodyStmt) {
                .expressionStatement => |body_expr| {
                    const infix = switch (body_expr.expression) {
                        .infixExpression => |infix| infix,
                        else => {
                            std.debug.panic("body expression not infixExpression, got {}\n", .{body_expr.expression});
                        },
                    };

                    try std.testing.expectEqualStrings("+", infix.operator);

                    // Test left side of expression
                    switch (infix.left.*) {
                        .identifier => |ident| {
                            try std.testing.expectEqualStrings("x", ident.value);
                        },
                        else => std.debug.panic("left not identifier, got {}\n", .{infix.left.*}),
                    }

                    // Test right side of expression
                    switch (infix.right.*) {
                        .identifier => |ident| {
                            try std.testing.expectEqualStrings("y", ident.value);
                        },
                        else => std.debug.panic("right not identifier, got {}\n", .{infix.right.*}),
                    }
                },
                else => std.debug.panic("body statement not expressionStatement, got {}\n", .{bodyStmt}),
            }
        },
        else => {
            std.debug.panic("statement not expressionStatement, got {}\n", .{stmt});
        },
    }
}

test "TestCallExpressionParsing" {
    const input = "add(1, 2 * 3, 4 + 5);";
    const lex = lexer.Lexer.init(input);
    var parser = try Parser.init(lex, std.testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram() orelse {
        std.debug.panic("parse program returned null\n", .{});
    };
    defer parser.allocator.free(program.statements);

    if (program.statements.len != 1) {
        std.debug.panic("expected 1 program statement but got {d}\n", .{program.statements.len});
    }

    const stmt = program.statements[0];
    switch (stmt) {
        .expressionStatement => |exprstmt| {
            const call = switch (exprstmt.expression) {
                .callExpression => |call_expr| call_expr,
                else => {
                    std.debug.panic("expression not callExpression, got {}\n", .{exprstmt.expression});
                },
            };

            // Test function identifier
            switch (call.function.*) {
                .identifier => |ident| {
                    try std.testing.expectEqualStrings("add", ident.value);
                },
                else => std.debug.panic("function not identifier, got {}\n", .{call.function.*}),
            }

            // Test arguements
            if (call.arguements.items.len != 3) {
                std.debug.panic("call should have 3 arguements, got {d}\n", .{call.arguements.items.len});
            }

            // Test first argument (1)
            switch (call.arguements.items[0]) {
                .integerLiteral => |int| {
                    try std.testing.expectEqual(@as(i64, 1), int.value);
                },
                else => std.debug.panic("first argument not integer, got {}\n", .{call.arguements.items[0]}),
            }

            // Test second argument (2 * 3)
            switch (call.arguements.items[1]) {
                .infixExpression => |infix| {
                    try std.testing.expectEqualStrings("*", infix.operator);

                    switch (infix.left.*) {
                        .integerLiteral => |int| {
                            try std.testing.expectEqual(@as(i64, 2), int.value);
                        },
                        else => std.debug.panic("left not integer, got {}\n", .{infix.left.*}),
                    }

                    switch (infix.right.*) {
                        .integerLiteral => |int| {
                            try std.testing.expectEqual(@as(i64, 3), int.value);
                        },
                        else => std.debug.panic("right not integer, got {}\n", .{infix.right.*}),
                    }
                },
                else => std.debug.panic("second argument not infix expression, got {}\n", .{call.arguements.items[1]}),
            }

            // Test third argument (4 + 5)
            switch (call.arguements.items[2]) {
                .infixExpression => |infix| {
                    try std.testing.expectEqualStrings("+", infix.operator);

                    switch (infix.left.*) {
                        .integerLiteral => |int| {
                            try std.testing.expectEqual(@as(i64, 4), int.value);
                        },
                        else => std.debug.panic("left not integer, got {}\n", .{infix.left.*}),
                    }

                    switch (infix.right.*) {
                        .integerLiteral => |int| {
                            try std.testing.expectEqual(@as(i64, 5), int.value);
                        },
                        else => std.debug.panic("right not integer, got {}\n", .{infix.right.*}),
                    }
                },
                else => std.debug.panic("third argument not infix expression, got {}\n", .{call.arguements.items[2]}),
            }
        },
        else => {
            std.debug.panic("statement not expressionStatement, got {}\n", .{stmt});
        },
    }
}
