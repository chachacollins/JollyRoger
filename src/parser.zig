const lexer = @import("lexer.zig");
const token = @import("token.zig");
const ast = @import("ast.zig");
const std = @import("std");
pub const Parser = struct {
    lexer: lexer.Lexer,
    curToken: token.Token,
    peekToken: token.Token,
    allocator: std.mem.Allocator,
    pub fn init(lex: lexer.Lexer, allocator: std.mem.Allocator) !Parser {
        var p = Parser{ .lexer = lex, .curToken = undefined, .peekToken = undefined, .allocator = allocator };
        try p.nextToken();
        try p.nextToken();
        return p;
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
        program.statements = programStatements.items;
        return program;
    }
    fn parseStatement(self: *Parser) !?ast.Statement {
        switch (self.curToken.Type) {
            token.TokenType.LET => {
                return try self.parseLetStatement();
            },
            else => {
                return null;
            },
        }
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
    const program = try parser.parseProgram() orelse std.zig.fatal("parse program returned null\n", .{});
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

    parser.allocator.free(program.statements);
}
fn testLetStatement(s: ast.Statement, name: []const u8) void {
    if (!std.mem.eql(u8, s.tokenLiteral(), "let")) {
        std.zig.fatal("s not ast.LetStatement literal got {any}\n ", .{s.tokenLiteral()});
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
        // else => {
        //     std.zig.fatal("s not ast.LetStatement got {any}\n ", .{s});
        // },
    }
}
