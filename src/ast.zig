const token = @import("token.zig");
const std = @import("std");

pub const Node = union(enum) {
    statement: Statement,
    expression: Expression,
    program: Program,

    pub fn tokenLiteral(self: Node) []const u8 {
        switch (self) {
            inline else => |case| {
                return case.tokenLiteral();
            },
        }
    }
};

pub const Statement = union(enum) {
    letStatement: LetStatementStruct,
    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| {
                return case.tokenLiteral();
            },
        }
    }
};
pub const Expression = union(enum) {
    identifier: Identifier,
    pub fn tokenLiteral(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| {
                return case.tokenLiteral();
            },
        }
    }
};

pub const Program = struct {
    statements: []Statement,
    pub fn tokenLiteral(self: Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};

pub const LetStatementStruct = struct {
    token: token.Token,
    name: Identifier,
    value: Expression,
    pub fn tokenLiteral(self: LetStatementStruct) []const u8 {
        return self.token.Literal;
    }
};
pub const Identifier = struct {
    token: token.Token,
    value: []const u8,
    pub fn tokenLiteral(self: Identifier) []const u8 {
        return self.token.Literal;
    }
};
