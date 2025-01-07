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
    returnStatement: ReturnStatementStruct,
    expressionStatement: ExpressionStatementStruct,
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
    integerLiteral: IntegerLiteralStruct,
    prefixExpression: PrefixExpressionStruct,
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

pub const ReturnStatementStruct = struct {
    token: token.Token,
    returnValue: Expression,

    pub fn tokenLiteral(self: ReturnStatementStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const ExpressionStatementStruct = struct {
    token: token.Token,
    expression: Expression,

    pub fn tokenLiteral(self: ExpressionStatementStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const IntegerLiteralStruct = struct {
    token: token.Token,
    value: i64,

    pub fn tokenLiteral(self: IntegerLiteralStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const PrefixExpressionStruct = struct {
    token: token.Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: PrefixExpressionStruct) []const u8 {
        return self.token.Literal;
    }
};
