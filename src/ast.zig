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
    blockStatement: BlockStatementStruct,
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
    infixExpression: InfixExpressionStruct,
    boolean: BooleanLiteralStruct,
    ifExpression: IfExpressionStruct,
    functionLiteral: FunctionLiteralStruct,
    callExpression: CallExpressionStruct,
    pub fn tokenLiteral(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| {
                return case.tokenLiteral();
            },
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),
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

pub const InfixExpressionStruct = struct {
    token: token.Token,
    operator: []const u8,
    left: *Expression,
    right: *Expression,
    pub fn tokenLiteral(self: InfixExpressionStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const BooleanLiteralStruct = struct {
    token: token.Token,
    value: bool,
    pub fn tokenLiteral(self: BooleanLiteralStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const IfExpressionStruct = struct {
    token: token.Token,
    condition: *Expression,
    consequence: BlockStatementStruct,
    alternative: ?BlockStatementStruct,
    pub fn tokenLiteral(self: IfExpressionStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const BlockStatementStruct = struct {
    token: token.Token,
    statement: std.ArrayList(Statement),

    pub fn tokenLiteral(self: BlockStatementStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const FunctionLiteralStruct = struct {
    token: token.Token,
    parameters: std.ArrayList(Identifier),
    body: BlockStatementStruct,

    pub fn tokenLiteral(self: FunctionLiteralStruct) []const u8 {
        return self.token.Literal;
    }
};

pub const CallExpressionStruct = struct {
    token: token.Token,
    function: *Expression,
    arguements: std.ArrayList(Expression),

    pub fn tokenLiteral(self: CallExpressionStruct) []const u8 {
        return self.token.Literal;
    }
};
