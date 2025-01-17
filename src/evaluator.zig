const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

const TRUE = object.Boolean{ .value = true };
const FALSE = object.Boolean{ .value = false };
const NULL = object.Object{ .null_ = .{} };

pub fn Eval(node: ast.Program) !object.Object {
    if (node.statements.items.len > 0) {
        return try evalStatement(node.statements.items[0]);
    }
    return NULL;
}
pub fn evalStatement(statement: ast.Statement) !object.Object {
    switch (statement) {
        .expressionStatement => |exp| {
            return try evalExpressionStatement(exp.expression);
        },
        else => {
            return NULL;
        },
    }
}

pub fn evalExpressionStatement(exp: ast.Expression) !object.Object {
    switch (exp) {
        .integerLiteral => {
            const int_obj = object.Integer{ .value = exp.integerLiteral.value };
            return object.Object{ .integer = int_obj };
        },
        .boolean => {
            const boolean_obj = nativeBooleanObject(exp.boolean.value);
            return object.Object{ .boolean = boolean_obj };
        },
        .prefixExpression => {
            const right = try evalExpressionStatement(exp.prefixExpression.right.*);
            return evalPrefixExpression(exp.prefixExpression.operator, right);
        },
        else => {
            return NULL;
        },
    }
}
fn evalPrefixExpression(operator: []const u8, right: object.Object) object.Object {
    if (std.mem.eql(u8, "!", operator)) {
        return evalBangExpression(right);
    } else {
        return NULL;
    }
}

fn evalBangExpression(right: object.Object) object.Object {
    switch (right) {
        .boolean => |boolean| {
            if (boolean.value == true) {
                return object.Object{ .boolean = FALSE };
            } else if (boolean.value == false) {
                return object.Object{ .boolean = TRUE };
            }
        },
        .null_ => {
            return object.Object{ .boolean = TRUE };
        },
        else => {
            return object.Object{ .boolean = FALSE };
        },
    }
    return object.Object{ .boolean = FALSE };
}

fn nativeBooleanObject(input: bool) object.Boolean {
    if (input) {
        return TRUE;
    } else {
        return FALSE;
    }
}

test "TestEvalInterger Expression" {
    const testStruct = struct {
        input: []const u8,
        expected: i64,
    };
    const tests = [_]testStruct{
        testStruct{ .input = "5", .expected = 5 },
        testStruct{ .input = "10", .expected = 10 },
    };
    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testIntObj(evaluated, tt.expected);
    }
}

fn testEval(input: []const u8) !object.Object {
    const lex = lexer.Lexer.init(input);
    var p = try parser.Parser.init(lex, std.testing.allocator);
    defer p.deinit();
    const program = try p.parseProgram() orelse @panic("nulllllll found");
    const evaluated = try Eval(program);
    return evaluated;
}

fn testIntObj(obj: object.Object, expected: i64) !void {
    switch (obj) {
        .integer => |lint| {
            try std.testing.expectEqual(expected, lint.value);
        },
        else => {
            std.zig.fatal("Expected type {s} but got {}\n", .{ "integer", obj });
        },
    }
}

test "TestEvalBooleanExpression" {
    const testStruct = struct {
        input: []const u8,
        expected: bool,
    };
    const tests = [_]testStruct{
        testStruct{ .input = "true", .expected = true },
        testStruct{ .input = "false", .expected = false },
    };
    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testBoolObj(evaluated, tt.expected);
    }
}

fn testBoolObj(obj: object.Object, expected: bool) !void {
    switch (obj) {
        .boolean => |lbool| {
            try std.testing.expectEqual(expected, lbool.value);
        },
        else => {
            std.zig.fatal("Expected type {s} but got {}\n", .{ "bool", obj });
        },
    }
}

test "TestBangOperator" {
    const testStruct = struct {
        input: []const u8,
        expected: bool,
    };
    const tests = [_]testStruct{
        testStruct{ .input = "!true", .expected = false },
        testStruct{ .input = "!false", .expected = true },
        testStruct{ .input = "!5", .expected = false },
        testStruct{ .input = "!!5", .expected = true },
        testStruct{ .input = "!!true", .expected = true },
        testStruct{ .input = "!!false", .expected = false },
    };

    for (tests) |tt| {
        const evaluated = try testEval(tt.input);
        try testBoolObj(evaluated, tt.expected);
    }
}
