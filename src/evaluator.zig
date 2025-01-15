const std = @import("std");
const object = @import("object.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn Eval(node: ast.Program) !object.Object {
    switch (node.statements.items[0]) {
        .expressionStatement => |exp| {
            switch (exp.expression) {
                .integerLiteral => {
                    const int_obj = object.Integer{ .value = exp.expression.integerLiteral.value };
                    return object.Object{ .integer = int_obj };
                },
                else => {},
            }
        },
        else => {
            std.zig.fatal("Got some other type {}\n", .{node});
        },
    }
    return object.Object{ .null_ = .{} };
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
