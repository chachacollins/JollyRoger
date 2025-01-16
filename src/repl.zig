const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");

pub fn start(Allocator: std.mem.Allocator) !void {
    const prompt = ">>";
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("Aargh me hearty! Welcome to tha Jolly Roger V 0.0.1\n", .{});
    try bw.flush(); 
    defer token.keywords.deinit();
    while (true) {
        try stdout.print("{s}", .{prompt});
        try bw.flush(); 
        const stdin_file = std.io.getStdIn().reader();
        var br = std.io.bufferedReader(stdin_file);
        const stdin = br.reader();
        const line = try stdin.readUntilDelimiterOrEofAlloc(Allocator, '\n', 4096) orelse "";
        defer Allocator.free(line);
        const l = lexer.Lexer.init(line);
        var p = try parser.Parser.init(l, Allocator);
        defer p.deinit();
        const program = try p.parseProgram() orelse @panic("null found");
        const evaluated = try evaluator.Eval(program);
        try stdout.print("{s}", .{try evaluated.Inspect(Allocator)});
        try stdout.print("\n", .{});
        try bw.flush(); 
    }
}
