const std = @import("std");
const lexer = @import("lexer.zig");
const token = @import("token.zig");

pub fn start(Allocator: std.mem.Allocator) !void {
    const prompt = ">>";
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("Aargh me hearty! Welcome to tha Jolly Roger V 0.0.1\n", .{});
    defer token.keywords.deinit();
    while (true) {
        try stdout.print("{s}", .{prompt});
        try bw.flush(); // don't forget to flush!

        const stdin_file = std.io.getStdIn().reader();
        var br = std.io.bufferedReader(stdin_file);
        const stdin = br.reader();
        const line = try stdin.readUntilDelimiterOrEofAlloc(Allocator, '\n', 4096) orelse "";
        var l = lexer.Lexer.init(line);
        for (line.len) |_| {
            const tok = try l.nextToken();
            std.debug.print("token type:{} token.literal {s}\n", .{ tok.Type, tok.Literal });
            if (tok.Type == token.TokenType.EOF) {
                break;
            }
        }
        defer Allocator.free(line);
    }
}
