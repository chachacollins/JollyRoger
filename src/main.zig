const std = @import("std");
const lexer = @import("lexer.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("Hello world\n", .{});
    try bw.flush(); // don't forget to flush!

    lexer.hello();
}
