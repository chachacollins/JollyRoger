const std = @import("std");
const lexer = @import("lexer.zig");
const ut = @import("utils.zig");

pub fn main() !void {
    const char = 'A';
    const not_printable = &[_]u8{char};
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("Hello {s} not_printable{s}\n", .{ not_printable, not_printable });
    try bw.flush(); // don't forget to flush!
    if (std.mem.eql(u8, "{", "{")) {
        std.debug.print("thes two are equal", .{});
    }
}
