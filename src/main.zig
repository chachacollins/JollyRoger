const std = @import("std");
const lexer = @import("lexer.zig");
const repl = @import("repl.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const ds = gpa.deinit();
        if (ds == .leak) {
            std.log.err("memory leak", .{});
        }
    }
    const allocator = gpa.allocator();
    try repl.start(allocator);
}
