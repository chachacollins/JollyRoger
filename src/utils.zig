const std = @import("std");
pub fn numToString(n: isize) ![]const u8 {
    var buf: [256]u8 = undefined;
    const str = try std.fmt.bufPrint(&buf, "{}", .{n});
    return str;
}
