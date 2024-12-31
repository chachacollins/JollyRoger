const std = @import("std");
pub fn charToString(n: u8) ![]u8 {
    const str = [1]u8{n};
    return &str;
}
