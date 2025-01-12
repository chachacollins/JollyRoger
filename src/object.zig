const std = @import("std");

pub const ObjectType = enum {
    INTEGER_OBJ,
    BOOLEAN_OBJ,
    NULL_OBJ,
};

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null_: Null,
    pub fn Inspect(self: Object) ![]const u8 {
        switch (self) {
            inline else => |case| return try case.Inspect(),
        }
    }
    pub fn typeOf(self: Object) ObjectType {
        switch (self) {
            inline else => |case| return try case.typeOf(),
        }
    }
};

pub const Integer = struct {
    value: i64,
    pub fn Inspect(self: Integer, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }
    pub fn typeOf() ObjectType {
        return ObjectType.INTEGER_OBJ;
    }
};
pub const Boolean = struct {
    value: bool,

    pub fn Inspect(self: Boolean, allocator: std.mem.Allocator) ![]const u8 {
        if (self.value) {
            return allocator.dupe(u8, "true");
        } else {
            return allocator.dupe(u8, "false");
        }
    }
    pub fn typeOf() ObjectType {
        return ObjectType.BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn Inspect(self: Null, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return allocator.dupe(u8, "null");
    }
    pub fn typeOf() ObjectType {
        return ObjectType.NULL_OBJ;
    }
};
