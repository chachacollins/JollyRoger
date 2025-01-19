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
    pub fn Inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            inline else => |case| return try case.Inspect(allocator),
        }
    }
    pub fn typeOf(self: Object) ObjectType {
        switch (self) {
            inline else => |case| return case.typeOf(), // Removed 'try'
        }
    }
};

pub const Integer = struct {
    value: i64,
    pub fn Inspect(self: Integer, allocator: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }
    pub fn typeOf(self: Integer) ObjectType {
        _ = self;
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
    pub fn typeOf(self: Boolean) ObjectType {
        _ = self;
        return ObjectType.BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn Inspect(self: Null, allocator: std.mem.Allocator) ![]const u8 {
        _ = self;
        return allocator.dupe(u8, "null");
    }
    pub fn typeOf(self: Null) ObjectType {
        _ = self;
        return ObjectType.NULL_OBJ;
    }
};
