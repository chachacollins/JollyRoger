const std = @import("std");

pub const Object = struct {
    ptr: *anyopaque,
    vtable: *const Vtable,

    pub const Vtable = struct {
        Type: *const fn () ObjectType,
        Inspect: *const fn (ctx: *anyopaque, allocator: std.mem.Allocator) anyerror![]const u8,
    };

    pub fn Type(self: Object) ObjectType {
        return self.vtable.Type();
    }

    pub fn Inspect(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        return self.vtable.Inspect(self.ptr, allocator);
    }
};

const ObjectType = enum { INTEGER_OBJ };

pub const Integer = struct {
    value: i64,

    pub fn Type() ObjectType {
        return ObjectType.INTEGER_OBJ;
    }

    pub fn Inspect(ctx: *anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        const self: *Integer = @ptrCast(@alignCast(ctx));
        return std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    const vtable: Object.Vtable = .{
        .Type = Type,
        .Inspect = Inspect,
    };

    pub fn Obj(self: *Integer) Object {
        return .{ .ptr = self, .vtable = &vtable };
    }
};

test "TestOBJ" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var testInt = Integer{ .value = 69 };
    const testObj = testInt.Obj();
    
    try std.testing.expectEqual(testObj.Type(), ObjectType.INTEGER_OBJ);
    
    const inspected = try testObj.Inspect(allocator);
    try std.testing.expectEqualStrings("69", inspected);
}
