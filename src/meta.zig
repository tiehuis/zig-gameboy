const std = @import("std");
const builtin = @import("builtin");

pub fn hasFunction(comptime T: type, name: []const u8) bool {
    const info = @typeInfo(T);
    for (info.Struct.defs) |def| {
        const DataType = @TagType(builtin.TypeInfo.Definition.Data);

        if (DataType(def.data) != DataType.Fn) {
            continue;
        }

        if (!std.mem.eql(u8, def.name, name)) {
            continue;
        }

        return true;
    }

    return false;
}

pub fn hasField(comptime T: type, comptime name: []const u8) bool {
    const info = @typeInfo(T);
    const fields = switch (info) {
        builtin.TypeId.Struct => |s| s.fields,
        builtin.TypeId.Union => |u| u.fields,
        builtin.TypeId.Enum => |e| e.fields,
        else => return false,
    };

    for (fields) |field| {
        if (std.mem.eql(u8, field.name, name)) {
            return true;
        }
    }

    return false;
}
