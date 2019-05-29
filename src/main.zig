const std = @import("std");
const Gameboy = @import("gameboy.zig").Gb;

pub fn main() !void {
    var direct = std.heap.DirectAllocator.init();
    var allocator = &direct.allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.warn("gameboy <rom>\n");
        std.os.exit(1);
    }

    const rom = try std.io.readFileAlloc(allocator, args[1]);
    defer allocator.free(rom);

    var gb = try Gameboy.init(rom);
    defer gb.deinit();

    gb.run();
}
