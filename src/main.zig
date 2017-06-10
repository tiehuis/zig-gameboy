const std = @import("std");
const Rom = @import("rom.zig").Rom;

pub fn main() -> %void {
    const s = Rom.load() %% |err| {
        %%std.io.stdout.printf("could not load rom: {}\n", err);
        return;
    };

    %%s.header.debugPrint();
}
