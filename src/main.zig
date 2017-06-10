const std = @import("std");
const Rom = @import("rom.zig").Rom;
const Cpu = @import("cpu.zig").Cpu;
const Mem = @import("mem.zig").Mem;
const printf = std.io.stdout.printf;

pub fn main() -> %void {
    const rom = Rom.load() %% |err| {
        %%printf("could not load rom: {}\n", err);
        return;
    };
    %%printf("<== rom details ==>\n");
    %%rom.header.debugPrint();

    var cpu = Cpu.init();
    cpu.mem = Mem.loadRom(&rom);

    while (true) {
        cpu.step();
    }
}
