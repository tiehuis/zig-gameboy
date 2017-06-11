const std = @import("std");
const Rom = @import("rom.zig").Rom;
const Cpu = @import("cpu.zig").Cpu;
const Mem = @import("mem.zig").Mem;
const printf = std.io.stdout.printf;
const readByte = std.io.stdin.readByte;

const want_step = false;

fn clearScreen() -> %void {
    if (want_step) {
        %return printf("{c}[2J{c}[H", u8(0x1B), u8(0x1B));
    }
}

fn stepWait() -> %void {
    if (want_step) {
        _ = %%readByte();
    }
}

pub fn main() -> %void {
    const rom = Rom.load() %% |err| {
        %%printf("could not load rom: {}\n", err);
        return;
    };

    %%clearScreen();
    %%printf("<== rom details ==>\n");
    %%rom.header.debugPrint();
    %%stepWait();

    var cpu = Cpu.init();
    cpu.mem = Mem.loadRom(&rom);

    // Skip the power up sequence for the moment
    cpu.reset();

    while (true) {
        %%clearScreen();
        cpu.step();

        // Allow stepping through and verifying registers
        if (want_step) {
            %%cpu.debugPrint();
        }
        %%stepWait();
    }
}
