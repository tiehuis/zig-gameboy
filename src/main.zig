const std = @import("std");
const printf = std.io.stdout.printf;
const readByte = std.io.stdin.readByte;
const Rom = @import("rom.zig").Rom;
const Gameboy = @import("gameboy.zig").Gameboy;

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

    var gb = Gameboy.init(&rom);

    while (true) {
        %%clearScreen();
        gb.cpu.step();

        // Allow stepping through and verifying registers
        if (want_step) {
            %%gb.cpu.debugPrint();
        }
        %%stepWait();
    }
}
