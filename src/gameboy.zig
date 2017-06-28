const Cpu = @import("cpu.zig").Cpu;
const Mem = @import("mem.zig").Mem;
const Rom = @import("rom.zig").Rom;

pub const Gameboy = struct {
    cpu: Cpu,
    mem: Mem,
    rom: &const Rom,

    // Io implementation which implements the following interface.
    //
    // io.init()
    // io.readKeys()
    // io.drawBuffer()
    // io.deinit()
    //
    // TODO: io should be part of the cpu controller.
    // io: &const Io,

    pub fn init(rom: &const Rom) -> Gameboy {
        var gb: Gameboy = undefined;
        gb.rom = rom;
        gb.mem = Mem.loadRom(gb.rom);
        gb.cpu = Cpu.init(&gb.mem);

        // Skip the power up sequence for the moment
        gb.cpu.reset();
        gb
    }
};
