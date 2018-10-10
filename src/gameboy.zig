const std = @import("std");
const time = std.os.time;

const Window = @import("window_sdl.zig").Window;
const irom = @import("rom.zig");
const immu = @import("mmu.zig");
const icpu = @import("cpu.zig");
const igpu = @import("gpu.zig");
const imbc = @import("mbc.zig");

pub const Gb = struct {
    mem: [0x10000]u8,
    window: Window,
    mmu: immu.Mmu,
    cpu: icpu.Z80,
    gpu: igpu.Gpu(Window),
    rom: irom.Rom,
    prng: std.rand.DefaultPrng,

    pub fn init(rom_binary: []u8) !Gb {
        var buf: [8]u8 = undefined;
        try std.os.getRandomBytes(buf[0..]);
        const seed = std.mem.readIntLE(u64, buf[0..8]);

        var gb: Gb = undefined;
        gb.rom = try irom.Rom.load(rom_binary);
        gb.mmu = immu.Mmu.init(gb.mem[0..], &gb.rom);
        gb.cpu = icpu.Z80.init(&gb.mmu);
        gb.gpu = igpu.Gpu(Window).init(&gb.mmu, &gb.window);

        gb.prng = std.rand.DefaultPrng.init(seed);
        gb.window = try Window.init(&gb.mmu);

        gb.rom.header.debugPrint();
        return gb;
    }

    pub fn deinit(gb: *Gb) void {
        gb.window.deinit();
    }

    pub fn run(gb: *Gb) void {
        // Having a specific boot mbc avoids an extra branch on every ROM/RAM access.
        const original_mbc = gb.mmu.mbc;
        gb.mmu.mbc = imbc.Mbc{ .Boot = imbc.MbcBoot.init() };

        // TODO: Handle boot rom write to ROM/RAM

        while (gb.cpu.r2[icpu.PC] != 0x100) {
            const frame_end_ticks = gb.cpu.ticks + igpu.frame_cycle_time;

            while (gb.cpu.total_ticks < frame_end_ticks) {
                const opcode = gb.cpu.read8(gb.cpu.r2[icpu.PC]);
                gb.cpu.r2[icpu.PC] +%= 1;
                const cycles = gb.cpu.step(opcode);
                gb.gpu.step(cycles);
            }

            gb.window.handleEvents() catch return;
            time.sleep(16 * time.millisecond); // time.ns_per_s * icpu.clock_speed / igpu.frame_cycle_time);
        }

        gb.mmu.mbc = original_mbc;

        gb.prng.random.bytes(gb.mmu.mem[256..0xe000]);
        std.mem.copy(u8, gb.mmu.mem[0xe000..0xfdff], gb.mmu.mem[0xc000..0xddff]);

        gb.cpu.r2[icpu.AF] = 0x01b0;
        gb.cpu.r2[icpu.BC] = 0x0013;
        gb.cpu.r2[icpu.DE] = 0x00d8;
        gb.cpu.r2[icpu.HL] = 0x014d;
        gb.cpu.r2[icpu.SP] = 0xfffe;

        const A = immu.addresses;
        gb.mmu.mem[A.TIMA] = 0x00;
        gb.mmu.mem[A.TMA_] = 0x00;
        gb.mmu.mem[A.TAC_] = 0x00;
        gb.mmu.mem[A.NR10] = 0x80;
        gb.mmu.mem[A.NR11] = 0xbf;
        gb.mmu.mem[A.NR12] = 0xf3;
        gb.mmu.mem[A.NR14] = 0xbf;
        gb.mmu.mem[A.NR21] = 0x3f;
        gb.mmu.mem[A.NR22] = 0x00;
        gb.mmu.mem[A.NR24] = 0xbf;
        gb.mmu.mem[A.NR30] = 0x7f;
        gb.mmu.mem[A.NR31] = 0xff;
        gb.mmu.mem[A.NR32] = 0x9f;
        gb.mmu.mem[A.NR33] = 0xbf;
        gb.mmu.mem[A.NR41] = 0xff;
        gb.mmu.mem[A.NR42] = 0x00;
        gb.mmu.mem[A.NR43] = 0x00;
        gb.mmu.mem[A.NR44] = 0xbf;
        gb.mmu.mem[A.NR50] = 0x77;
        gb.mmu.mem[A.NR51] = 0xf3;
        gb.mmu.mem[A.NR52] = 0xf1;
        gb.mmu.mem[A.LCDC] = 0x91;
        gb.mmu.mem[A.SCY_] = 0x00;
        gb.mmu.mem[A.SCX_] = 0x00;
        gb.mmu.mem[A.LYC_] = 0x00;
        gb.mmu.mem[A.BGP_] = 0xfc;
        gb.mmu.mem[A.OBP0] = 0xff;
        gb.mmu.mem[A.OBP1] = 0xff;
        gb.mmu.mem[A.WY__] = 0x00;
        gb.mmu.mem[A.WX__] = 0x00;
        gb.mmu.mem[A.IE__] = 0x00;

        while (true) {
            const frame_end_ticks = gb.cpu.ticks + igpu.frame_cycle_time;

            while (gb.cpu.total_ticks < frame_end_ticks) {
                const opcode = gb.cpu.read8(gb.cpu.r2[icpu.PC]);
                gb.cpu.r2[icpu.PC] +%= 1;
                const cycles = gb.cpu.step(opcode);
                gb.gpu.step(cycles);
            }

            gb.window.handleEvents() catch return;
            time.sleep(16 * time.millisecond); //time.ns_per_s * icpu.clock_speed / igpu.frame_cycle_time);
        }
    }
};
