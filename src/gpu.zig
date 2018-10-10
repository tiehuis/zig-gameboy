const std = @import("std");
const immu = @import("mmu.zig");
const Mmu = immu.Mmu;
const A = immu.addresses;

const meta = @import("meta.zig");

// number of cycles to scan and vblank
pub const frame_cycle_time = 70224;

pub fn Gpu(comptime Window: type) type {
    comptime {
        std.debug.assert(meta.hasField(Window, "pixels"));
        std.debug.assert(meta.hasFunction(Window, "init"));
        std.debug.assert(meta.hasFunction(Window, "render"));
    }

    return struct {
        const Self = @This();

        window: *Window,
        ticks: usize,

        // NOTE: We bypass the mmu in most cases since we don't have the same R/W restrictions on
        // various registers that the mmu imposes.
        mmu: *Mmu,

        pub fn init(mmu: *Mmu, window: *Window) Self {
            return Self{
                .window = window,
                .ticks = 0,
                .mmu = mmu,
            };
        }

        // swizzle(abcdefgh, ABCDEFGH) = aAbBcCdDeEfFgGhH
        fn swizzle(x: u8, y: u8) u16 {
            return @truncate(u16, ((u64(x) *% 0x0101010101010101 & 0x8040201008040201) *%
                0x0102040810204081 >> 49) & 0x5555 |
                ((u64(y) *% 0x0101010101010101 & 0x8040201008040201) *%
                0x0102040810204081 >> 48) & 0xAAAA);
        }

        // NOTE: We could draw the entire frame at once since we don't blit until the end but this
        // is more authentic compared to how the screen is actually drawn in hardware.
        fn renderScanLine(g: *Self) void {
            // We have two tile-maps. Use the correct one and offset by SCY scanlines.
            const map_offset = ((g.mmu.mem[A.LY__] +% g.mmu.mem[A.SCY_]) >> 3) + if (g.mmu.mem[A.LCDC] & 0x08 != 0) u16(0x1c00) else 0x1800;
            var line_offset = g.mmu.mem[A.SCX_] >> 3;

            const y = (g.mmu.mem[A.LY__] +% g.mmu.mem[A.SCY_]) & 3;
            var x = g.mmu.mem[A.SCX_] & 7;

            // Look up the palette order from BGP and display
            const palette = [4][3]u8{
                []u8{ 0xff, 0xff, 0xff }, // White
                []u8{ 0xaa, 0xaa, 0xaa }, // Light gray
                []u8{ 0x55, 0x55, 0x55 }, // Dark gray
                []u8{ 0x00, 0x00, 0x00 }, // Black
            };

            // TODO: Can LCDC be modified during the write to switch tiles mid-scroll.
            const tile_offset = x % 8;

            if (tile_offset != 0) {
                var tile: u16 = g.mmu.vram[map_offset + line_offset];
                if (g.mmu.mem[A.LCDC] & 0x04 != 0 and tile < 128) tile += 256;
                line_offset = (line_offset + 1) & 31;

                const b = swizzle(g.mmu.read(2 * tile), g.mmu.read(2 * tile + 1));

                var px: usize = tile_offset;
                while (px < 8) : (px += 1) {
                    const shift = @truncate(u3, b >> @intCast(u4, 2 * px));
                    const c = palette[(g.mmu.mem[A.BGP_] >> shift) & 3];
                    g.window.pixels[y][x] = (u32(c[0]) << 24) | (u32(c[1]) << 16) | (u32(c[2]) << 8);
                }
            }

            var i: usize = 0;
            while (i < 160 - tile_offset) : (i += 8) {
                var tile: u16 = g.mmu.vram[map_offset + line_offset];
                if (g.mmu.mem[A.LCDC] & 0x04 != 0 and tile < 128) tile += 256;
                line_offset = (line_offset + 1) & 31;

                const b = swizzle(g.mmu.read(2 * tile), g.mmu.read(2 * tile + 1));

                var px: usize = 0;
                while (px < 8) : (px += 1) {
                    const shift = @truncate(u3, b >> @intCast(u4, 2 * px));
                    const c = palette[(g.mmu.mem[A.BGP_] >> shift) & 3];
                    g.window.pixels[y][x] = (u32(c[0]) << 24) | (u32(c[1]) << 16) | (u32(c[2]) << 8);
                }
            }

            if (tile_offset != 0) {
                var tile: u16 = g.mmu.vram[map_offset + line_offset];
                if (g.mmu.mem[A.LCDC] & 0x04 != 0 and tile < 128) tile += 256;
                line_offset = (line_offset + 1) & 31;

                const b = swizzle(g.mmu.read(2 * tile), g.mmu.read(2 * tile + 1));

                var px: usize = 0;
                while (px < 8 - tile_offset) : (px += 1) {
                    const shift = @truncate(u3, b >> @intCast(u4, 2 * px));
                    const c = palette[(g.mmu.mem[A.BGP_] >> shift) & 3];
                    g.window.pixels[y][x] = (u32(c[0]) << 24) | (u32(c[1]) << 16) | (u32(c[2]) << 8);
                }
            }
        }

        pub fn step(g: *Self, cycles: usize) void {
            g.ticks += cycles;

            const mode = @truncate(u2, g.mmu.mem[A.STAT]);
            switch (mode) {
                // The LCD controller is in the H-Blank period and
                // the CPU can access both the display RAM (8000h-9FFFh)
                // and OAM (FE00h-FE9Fh)
                0 => {
                    if (g.ticks >= 80) {
                        g.ticks -= 80;
                        g.mmu.mem[A.STAT] = ~u8(0b11);
                        g.mmu.mem[A.STAT] |= 0b11;
                    }
                },

                // The LCD controller is in the V-Blank period (or the
                // display is disabled) and the CPU can access both the
                // display RAM (8000h-9FFFh) and OAM (FE00h-FE9Fh)
                1 => {
                    if (g.ticks >= 172) {
                        g.ticks -= 172;
                        g.mmu.mem[A.STAT] = ~u8(0b11);

                        g.renderScanLine();
                    }
                },

                // The LCD controller is reading from OAM memory.
                // The CPU <cannot> access OAM memory (FE00h-FE9Fh)
                // during this period.
                2 => {
                    if (g.ticks >= 204) {
                        g.ticks -= 204;
                        g.mmu.mem[A.LY__] += 1;

                        if (g.mmu.mem[A.LY__] == 143) {
                            g.mmu.mem[A.STAT] = ~u8(0b11);
                            g.mmu.mem[A.STAT] |= 1;
                            g.window.render();
                        } else {
                            g.mmu.mem[A.STAT] = ~u8(0b11);
                            g.mmu.mem[A.STAT] |= 2;
                        }
                    }
                },

                // The LCD controller is reading from both OAM and VRAM,
                // The CPU <cannot> access OAM and VRAM during this period.
                // CGB Mode: Cannot access Palette Data (FF69,FF6B) either.
                3 => {
                    if (g.ticks >= 456) {
                        g.ticks -= 456;
                        g.mmu.mem[A.LY__] += 1;

                        if (g.mmu.mem[A.LY__] > 153) {
                            g.mmu.mem[A.STAT] = ~u8(0b11);
                            g.mmu.mem[A.STAT] |= 2;
                            g.mmu.mem[A.LY__] = 0;
                        }
                    }
                },
            }
        }
    };
}
