const std = @import("std");
const Rom = @import("rom.zig").Rom;

pub const Mbc = union(enum) {
    Boot: MbcBoot,
    None: MbcNone,

    pub fn init(rom: *Rom) Mbc {
        switch (rom.header.cartridge_type) {
            0x00 => return Mbc{ .None = MbcNone.init(rom.content) },
            else => unreachable,
        }
    }

    pub fn read(mbc: *Mbc, address: u16) u8 {
        return switch (mbc.*) {
            Mbc.Boot => |*m| m.read(address),
            Mbc.None => |*m| m.read(address),
        };
    }

    pub fn write(mbc: *Mbc, address: u16, value: u8) void {
        return switch (mbc.*) {
            Mbc.Boot => |*m| m.write(address, value),
            Mbc.None => |*m| m.write(address, value),
        };
    }
};

pub const MbcBoot = struct {
    const boot_rom = [256]u8{
        0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
        0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
        0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
        0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
        0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
        0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
        0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
        0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
        0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
        0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
        0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
        0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
        0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
        0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
        0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
        0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50,
    };

    pub fn init() MbcBoot {
        return MbcBoot{};
    }

    pub fn read(mbc: *MbcBoot, address: u16) u8 {
        switch (address) {
            0x0000...0x0100 => {
                return boot_rom[address];
            },

            else => {
                unreachable;
            },
        }
    }

    pub fn write(mbc: *MbcBoot, address: u16, value: u8) void {
        switch (address) {
            0x0000...0x0100 => {
                // read-only
            },

            else => {
                unreachable;
            },
        }
    }
};

pub const MbcNone = struct {
    rom: []u8,

    pub fn init(rom: []u8) MbcNone {
        return MbcNone{ .rom = rom };
    }

    pub fn read(mbc: *MbcNone, address: u16) u8 {
        switch (address) {
            // 32Kb ROM
            0x0000...0x7FFF => {
                return mbc.rom[address];
            },

            // 8Kb RAM
            0xA000...0xBFFF => {
                return mbc.rom[address];
            },

            else => {
                unreachable;
            },
        }
    }

    pub fn write(mbc: *MbcNone, address: u16, value: u8) void {
        switch (address) {
            // 32Kb ROM
            0x0000...0x7FFF => {
                // read-only
            },

            // 8Kb RAM
            0xA000...0xBFFF => {
                mbc.rom[address] = value;
            },

            else => {
                unreachable;
            },
        }
    }
};
