const std = @import("std");
const Rom = @import("rom.zig").Rom;

pub const Mem = struct {
    memory: [0x10000]u8,
    bank0: []u8,
    bank1: []u8,
    vram:  []u8,
    ram:   []u8,
    wram0: []u8,
    wram1: []u8,
    echo:  []u8,
    oam:   []u8,
    io:    []u8,
    hram:  []u8,

    pub fn loadRom(rom: &const Rom) -> Mem {
        var mem: Mem = undefined;
        mem.memory = undefined;

        // TODO: Can we fix these on declaration instead since they are constant?
        mem.bank0 = mem.memory[0x0000 .. 0x3FFF];
        mem.bank1 = mem.memory[0x4000 .. 0x7FFF];
        mem.vram  = mem.memory[0x8000 .. 0x9FFF];
        mem.ram   = mem.memory[0xA000 .. 0xBFFF];
        mem.wram0 = mem.memory[0xC000 .. 0xCFFF];
        mem.wram1 = mem.memory[0xD000 .. 0xDFFF];
        mem.echo  = mem.memory[0xE000 .. 0xFDFF];
        mem.oam   = mem.memory[0xFE00 .. 0xFE9F];
        mem.io    = mem.memory[0xFF00 .. 0xFF7F];
        mem.hram  = mem.memory[0xFF80 .. 0xFFFE];

        // Only handle the 32Kb Rom case now.
        std.debug.assert(rom.header.rom_size == 0x0);
        std.mem.copy(u8, mem.memory[0 .. 0x8000], rom.content);

        mem
    }

    pub fn copy(mem: &Mem, dst_addr: u16, src_addr: u16, len: usize) {
        var i: usize = 0;
        while (i < len) : (i += 1) {
            const m = mem.read8(src_addr + i);
            mem.write8(dst_addr + i, m);
        }
    }

    pub fn read8(mem: &const Mem, address: u16) -> u8 {
        mem.memory[address]
    }

    pub fn read16(mem: &const Mem, address: u16) -> u16 {
        u16(mem.read8(address)) | (u16(mem.read8(address + 1)) << 8)
    }

    pub fn write8(mem: &Mem, address: u16, value: u8) {
        switch (address) {
            0x0000 ... 0x7FFF => {
                // read-only segments
                //
                // NOTE: When an MBC is present, portions of this address space become writable and
                // and modify rom/ram banking options.
            },

            // First 1K of WRAM echos to ECHO RAM and vice-versa
            0xC000 ... 0xDDFF => {
                mem.memory[address] = value;
                mem.memory[address + 0x2000] = value;
            },

            0xE000 ... 0xFDFF => {
                mem.memory[address] = value;
                mem.memory[address - 0x2000] = value;
            },

            0xFF00 => {
                mem.memory[address] = value & 0xF0;
            },

            0xFF04 => {
                mem.memory[address] = 0;
            },

            else => {
                mem.memory[address] = value;
            },
        }
    }

    pub fn write16(mem: &Mem, address: u16, value: u16) {
        mem.write8(address, u8(value & 0xFF));
        mem.write8(address + 1, u8(value >> 8));
    }
};

