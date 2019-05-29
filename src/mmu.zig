const std = @import("std");
const Rom = @import("rom.zig").Rom;
const Mbc = @import("mbc.zig").Mbc;

pub const Mmu = struct {
    mem: []u8,
    mbc: Mbc,

    bank0: []u8,
    bank1: []u8,
    vram: []u8,
    ram: []u8,
    wram0: []u8,
    wram1: []u8,
    echo: []u8,
    oam: []u8,
    io: []u8,
    hram: []u8,

    // The JOYP circuitry can be physically switched via a register write to switch between
    // diretional or other buttons. We need an extra u8 to handle this in software.
    joyp_bit: [2]u8,
    joyp_active: u1,

    pub fn init(mem: []u8, rom: *Rom) Mmu {
        var mmu: Mmu = undefined;
        mmu.mem = mem;
        mmu.mbc = Mbc.init(rom);

        mmu.bank0 = mmu.mem[0x0000..0x3FFF];
        mmu.bank1 = mmu.mem[0x4000..0x7FFF];
        mmu.vram = mmu.mem[0x8000..0x9FFF];
        mmu.ram = mmu.mem[0xA000..0xBFFF];
        mmu.wram0 = mmu.mem[0xC000..0xCFFF];
        mmu.wram1 = mmu.mem[0xD000..0xDFFF];
        mmu.echo = mmu.mem[0xE000..0xFDFF];
        mmu.oam = mmu.mem[0xFE00..0xFE9F];
        mmu.io = mmu.mem[0xFF00..0xFF7F];
        mmu.hram = mmu.mem[0xFF80..0xFFFE];

        mmu.joyp_active = 0; // bit 4

        std.mem.copy(u8, mmu.mem[0..0x8000], rom.content);
        return mmu;
    }

    pub fn read(self: *Mmu, address: u16) u8 {
        switch (address) {
            // ROM/RAM banks
            0x0000...0x7FFF, 0xA000...0xBFFF => {
                return self.mbc.read(address);
            },

            0x8000...0x9FFF => {
                // VRAM is accessible during Mode 0-2
                const mode = @truncate(u2, self.mem[addresses.STAT]);
                switch (mode) {
                    0, 1, 2 => {
                        return self.mem[address];
                    },
                    3 => {
                        return 0xFF; // undefined
                    },
                }
            },

            0xFE00...0xFE9F => {
                // OAM is accessible during Mode 0-1
                const mode = @truncate(u2, self.mem[addresses.STAT]);
                switch (mode) {
                    0, 1 => {
                        return self.mem[address];
                    },
                    2, 3 => {
                        return 0xFF; // undefined
                    },
                }
            },

            else => {
                return self.mem[address];
            },
        }
    }

    pub fn write(self: *Mmu, address: u16, value: u8) void {
        switch (address) {
            // ROM/RAM banks
            0x0000...0x7FFF, 0xA000...0xBFFF => {
                return self.mbc.write(address, value);
            },

            // First 1K of WRAM echos to ECHO RAM and vice-versa
            0xC000...0xDDFF => {
                self.mem[address] = value;
                self.mem[address + 0x2000] = value;
            },

            0x8000...0x9FFF => {
                // VRAM is accessible during Mode 0-2
                const mode = @truncate(u2, self.mem[addresses.STAT]);
                switch (mode) {
                    0, 1, 2 => {
                        self.mem[address] = value;
                    },
                    3 => {},
                }
            },

            0xE000...0xFDFF => {
                self.mem[address] = value;
                self.mem[address - 0x2000] = value;
            },

            0xFE00...0xFE9F => {
                // OAM is accessible during Mode 0-1
                const mode = @truncate(u2, self.mem[addresses.STAT]);
                switch (mode) {
                    0, 1 => {
                        self.mem[address] = value;
                    },
                    2, 3 => {},
                }
            },

            addresses.JOYP => {
                switch (value) {
                    0x10 => {
                        self.joyp_active = 0;
                        self.mem[addresses.JOYP] = self.joyp_bit[0];
                    },
                    0x20 => {
                        self.joyp_active = 1;
                        self.mem[addresses.JOYP] = self.joyp_bit[1];
                    },
                    else => {},
                }
            },

            0xFF01 => {
                self.mem[address] = value & 0xF0;
            },

            0xFF04 => {
                self.mem[address] = 0;
            },

            else => {
                self.mem[address] = value;
            },
        }
    }
};

pub const addresses = struct {
    // Video Display
    pub const LCDC = 0xff40; // LCD Control Register (R/W)
    pub const STAT = 0xff41; // LCD Status Register (R/W)
    pub const SCY_ = 0xff42; // LCD Scroll Y (R)
    pub const SCX_ = 0xff43; // LCD Scroll X (R/W)
    pub const LY__ = 0xff44; // LCDC Y-Coordinate (R/W)
    pub const LYC_ = 0xff45; // LY Compare (R/W)
    pub const WY__ = 0xff4a; // Window Y Position (R/W)
    pub const WX__ = 0xff4b; // Window X Position (R/W)
    pub const BGP_ = 0xff47; // BG Palette Data (R/W)
    pub const OBP0 = 0xff48; // Object Palette 0 Data
    pub const OBP1 = 0xff48; // Object Palette 1 Data
    pub const DMA_ = 0xff46; // DMA Transfer and Start Address (R/W)

    // Sound Controller
    pub const NR10 = 0xff10; // CH1 Sweep Register (R/W)
    pub const NR11 = 0xff10; // CH1 Sound length/Wave pattern duty (R/w)
    pub const NR12 = 0xff10; // CH1 Volume Envelope (R/W)
    pub const NR13 = 0xff10; //CH1 Frequency lo (W)
    pub const NR14 = 0xff10; // CH1 Frequency hi (R/W)

    pub const NR21 = 0xff10; // CH2 Sound Length/Wave Pattern Duty (R/W)
    pub const NR22 = 0xff10; // CH2 Volume Envelope (R/W)
    pub const NR23 = 0xff10; // CH2 Frequency lo (W)
    pub const NR24 = 0xff10; // CH2 Frequency hi (R/W)

    pub const NR30 = 0xff10; // CH3 Sound on/off (R/W)
    pub const NR31 = 0xff10; // CH3 Sound Length
    pub const NR32 = 0xff10; // CH3 Select output level (R/W)
    pub const NR33 = 0xff10; // CH3 Frequency lo (W)
    pub const NR34 = 0xff10; // CH3 Frequency hi (R/W)

    pub const NR41 = 0xff10; // CH4 Sound Length (R/W)
    pub const NR42 = 0xff10; // CH4 Volume Envelope (R/W)
    pub const NR43 = 0xff10; // CH4 Polynomial Counter (R/W)
    pub const NR44 = 0xff10; // CH4 Counter/consecutive; Initial (R/W)

    pub const NR50 = 0xff10; // Channel control/ON-OFF/Volume (R/W)
    pub const NR51 = 0xff10; // Selection of Sound output terminal (R/W)
    pub const NR52 = 0xff10; // Sound on/off

    // Joypad Input
    pub const P1__ = 0xff00; // Joypad (R/W)
    pub const JOYP = P1__;

    // Serial Data Transfer (Link Cable)
    pub const SB__ = 0xff01; // Serial Transfer Data (R/W)
    pub const SC__ = 0xff02; // Serial Transfer Control (R/W)

    // Timer and Divider Registers
    pub const DIV_ = 0xff04; // Divider Register (R/W)
    pub const TIMA = 0xff05; // Timer Counter (R/W)
    pub const TMA_ = 0xff06; // Timer Modulo (R/W)
    pub const TAC_ = 0xff07; // Timer Control (R/W)

    // Interrupts
    pub const IE__ = 0xffff; // Interrupt Enable (R/W)
    pub const IF__ = 0xff0f; // Interrupt Flag (R/W)
};
