error InvalidRomSize;
error UnknownCartridgeType;
error UnknownRomSize;
error UnknownRamSize;
error UnknownDestinationCode;

// Would load this into the actual Rom structure and keep it as a fixed size. No allocation.
const tetris_rom = @embedFile("tetris.rom");

const rom_header_begin = 0x0100;
const rom_header_end   = 0x014F;
// comptime { assert(@sizeOf(RomHeader) == rom_header_end - rom_header_begin); }

pub const RomHeader = packed struct {
    // 0x0100 .. 0x0103
    entry_point: [4]u8,

    // 0x0104 .. 0x0133
    nintendo_logo: [48]u8,

    // 0x0134 .. 0x0143
    title: [16]u8,

    // 0x0143
    cgb_flag: u8,

    // 0x0144 .. 0x0145
    new_licensee_code: [2]u8,

    // 0x0146
    sgb_flag: u8,

    // 0x0147
    cartridge_type: u8,

    // 0x0148
    rom_size: u8,

    // 0x0149
    ram_size: u8,

    // 0x014A
    destination_code: u8,

    // 0x014B
    old_licensee_code: u8,

    // 0x014C
    mask_rom_version_number: u8,

    // 0x014D
    header_checksum: u8,

    // 0x014E .. 0x014F
    global_checksum: [2]u8,

    pub fn debugPrint(header: &const RomHeader) -> %void {
        const printf = @import("std").io.stdout.printf;

        // NOTE: Evaluation exceeded 1000 backwards branches if all one printf.
        %return printf(
            \\entry point               : {}
            \\title                     : {}
            \\cgb_flag                  : {} = {}
            \\new_licensee_code         : {}
            \\sgb_flag                  : {} = {}
            \\cartridge_type            : {} = {}
            \\
            , header.entry_point
            , header.title
            , header.cgb_flag , Format.cgbFlag(header.cgb_flag)
            , header.new_licensee_code
            , header.sgb_flag , Format.sgbFlag(header.sgb_flag)
            , header.cartridge_type , Format.cartridgeType(header.cartridge_type)
        );

        %return printf(
            \\rom_size                  : {} = {}
            \\ram_size                  : {} = {}
            \\destination code          : {} = {}
            \\old_licensee code         : {}
            \\mask_rom_version_number   : {}
            \\header checksum           : {}
            \\global checksum           : {}
            \\
            , header.rom_size , Format.romSize(header.rom_size)
            , header.ram_size , Format.ramSize(header.ram_size)
            , header.destination_code , Format.destinationCode(header.destination_code)
            , header.old_licensee_code
            , header.mask_rom_version_number
            , header.header_checksum
            , header.global_checksum
        );
    }
};

pub const Rom = struct {
    // Maximum size this can be is 4Mb (256 banks)
    content: []const u8,

    header: &const RomHeader,

    pub fn load() -> %Rom {
        var rom: Rom = undefined;
        rom.content = tetris_rom;
        rom.header = @ptrCast(&const RomHeader, @ptrCast(&u8, &rom.content[rom_header_begin]));
        %return verifyRom(&rom);
        rom
    }

    fn verifyRom(rom: &Rom) -> %void {
        // Only handle ROM ONLY cartridges
        if (rom.header.cartridge_type != 0x00) {
            return error.UnknownCartridgeType;
        }

        // Only handle 32Kb ROM size only
        if (rom.header.rom_size != 0x00) {
            return error.UnknownRomSize;
        }

        if (rom.content.len != 32 * 1024) {
            return error.InvalidRomSize;
        }
    }
};

const Format = struct {
    fn cgbFlag(value: u8) -> %[]const u8 {
        switch (value) {
            0x80 => "CGB plus old gameboys",
            0xC0 => "CGB only",
            else => "Part of Title",
        }
    }

    fn sgbFlag(value: u8) -> %[]const u8 {
        switch (value) {
            0x03 => "SGB support",
            else => "No SGB support",
        }
    }

    fn cartridgeType(value: u8) -> %[]const u8 {
        switch (value) {
            0x00 => "ROM ONLY",
            0x01 => "MBC1",
            0x02 => "MBC1+RAM",
            0x03 => "MBC1+RAM+BATTERY",
            0x05 => "MBC2",
            0x06 => "MBC2+BATTERY",
            0x08 => "ROM+RAM",
            0x09 => "ROM+RAM+BATTERY",
            0x0B => "MMM01",
            0x0C => "MMM01+SRAM",
            0x0D => "MMM01+SRAM+BATTERY",
            0x0F => "MBC3+TIMER+BATTERY",
            0x10 => "MBC3+TIMER+RAM+BATTERY",
            0x11 => "MBC3",
            0x12 => "MBC3+RAM",
            0x13 => "MBC3+RAM+BATTERY",
            0x15 => "MBC4",
            0x16 => "MBC4+RAM",
            0x17 => "MBC14+RAM+BATTERY",
            0x19 => "MBC5",
            0x1A => "MBC5+RAM",
            0x1B => "MBC5+RAM+BATTERY",
            0x1C => "MBC5+RUMBLE",
            0x1D => "MBC5+RUMBLE+RAM",
            0x1E => "MBC5+RUMBLE+RAM+BATTERY",
            0x1F => "POCKET CAMERA",
            0xFD => "BANDAI TAMA5",
            0xFE => "HuC3",
            0xFF => "HuC1+RAM+BATTERY",
            else => return error.UnknownCartridgeType,
        }
    }

    fn romSize(value: u8) -> %[]const u8 {
        switch (value) {
            0x00 => "32Kb (no rom banks)",
            0x01 => "64Kb (4 banks)",
            0x02 => "128Kb (8 banks)",
            0x03 => "256Kb (16 banks)",
            0x04 => "512Kb (32 banks)",
            0x05 => "1Mb (64 banks)",
            0x06 => "2Mb (128 banks)",
            0x07 => "4Mb (256 banks)",
            0x52 => "1.1Mb (72 banks)",
            0x53 => "1.2Mb (80 banks)",
            0x54 => "1.5Mb (96 banks)",
            else => return error.UnknownRomSize,
        }
    }

    fn ramSize(value: u8) -> %[]const u8 {
        switch (value) {
            0x00 => "None",
            0x01 => "2Kb",
            0x02 => "8Kb",
            0x03 => "32Kb",
            else => return error.UnknownRamSize,
        }
    }

    fn destinationCode(value: u8) -> %[]const u8 {
        switch (value) {
            0x00 => "Japanese",
            0x01 => "Non-Japanese",
            else => return error.UnknownDestinationCode,
        }
    }
};
