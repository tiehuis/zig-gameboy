const std = @import("std");
const Mmu = @import("mmu.zig").Mmu;
const assert = std.debug.assert;

// 4.194304Mhz
pub const clock_speed = 4194304;

pub const FlagZ: u8 = 0x80;
pub const FlagS: u8 = 0x40;
pub const FlagH: u8 = 0x20;
pub const FlagC: u8 = 0x10;

pub const A = 0;
pub const F = 1;
pub const B = 2;
pub const C = 3;
pub const D = 4;
pub const E = 5;
pub const H = 6;
pub const L = 7;

pub const AF = 0;
pub const BC = 1;
pub const DE = 2;
pub const HL = 3;
pub const SP = 4;
pub const PC = 5;

const trace = false;

pub const Z80 = struct {
    // A, F, B, C, D, E, H, L, PC, SP
    r: [12]u8,
    r1: []u8,
    r2: []u16,
    total_ticks: usize,
    ticks: usize,

    // Communication
    mmu: *Mmu,

    pub fn init(mmu: *Mmu) Z80 {
        var z = Z80{
            .r = []u8{0} ** 12,
            .r1 = undefined,
            .r2 = undefined,
            .total_ticks = 0,
            .ticks = 0,
            .mmu = mmu,
        };

        // Allow accessing half/whole registers somewhwat similar to anonymous unions
        z.r1 = z.r[0..8];
        z.r2 = @alignCast(@alignOf(u16), @bytesToSlice(u16, z.r[0..12]));
        return z;
    }

    pub fn step(self: *Z80, opcode: u8) usize {
        instruction_table[opcode](self);

        if (trace) {
            if (opcode != 0xcb) {
                std.debug.warn("\n : {} [{x}]\n", instruction_names_table[opcode], opcode);
            } else {
                const cb_opcode = self.mmu.read(self.r2[PC] + 1);
                std.debug.warn("\n : {} [{x}]\n", cb_instruction_names_table[cb_opcode], cb_opcode);
            }
            self.dump();

            var stdin_file = std.io.getStdIn() catch unreachable;
            var stdin = stdin_file.inStream();
            _ = stdin.stream.readByte();
        }

        const r = self.ticks;
        self.total_ticks += r;
        self.ticks = 0;
        return r;
    }

    pub fn dump(self: *Z80) void {
        std.debug.warn(
            \\ SP:{x} PC:{x} AF:{x} BC:{x} DE:{x} ({})
            \\  [{x}, {x}, ... ]
            \\
        ,
            self.r2[SP],
            self.r2[PC],
            self.r2[AF],
            self.r2[BC],
            self.r2[DE],
            self.total_ticks,
            self.mmu.read(self.r2[PC]),
            self.mmu.read(self.r2[PC] + 1),
        );
    }

    // Read a byte from memory in 4 cycles
    fn read8(self: *Z80, address: u16) u8 {
        self.ticks += 4;
        return self.mmu.read(address);
    }

    // Read a word from memory in 8 cycles
    fn read16(self: *Z80, address: u16) u16 {
        return u16(self.read8(address)) | (u16(self.read8(address + 1)) << 8);
    }

    // Write a byte to memory in 4 cycles
    fn write8(self: *Z80, address: u16, byte: u8) void {
        self.ticks += 4;
        self.mmu.write(address, byte);
    }

    // Write a word to memory in 8 cycles
    fn write16(self: *Z80, address: u16, word: u16) void {
        self.write8(address, @truncate(u8, word));
        self.write8(address + 1, @intCast(u8, word >> 8));
    }

    // Cycle the cpu with no other side-effects.
    fn cycle(self: *Z80) void {
        self.ticks += 4;
    }
};

// See: http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html

const Instruction = fn (z: *Z80) void;

const instruction_table = [256]Instruction{
    //    X0,       X1,       X2,       X3,       X4,       X5,       X6,       X7,
    //    X8,       X9,       Xa,       Xb,       Xc,       Xd,       Xe,       Xf,
    nop_____, ld16(BC), ldXa(BC), incw(BC), inc__(B), dec__(B), ld8__(B), rlca____, // 0X
    ld_d16sp, adhl(BC), ldaX(BC), decw(BC), inc__(C), dec__(C), ld8__(C), rrca____,
    stop____, ld16(DE), ldXa(DE), incw(DE), inc__(D), dec__(D), ld8__(D), rla_____, // 1X
    jr______, adhl(DE), ldaX(DE), decw(DE), inc__(E), dec__(E), ld8__(E), rra_____,
    jr_nz___, ld16(HL), ld_hli_a, incw(HL), inc__(H), dec__(H), ld8__(H), ________, // 2X
    jr_z____, adhl(HL), ld_a_hli, decw(HL), inc__(L), dec__(L), ld8__(L), ________,
    jr_nc___, ld16(SP), ld_hld_a, incw(SP), inc_hl__, dec_hl__, ld_hl_d8, ________, // 3X
    jr_c____, adhl(SP), ld_a_hld, decw(SP), inc__(A), dec__(A), ld8__(A), ________,
    ld(B, B), ld(B, C), ld(B, D), ld(B, E), ld(B, H), ld(B, L), ldXhl(B), ld(B, A), // 4X
    ld(C, B), ld(C, C), ld(C, D), ld(C, E), ld(C, H), ld(C, L), ldXhl(C), ld(C, A),
    ld(D, B), ld(D, C), ld(D, D), ld(D, E), ld(D, H), ld(D, L), ldXhl(D), ld(D, A), // 5X
    ld(E, B), ld(E, C), ld(E, D), ld(E, E), ld(E, H), ld(E, L), ldXhl(E), ld(E, A),
    ld(H, B), ld(H, C), ld(H, D), ld(H, E), ld(H, H), ld(H, L), ldXhl(H), ld(H, A), // 6X
    ld(L, B), ld(L, C), ld(L, D), ld(L, E), ld(L, H), ld(L, L), ldXhl(L), ld(L, A),
    ldhlY(B), ldhlY(C), ldhlY(D), ldhlY(E), ldhlY(H), ldhlY(L), halt____, ldhlY(A), // 7X
    ld(A, B), ld(A, C), ld(A, D), ld(A, E), ld(A, H), ld(A, L), ldXhl(A), ld(A, A),
    add__(B), add__(C), add__(D), add__(E), add__(H), add__(L), addhl___, add__(A), // 8X
    adc__(B), adc__(C), adc__(D), adc__(E), adc__(H), adc__(L), adchl___, adc__(A),
    sub__(B), sub__(C), sub__(D), sub__(E), sub__(H), sub__(L), subhl___, sub__(A), // 9X
    sbc__(B), sbc__(C), sbc__(D), sbc__(E), sbc__(H), sbc__(L), sbchl___, sbc__(A),
    and__(B), and__(C), and__(D), and__(E), and__(H), and__(L), andhl___, and__(A), // aX
    xor__(B), xor__(C), xor__(D), xor__(E), xor__(H), xor__(L), xorhl___, xor__(A),
    or___(B), or___(C), or___(D), or___(E), or___(H), or___(L), orhl____, or___(A), // bX
    cp___(B), cp___(C), cp___(D), cp___(E), cp___(H), cp___(L), cphl____, cp___(A),
    ret_nz__, pop_(BC), jp_nz___, jp_nn___, call_nz_, push(BC), add_a_d8, rst__(0), // cX
    ret_z___, ret_____, jp_z____, cb______, call_z__, call____, adc_a_d8, rst__(8),
    ret_nc__, pop_(DE), jp_nc___, illegal_, call_nc_, push(DE), sub_a_d8, rst_(16), // dX
    ret_c___, ________, jp_c____, illegal_, call_c__, illegal_, sbc_a_d8, rst_(24),
    ldh_a8_a, pop_(HL), ld_uc_a_, illegal_, illegal_, push(HL), and_a_d8, rst_(32), // eX
    ________, ________, ld_a16_a, illegal_, illegal_, illegal_, xor_a_d8, rst_(40),
    ldh_a_a8, pop_(AF), ld_a_uc_, di______, illegal_, push(AF), or_a_d8_, rst_(48), // fX
    ________, ________, ld_a_a16, ei______, illegal_, illegal_, cp_a_d8_, rst_(56),
};

const instruction_names_table = [256][11]u8{
    "NOP        ", "LD BC,d16  ", "LD (BC),A  ", "INC BC     ",
    "INC B      ", "DEC B      ", "LD B,d8    ", "RLCA       ",
    "LD (a16),SP", "ADD HL,BC  ", "LD A,(BC)  ", "DEC BC     ",
    "INC C      ", "DEC C      ", "LD C,d8    ", "RRCA       ",

    "STOP       ", "LD DE,d16  ", "LD (DE),A  ", "INC DE     ",
    "INC D      ", "DEC D      ", "LD D,d8    ", "RLA        ",
    "JR r8      ", "ADD HL,DE  ", "LD A,(DE)  ", "DEC DE     ",
    "INC E      ", "DEC E      ", "LD E,d8    ", "RRA        ",

    "JR NZ,r8   ", "LD HL,d16  ", "LD (HL+),A ", "INC HL     ",
    "INC H      ", "DEC H      ", "LD H,d8    ", "DAA        ",
    "JR Z,r8    ", "ADD HL,HL  ", "LD A,(HL+) ", "DEC HL     ",
    "INC L      ", "DEC L      ", "LD L,d8    ", "CPL        ",

    "JR NC,r8   ", "LD SP,d16  ", "LD (HL-),A ", "INC SP     ",
    "INC (HL)   ", "DEC (HL)   ", "LD (HL),d8 ", "SCF        ",
    "JR C,r8    ", "ADD HL,SP  ", "LD A,(HL-) ", "DEC SP     ",
    "INC A      ", "DEC A      ", "LD A,d8    ", "CCF        ",

    "LD B,B     ", "LD B,C     ", "LD B,D     ", "LD B,E     ",
    "LD B,H     ", "LD B,L     ", "LD B,(HL)  ", "LD B,A     ",
    "LD C,B     ", "LD C,C     ", "LD C,D     ", "LD C,E     ",
    "LD C,H     ", "LD C,L     ", "LD C,(HL)  ", "LD C,A     ",

    "LD D,B     ", "LD D,C     ", "LD D,D     ", "LD D,E     ",
    "LD D,H     ", "LD D,L     ", "LD D,(HL)  ", "LD D,A     ",
    "LD E,B     ", "LD E,C     ", "LD E,D     ", "LD E,E     ",
    "LD E,H     ", "LD E,L     ", "LD E,(HL)  ", "LD E,A     ",

    "LD H,B     ", "LD H,C     ", "LD H,D     ", "LD H,E     ",
    "LD H,H     ", "LD H,L     ", "LD H,(HL)  ", "LD H,A     ",
    "LD L,B     ", "LD L,C     ", "LD L,D     ", "LD L,E     ",
    "LD L,H     ", "LD L,L     ", "LD L,(HL)  ", "LD L,A     ",

    "LD (HL),B  ", "LD (HL),C  ", "LD (HL),D  ", "LD (HL),E  ",
    "LD (HL),H  ", "LD (HL),L  ", "HALT       ", "LD (HL),A  ",
    "LD A,B     ", "LD A,C     ", "LD A,D     ", "LD A,E     ",
    "LD A,H     ", "LD A,L     ", "LD A,(HL)  ", "LD A,A     ",

    "ADD A,B    ", "ADD A,C    ", "ADD A,D    ", "ADD A,E    ",
    "ADD A,H    ", "ADD A,L    ", "ADD A,(HL) ", "ADD A,A    ",
    "ADC A,B    ", "ADC A,C    ", "ADC A,D    ", "ADC A,E    ",
    "ADC A,H    ", "ADC A,L    ", "ADC A,(HL) ", "ADC A,A    ",

    "SUB B      ", "SUB C      ", "SUB D      ", "SUB E      ",
    "SUB H      ", "SUB L      ", "SUB (HL)   ", "SUB A      ",
    "SBC A,B    ", "SBC A,C    ", "SBC A,D    ", "SBC A,E    ",
    "SBC A,H    ", "SBC A,L    ", "SBC A,(HL) ", "SBC A,A    ",

    "AND B      ", "AND C      ", "AND D      ", "AND E      ",
    "AND H      ", "AND L      ", "AND (HL)   ", "AND A      ",
    "XOR B      ", "XOR C      ", "XOR D      ", "XOR E      ",
    "XOR H      ", "XOR L      ", "XOR (HL)   ", "XOR A      ",

    "OR B       ", "OR C       ", "OR D       ", "OR E       ",
    "OR H       ", "OR L       ", "OR (HL)    ", "OR A       ",
    "CP B       ", "CP C       ", "CP D       ", "CP E       ",
    "CP H       ", "CP L       ", "CP (HL)    ", "CP A       ",

    "RET NZ     ", "POP BC     ", "JP NZ,a16  ", "JP a16     ",
    "CALL NZ,a16", "PUSH BC    ", "ADD A,d8   ", "RST 00h    ",
    "RET Z      ", "RET        ", "JP Z,a16   ", "PREFIX CB  ",
    "CALL Z,a16 ", "CALL a16   ", "ADC A,d8   ", "RST 08h    ",

    "RET NC     ", "POP DE     ", "JP NC,a16  ", "ILLEGAL    ",
    "CALL NC,a16", "PUSH DE    ", "SUB d8     ", "RST 10h    ",
    "RET C      ", "RETI       ", "JP C,a16   ", "ILLEGAL    ",
    "CALL C,a16 ", "ILLEGAL    ", "SBC A,d8   ", "RST 18h    ",

    "LDH (a8),A ", "POP HL     ", "LD (C),A   ", "ILLEGAL    ",
    "ILLEGAL    ", "PUSH HL    ", "AND d8     ", "RST 20h    ",
    "ADD SP,r8  ", "JP (HL)    ", "LD (a16),A ", "ILLEGAL    ",
    "ILLEGAL    ", "ILLEGAL    ", "XOR d8     ", "RST 28h    ",

    "LDH A,(a8) ", "POP AF     ", "LD A,(C)   ", "DI         ",
    "ILLEGAL    ", "PUSH AF    ", "OR d8      ", "RST 30h    ",
    "LD HL,SP+r8", "LD SP,HL   ", "LD A,(a16) ", "EI         ",
    "ILLEGAL    ", "ILLEGAL    ", "CP d8      ", "RST 38h    ",
};

fn ld_d16sp(z: *Z80) void {
    const address = z.read16(z.r2[PC]);
    z.r2[PC] += 2;
    z.write16(address, z.r2[SP]);
    assert(z.ticks == 20);
}

fn ________(z: *Z80) void {
    const opcode = z.mmu.read(z.r2[PC] -% 1);

    std.debug.warn(
        \\!!> UNIMPLEMENTED OPCODE: {} [{x}]
        \\
    ,
        instruction_names_table[opcode],
        opcode,
    );

    z.dump();
    std.os.exit(1);
}

fn nop_____(z: *Z80) void {
    assert(z.ticks == 4);
}

fn stop____(z: *Z80) void {
    assert(z.ticks == 4);
}

fn di______(z: *Z80) void {
    // TODO: Set flag
    assert(z.ticks == 4);
}

fn ei______(z: *Z80) void {
    // TODO: Set flag
    assert(z.ticks == 4);
}

// PC = (PC + 1)
fn jp_nn___(z: *Z80) void {
    const address = z.read16(z.r2[PC]);
    z.cycle();
    z.r2[PC] = address;
    assert(z.ticks == 16);
}

fn flagCondition(z: *Z80, comptime flag: comptime_int, comptime invert: bool) bool {
    const r = (z.r1[F] & flag) != 0;
    return if (invert) !r else r;
}

// PC += (PC + 1) if condition
fn jr_c(comptime flag: comptime_int, comptime invert: bool) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const offset = z.read8(z.r2[PC]);
            z.r2[PC] += 1;
            if (flagCondition(z, flag, invert)) {
                z.cycle();
                z.r2[PC] += offset;
                assert(z.ticks == 12);
            } else {
                assert(z.ticks == 8);
            }
        }
    }.impl;
}

const jr_nz___ = jr_c(FlagZ, false);
const jr_nc___ = jr_c(FlagC, false);
const jr_z____ = jr_c(FlagZ, true);
const jr_c____ = jr_c(FlagC, true);

fn jr______(z: *Z80) void {
    const offset = z.read8(z.r2[PC]);
    z.r2[PC] += 1;
    z.cycle();
    z.r2[PC] += offset;
    assert(z.ticks == 12);
}

// PC += (PC + 1) if condition
fn jp_c(comptime flag: comptime_int, comptime invert: bool) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const address = z.read16(z.r2[PC]);
            z.r2[PC] += 2;
            if (flagCondition(z, flag, invert)) {
                z.cycle();
                z.r2[PC] = address;
                assert(z.ticks == 16);
            } else {
                assert(z.ticks == 12);
            }
        }
    }.impl;
}

const jp_nz___ = jp_c(FlagZ, false);
const jp_nc___ = jp_c(FlagC, false);
const jp_z____ = jp_c(FlagZ, true);
const jp_c____ = jp_c(FlagC, true);

// PC = (SP) if condition
fn ret_c(comptime flag: comptime_int, comptime invert: bool) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const address = z.read16(z.r2[SP]);
            z.cycle();
            z.r2[PC] += 2;
            if (flagCondition(z, flag, invert)) {
                z.r2[PC] = address;
                assert(z.ticks == 16);
            } else {
                assert(z.ticks == 12);
            }
        }
    }.impl;
}

const ret_nz__ = ret_c(FlagZ, false);
const ret_nc__ = ret_c(FlagC, false);
const ret_z___ = ret_c(FlagZ, true);
const ret_c___ = ret_c(FlagC, true);

const rst__ = rst_;

fn rst_(comptime address: u8) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.cycle();
            z.write16(z.r2[SP] - 2, z.r2[PC]);
            z.r2[PC] = address;
        }
    }.impl;
}

fn call_c(comptime flag: comptime_int, comptime invert: bool) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const address = z.read16(z.r2[PC]);
            z.r2[PC] += 2;
            if (flagCondition(z, flag, invert)) {
                z.cycle();
                z.write16(z.r2[SP] - 2, z.r2[PC]);
                z.r2[PC] = address;
                assert(z.ticks == 24);
            } else {
                assert(z.ticks == 12);
            }
        }
    }.impl;
}

const call_nz_ = call_c(FlagZ, false);
const call_nc_ = call_c(FlagC, false);
const call_z__ = call_c(FlagZ, true);
const call_c__ = call_c(FlagC, true);

fn call____(z: *Z80) void {
    const address = z.read16(z.r2[PC]);
    z.r2[PC] +%= 2;
    z.cycle();
    z.write16(z.r2[SP] - 2, z.r2[PC]);
    z.r2[SP] -%= 2;
    z.r2[PC] = address;
    assert(z.ticks == 24);
}

fn ret_____(z: *Z80) void {
    // TODO:
    const address = z.read16(z.r2[SP]);
    z.cycle();
    z.r2[SP] +%= 2;
    z.r2[PC] = address;
}

// Pop stack into rr register
fn pop_(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r2[X] = z.read16(z.r2[SP]);
            z.r2[SP] +%= 2;
            z.r1[F] &= 0xf0;
            assert(z.ticks == 12);
        }
    }.impl;
}

// Push rr register onto stack
fn push(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.cycle();
            z.write16(z.r2[SP] - 2, z.r2[X]);
            z.r2[SP] -%= 2;
            assert(z.ticks == 16);
        }
    }.impl;
}

fn illegal_(z: *Z80) void {
    const opcode = z.mmu.read(z.r2[PC] -% 1);

    std.debug.warn(
        \\
        \\ILLEGAL OPCODE: {x}
        \\
    , opcode);

    std.os.exit(1);
}

fn halt____(z: *Z80) void {
    // TODO: Update cycles
}

fn adhl(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.cycle();

            const r = z.r2[HL];
            const u = z.r2[X];

            z.r2[HL] +%= z.r2[X];
            assert(z.ticks == 8);
            z.r1[F] &= ~(FlagS | FlagC | FlagH);

            if (((r & 0xfff) + (u & 0xfff)) & 0x1000 != 0) {
                z.r1[F] |= FlagH;
            }
            if (usize(r) + usize(u) & 0x10000 != 0) {
                z.r1[F] |= FlagC;
            }
        }
    }.impl;
}

// rX = d8
fn ldd8(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.read8(z.r2[PC]);
            z.r2[PC] += 1;
            z.r1[X] = r;
            assert(z.ticks == 8);
        }
    }.impl;
}

fn ld_hl_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] += 1;
    z.write8(z.r2[HL], r);
    assert(z.ticks == 12);
}

// (0xff00 + a8) = A
fn ldh_a8_a(z: *Z80) void {
    const address = z.read8(z.r2[PC]);
    z.r2[PC] += 1;
    z.write8(0xff00 + u16(address), z.r1[A]);
    assert(z.ticks == 12);
}

// A = (0xff00 + a8)
fn ldh_a_a8(z: *Z80) void {
    const address = z.read8(z.r2[PC]);
    z.r2[PC] += 1;
    z.r1[A] = z.read8(0xff00 + u16(address));
    assert(z.ticks == 12);
}

// (0xff00 + C) = A
fn ld_uc_a_(z: *Z80) void {
    z.write8(0xff00 + u16(z.r1[C]), z.r1[A]);
    assert(z.ticks == 8);
}

// A = (0xff00 + C)
fn ld_a_uc_(z: *Z80) void {
    z.r1[A] = z.read8(0xff00 + u16(z.r1[C]));
    assert(z.ticks == 8);
}

// (X) = A
fn ldXa(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.write8(z.r2[X], z.r1[A]);
            assert(z.ticks == 4);
        }
    }.impl;
}

// A = (X)
fn ldaX(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[A] = z.read8(z.r2[X]);
            assert(z.ticks == 8);
        }
    }.impl;
}

// (HL) = A, HL += 1
fn ld_hli_a(z: *Z80) void {
    z.write8(z.r2[HL], z.r1[A]);
    z.r2[HL] +%= 1;
    assert(z.ticks == 8);
}

// A = (HL), HL += 1
fn ld_a_hli(z: *Z80) void {
    z.r1[A] = z.read8(z.r2[HL]);
    z.r2[HL] +%= 1;
    assert(z.ticks == 8);
}

// (HL) = A, HL -= 1
fn ld_hld_a(z: *Z80) void {
    z.write8(z.r2[HL], z.r1[A]);
    z.r2[HL] -%= 1;
    assert(z.ticks == 8);
}

// A = (HL), HL -= 1
fn ld_a_hld(z: *Z80) void {
    z.r1[A] = z.read8(z.r2[HL]);
    z.r2[HL] -%= 1;
    assert(z.ticks == 8);
}

// rX = (PC)
fn ld16(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r2[X] = z.read16(z.r2[PC]);
            z.r2[PC] += 2;
            assert(z.ticks == 12);
        }
    }.impl;
}

// A = (PC)
fn ld_a_a16(z: *Z80) void {
    const address = z.read16(z.r2[PC]);
    z.r2[PC] += 2;
    z.r1[A] = z.read8(address);
    assert(z.ticks == 16);
}

// (PC) = A
fn ld_a16_a(z: *Z80) void {
    const address = z.read16(z.r2[PC]);
    z.r2[PC] += 2;
    z.write8(address, z.r1[A]);
    assert(z.ticks == 16);
}

// rX = (PC)
fn ld8__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] = z.read8(z.r2[PC]);
            z.r2[PC] += 1;
            assert(z.ticks == 8);
        }
    }.impl;
}

// rX = rY
fn ld(comptime X: comptime_int, comptime Y: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            if (X != Y) {
                z.r1[X] = z.r1[Y];
            }
            assert(z.ticks == 4);
        }
    }.impl;
}

// rX = (HL)
fn ldXhl(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] = z.read8(z.r2[HL]);
            assert(z.ticks == 8);
        }
    }.impl;
}

// (HL) = rY
fn ldhlY(comptime Y: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.write8(z.r2[HL], z.r1[Y]);
            assert(z.ticks == 8);
        }
    }.impl;
}

// rX += 1, no flags set
fn incw(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r2[X] +%= 1;
            z.cycle();
            assert(z.ticks == 8);
        }
    }.impl;
}

// rX = rX + 1
fn inc__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] +%= 1;
            z.r1[F] &= ~(FlagS | FlagZ | FlagH);

            if (z.r1[X] & 0x0f == 0) {
                z.r1[F] |= FlagH;
            }
            if (z.r1[X] == 0) {
                z.r1[F] |= FlagZ;
            }

            assert(z.ticks == 4);
        }
    }.impl;
}

fn inc_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]) +% 1;
    z.r1[F] &= ~(FlagS | FlagZ | FlagH);
    z.write8(z.r2[HL], r);

    if (r & 0x0f == 0) {
        z.r1[F] |= FlagH;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }

    assert(z.ticks == 12);
}

// rX -= 1, no flags set
fn decw(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r2[X] -%= 1;
            z.cycle();
            assert(z.ticks == 8);
        }
    }.impl;
}

// rX = rX - 1
fn dec__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] -%= 1;
            z.r1[F] &= ~(FlagZ | FlagH);
            z.r1[F] |= FlagS;

            if (z.r1[X] & 0xf == 0xf) {
                z.r1[F] |= FlagH;
            }
            if (z.r1[X] == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 4);
        }
    }.impl;
}

fn dec_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]) -% 1;
    z.r1[F] &= ~(FlagZ | FlagH);
    z.r1[F] |= FlagS;
    z.write8(z.r2[HL], r);

    if (r & 0xf == 0xf) {
        z.r1[F] |= FlagH;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 12);
}

// A = A + r \w carry
fn adc__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            adcv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn adc_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    adcv(z, r);
    assert(z.ticks == 8);
}

fn adchl___(z: *Z80) void {
    adcv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn adcv(z: *Z80, r: u8) void {
    const a = z.r1[A];
    const c = @boolToInt((z.r1[F] & FlagC) != 0);
    z.r1[A] = a +% r +% c;

    if (a +% r +% c == 0) {
        z.r1[F] |= FlagZ;
    }
    if ((a & 0xf) + (r & 0xf) + c > 0xf) {
        z.r1[F] |= FlagH;
    }
    if (usize(a) + usize(r) + c > 0xff) {
        z.r1[F] |= FlagC;
    }
}

// A = A - r \w carry
fn sbc__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            sbcv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn sbc_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    sbcv(z, r);
    assert(z.ticks == 8);
}

fn sbchl___(z: *Z80) void {
    sbcv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn sbcv(z: *Z80, r: u8) void {
    const a = z.r1[A];
    const c = @boolToInt((z.r1[F] & FlagC) != 0);
    z.r1[A] = a -% r -% c;
    z.r1[F] = FlagS;

    if (a -% r -% c == 0) {
        z.r1[F] |= FlagZ;
    }
    if ((a & 0xf) < (r & 0xf) + c) {
        z.r1[F] |= FlagH;
    }
    if (usize(a) -% usize(r) - c > 0xff) {
        z.r1[F] |= FlagC;
    }
}

// A = A + r
fn add__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            addv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn add_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    addv(z, r);
    assert(z.ticks == 8);
}

fn addhl___(z: *Z80) void {
    addv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn addv(z: *Z80, r: u8) void {
    const a = z.r1[A];
    z.r1[A] +%= r;

    if (a +% r == 0) {
        z.r1[F] |= FlagZ;
    }
    if ((a & 0xf) + (r & 0xf) > 0xf) {
        z.r1[F] |= FlagH;
    }
    if (usize(a) + usize(r) > 0xff) {
        z.r1[F] |= FlagC;
    }
}

// A = A - r
fn sub__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            subv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn sub_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    subv(z, r);
    assert(z.ticks == 8);
}

fn subhl___(z: *Z80) void {
    subv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn subv(z: *Z80, r: u8) void {
    const a = z.r1[A];
    z.r1[A] -%= r;
    z.r1[F] |= FlagS;

    if (a == r) {
        z.r1[F] |= FlagZ;
    }
    if ((a & 0xf) < (r & 0xf)) {
        z.r1[F] |= FlagH;
    }
    if (a < r) {
        z.r1[F] |= FlagC;
    }
}

// A = A ^ r
fn xor__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            xorv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn xor_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    xorv(z, r);
    assert(z.ticks == 8);
}

fn xorhl___(z: *Z80) void {
    xorv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn xorv(z: *Z80, r: u8) void {
    z.r1[F] = if (z.r1[A] ^ r == 0) FlagZ else 0;
    z.r1[A] ^= r;
}

// A = A & r
fn and__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            andv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn and_a_d8(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    andv(z, r);
    assert(z.ticks == 8);
}

fn andhl___(z: *Z80) void {
    andv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn andv(z: *Z80, r: u8) void {
    z.r1[F] = FlagH | if ((z.r1[A] & r) == 0) FlagZ else 0;
    z.r1[A] &= r;
}

// A = A | r
fn or___(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            orv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn or_a_d8_(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    orv(z, r);
    assert(z.ticks == 8);
}

fn orhl____(z: *Z80) void {
    orv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn orv(z: *Z80, r: u8) void {
    z.r1[F] = if ((z.r1[A] | r) == 0) FlagZ else 0;
    z.r1[A] |= r;
}

fn cp___(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            cpv(z, z.r1[X]);
            assert(z.ticks == 4);
        }
    }.impl;
}

fn cp_a_d8_(z: *Z80) void {
    const r = z.read8(z.r2[PC]);
    z.r2[PC] +%= 1;
    cpv(z, r);
    assert(z.ticks == 8);
}

fn cphl____(z: *Z80) void {
    cpv(z, z.read8(z.r2[HL]));
    assert(z.ticks == 8);
}

fn cpv(z: *Z80, r: u8) void {
    const a = z.r1[A];
    z.r1[F] = FlagS;

    if (a == r) {
        z.r1[F] |= FlagZ;
    }
    if ((a & 0xf) < (r & 0xf)) {
        z.r1[F] |= FlagH;
    }
    if (a < r) {
        z.r1[F] |= FlagC;
    }
}

// These are similar to the generic rlc_ but do not modify the zero flag and are lower latency.
fn rlca____(z: *Z80) void {
    const r = z.r1[A];
    const c = @boolToInt(r & 0x80 != 0);
    z.r1[A] = (r << 1) | c;
    z.r1[F] = 0;

    if (c != 0) {
        z.r1[F] |= FlagC;
    }
    assert(z.ticks == 4);
}

fn rla_____(z: *Z80) void {
    const r = z.r1[A];
    const c = @boolToInt(r & 0x80 != 0);
    const h = z.r1[F] & FlagC != 0;
    z.r1[A] = (r << 1) | c;
    z.r1[F] = 0;

    if (h) {
        z.r1[F] |= FlagC;
    }
    assert(z.ticks == 4);
}

fn rrca____(z: *Z80) void {
    const r = z.r1[A];
    const c = r & 1;
    z.r1[A] = (r >> 1) | (c << 7);
    z.r1[F] = 0;
    if (c != 0) {
        z.r1[F] |= FlagC;
    }
}

fn rra_____(z: *Z80) void {
    const r = z.r1[A];
    const c = r & 1;
    const h = z.r1[F] & FlagC != 0;
    z.r1[A] = (r >> 1) | (c << 7);
    z.r1[F] = 0;

    if (h) {
        z.r1[F] |= FlagC;
    }
    assert(z.ticks == 4);
}

// TODO: Check this as it seems the wrong way around.

const cb_instruction_table = [256]Instruction{
    //    X0,       X1,       X2,       X3,       X4,       X5,       X6,       X7,
    //    X8,       X9,       Xa,       Xb,       Xc,       Xd,       Xe,       Xf,
    rlc__(B), rlc__(C), rlc__(D), rlc__(E), rlc__(H), rlc__(L), rlc_hl__, rlc__(A), // 0X
    rrc__(B), rrc__(C), rrc__(D), rrc__(E), rrc__(H), rrc__(L), rrc_hl__, rrc__(A),
    rl___(B), rl___(C), rl___(D), rl___(E), rl___(H), rl___(L), rl_hl___, rl___(A), // 1X
    rr___(B), rr___(C), rr___(D), rr___(E), rr___(H), rr___(L), rr_hl___, rr___(A),
    sla__(B), sla__(C), sla__(D), sla__(E), sla__(H), sla__(L), sla_hl__, sla__(A), // 2X
    sra__(B), sra__(C), sra__(D), sra__(E), sra__(H), sra__(L), sra_hl__, sra__(A),
    swap_(B), swap_(C), swap_(D), swap_(E), swap_(H), swap_(L), swap_hl_, swap_(A), // 3X
    srl__(B), srl__(C), srl__(D), srl__(E), srl__(H), srl__(L), srl_hl__, srl__(A),
    bt(0, B), bt(0, C), bt(0, D), bt(0, E), bt(0, H), bt(0, L), bt_hl(0), bt(0, A), // 4X
    bt(1, B), bt(1, C), bt(1, D), bt(1, E), bt(1, H), bt(1, L), bt_hl(1), bt(1, A),
    bt(2, B), bt(2, C), bt(2, D), bt(2, E), bt(2, H), bt(2, L), bt_hl(2), bt(2, A), // 5X
    bt(3, B), bt(3, C), bt(3, D), bt(3, E), bt(3, H), bt(3, L), bt_hl(3), bt(3, A),
    bt(4, B), bt(4, C), bt(4, D), bt(4, E), bt(4, H), bt(4, L), bt_hl(4), bt(4, A), // 6X
    bt(5, B), bt(5, C), bt(5, D), bt(5, E), bt(5, H), bt(5, L), bt_hl(5), bt(5, A),
    bt(6, B), bt(6, C), bt(6, D), bt(6, E), bt(6, H), bt(6, L), bt_hl(6), bt(6, A), // 7X
    bt(7, B), bt(7, C), bt(7, D), bt(7, E), bt(7, H), bt(7, L), bt_hl(7), bt(7, A),
    rs(0, B), rs(0, C), rs(0, D), rs(0, E), rs(0, H), rs(0, L), rs_hl(0), rs(0, A), // 8X
    rs(1, B), rs(1, C), rs(1, D), rs(1, E), rs(1, H), rs(1, L), rs_hl(1), rs(1, A),
    rs(2, B), rs(2, C), rs(2, D), rs(2, E), rs(2, H), rs(2, L), rs_hl(2), rs(2, A), // 9X
    rs(3, B), rs(3, C), rs(3, D), rs(3, E), rs(3, H), rs(3, L), rs_hl(3), rs(3, A),
    rs(4, B), rs(4, C), rs(4, D), rs(4, E), rs(4, H), rs(4, L), rs_hl(4), rs(4, A), // aX
    rs(5, B), rs(5, C), rs(5, D), rs(5, E), rs(5, H), rs(5, L), rs_hl(5), rs(5, A),
    rs(6, B), rs(6, C), rs(6, D), rs(6, E), rs(6, H), rs(6, L), rs_hl(6), rs(6, A), // bX
    rs(7, B), rs(7, C), rs(7, D), rs(7, E), rs(7, H), rs(7, L), rs_hl(7), rs(7, A),
    st(0, B), st(0, C), st(0, D), st(0, E), st(0, H), st(0, L), st_hl(0), st(0, A), // cX
    st(1, B), st(1, C), st(1, D), st(1, E), st(1, H), st(1, L), st_hl(1), st(1, A),
    st(2, B), st(2, C), st(2, D), st(2, E), st(2, H), st(2, L), st_hl(2), st(2, A), // dX
    st(3, B), st(3, C), st(3, D), st(3, E), st(3, H), st(3, L), st_hl(3), st(3, A),
    st(4, B), st(4, C), st(4, D), st(4, E), st(4, H), st(4, L), st_hl(4), st(4, A), // eX
    st(5, B), st(5, C), st(5, D), st(5, E), st(5, H), st(5, L), st_hl(5), st(5, A),
    st(6, B), st(6, C), st(6, D), st(6, E), st(6, H), st(6, L), st_hl(6), st(6, A), // fX
    st(7, B), st(7, C), st(7, D), st(7, E), st(7, H), st(7, L), st_hl(7), st(7, A),
};

const cb_instruction_names_table = [256][11]u8{
    "RLC B      ", "RLC C      ", "RLC D      ", "RLC E      ",
    "RLC H      ", "RLC L      ", "RLC (HL)   ", "RLC A      ",
    "RRC B      ", "RRC C      ", "RRC D      ", "RRC E      ",
    "RRC H      ", "RRC L      ", "RRC (HL)   ", "RRC A      ",

    "RL  B      ", "RL  C      ", "RL  D      ", "RL  E      ",
    "RL  H      ", "RL  L      ", "RL  (HL)   ", "RL  A      ",
    "RR  B      ", "RR  C      ", "RR  D      ", "RR  E      ",
    "RR  H      ", "RR  L      ", "RR  (HL)   ", "RR  A      ",

    "SLA B      ", "SLA C      ", "SLA D      ", "SLA E      ",
    "SLA H      ", "SLA L      ", "SLA (HL)   ", "SLA A      ",
    "SRA B      ", "SRA C      ", "SRA D      ", "SRA E      ",
    "SRA H      ", "SRA L      ", "SRA (HL)   ", "SRA A      ",

    "SWAP B     ", "SWAP C     ", "SWAP D     ", "SWAP E     ",
    "SWAP H     ", "SWAP L     ", "SWAP (HL)  ", "SWAP A     ",
    "SRL B      ", "SRL C      ", "SRL D      ", "SRL E      ",
    "SRL H      ", "SRL L      ", "SRL (HL)   ", "SRL A      ",

    "BIT 0,B    ", "BIT 0,C    ", "BIT 0,D    ", "BIT 0,E    ",
    "BIT 0,H    ", "BIT 0,L    ", "BIT 0,(HL) ", "BIT 0,A    ",
    "BIT 1,B    ", "BIT 1,C    ", "BIT 1,D    ", "BIT 1,E    ",
    "BIT 1,H    ", "BIT 1,L    ", "BIT 1,(HL) ", "BIT 1,A    ",

    "BIT 2,B    ", "BIT 2,C    ", "BIT 2,D    ", "BIT 2,E    ",
    "BIT 2,H    ", "BIT 2,L    ", "BIT 2,(HL) ", "BIT 2,A    ",
    "BIT 3,B    ", "BIT 3,C    ", "BIT 3,D    ", "BIT 3,E    ",
    "BIT 3,H    ", "BIT 3,L    ", "BIT 3,(HL) ", "BIT 3,A    ",

    "BIT 4,B    ", "BIT 4,C    ", "BIT 4,D    ", "BIT 4,E    ",
    "BIT 4,H    ", "BIT 4,L    ", "BIT 4,(HL) ", "BIT 4,A    ",
    "BIT 5,B    ", "BIT 5,C    ", "BIT 5,D    ", "BIT 5,E    ",
    "BIT 5,H    ", "BIT 5,L    ", "BIT 5,(HL) ", "BIT 5,A    ",

    "BIT 6,B    ", "BIT 4,C    ", "BIT 4,D    ", "BIT 4,E    ",
    "BIT 4,H    ", "BIT 4,L    ", "BIT 4,(HL) ", "BIT 4,A    ",
    "BIT 7,B    ", "BIT 7,C    ", "BIT 7,D    ", "BIT 7,E    ",
    "BIT 7,H    ", "BIT 7,L    ", "BIT 7,(HL) ", "BIT 7,A    ",

    "RES 0,B    ", "RES 0,C    ", "RES 0,D    ", "RES 0,E    ",
    "RES 0,H    ", "RES 0,L    ", "RES 0,(HL) ", "RES 0,A    ",
    "RES 1,B    ", "RES 1,C    ", "RES 1,D    ", "RES 1,E    ",
    "RES 1,H    ", "RES 1,L    ", "RES 1,(HL) ", "RES 1,A    ",

    "RES 2,B    ", "RES 2,C    ", "RES 2,D    ", "RES 2,E    ",
    "RES 2,H    ", "RES 2,L    ", "RES 2,(HL) ", "RES 2,A    ",
    "RES 3,B    ", "RES 3,C    ", "RES 3,D    ", "RES 3,E    ",
    "RES 3,H    ", "RES 3,L    ", "RES 3,(HL) ", "RES 3,A    ",

    "RES 4,B    ", "RES 4,C    ", "RES 4,D    ", "RES 4,E    ",
    "RES 4,H    ", "RES 4,L    ", "RES 4,(HL) ", "RES 4,A    ",
    "RES 5,B    ", "RES 5,C    ", "RES 5,D    ", "RES 5,E    ",
    "RES 5,H    ", "RES 5,L    ", "RES 5,(HL) ", "RES 5,A    ",

    "RES 6,B    ", "RES 4,C    ", "RES 4,D    ", "RES 4,E    ",
    "RES 4,H    ", "RES 4,L    ", "RES 4,(HL) ", "RES 4,A    ",
    "RES 7,B    ", "RES 7,C    ", "RES 7,D    ", "RES 7,E    ",
    "RES 7,H    ", "RES 7,L    ", "RES 7,(HL) ", "RES 7,A    ",

    "SET 0,B    ", "SET 0,C    ", "SET 0,D    ", "SET 0,E    ",
    "SET 0,H    ", "SET 0,L    ", "SET 0,(HL) ", "SET 0,A    ",
    "SET 1,B    ", "SET 1,C    ", "SET 1,D    ", "SET 1,E    ",
    "SET 1,H    ", "SET 1,L    ", "SET 1,(HL) ", "SET 1,A    ",

    "SET 2,B    ", "SET 2,C    ", "SET 2,D    ", "SET 2,E    ",
    "SET 2,H    ", "SET 2,L    ", "SET 2,(HL) ", "SET 2,A    ",
    "SET 3,B    ", "SET 3,C    ", "SET 3,D    ", "SET 3,E    ",
    "SET 3,H    ", "SET 3,L    ", "SET 3,(HL) ", "SET 3,A    ",

    "SET 4,B    ", "SET 4,C    ", "SET 4,D    ", "SET 4,E    ",
    "SET 4,H    ", "SET 4,L    ", "SET 4,(HL) ", "SET 4,A    ",
    "SET 5,B    ", "SET 5,C    ", "SET 5,D    ", "SET 5,E    ",
    "SET 5,H    ", "SET 5,L    ", "SET 5,(HL) ", "SET 5,A    ",

    "SET 6,B    ", "SET 4,C    ", "SET 4,D    ", "SET 4,E    ",
    "SET 4,H    ", "SET 4,L    ", "SET 4,(HL) ", "SET 4,A    ",
    "SET 7,B    ", "SET 7,C    ", "SET 7,D    ", "SET 7,E    ",
    "SET 7,H    ", "SET 7,L    ", "SET 7,(HL) ", "SET 7,A    ",
};

fn cb______(z: *Z80) void {
    const cb_op = z.read8(z.r2[PC]);
    z.r2[PC] += 1;
    cb_instruction_table[cb_op](z);
}

// TODO: Merge common paths with a generic write/read
fn rlc__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const c = @boolToInt(r & 0x80 != 0);
            z.r1[F] = 0;
            z.r1[X] = (r << 1) | c;

            if (c != 0) {
                z.r1[F] |= FlagC;
            }
            if (r << 1 == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn rlc_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const c = @boolToInt(r & 0x80 != 0);
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r << 1) | c);

    if (c != 0) {
        z.r1[F] |= FlagC;
    }
    if (r << 1 == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn rrc__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const c = @boolToInt(r & 0x01 != 0);
            z.r1[F] = 0;
            z.r1[X] = (r >> 1) | (u8(c) << 7);

            if (c != 0) {
                z.r1[F] |= FlagC;
            }
            if (r == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn rrc_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const c = @boolToInt(r & 0x01 != 0);
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r >> 1) | (u8(c) << 7));

    if (c != 0) {
        z.r1[F] |= FlagC;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn rl___(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const c = @boolToInt(z.r1[F] & FlagC != 0);
            const h = r & 0x80 != 0;
            z.r1[F] = 0;
            z.r1[X] = (r << 1) | c;

            if (h) {
                z.r1[F] |= FlagC;
            }
            if (r == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn rl_hl___(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const c = @boolToInt(z.r1[F] & FlagC != 0);
    const h = r & 0x80 != 0;
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r << 1) | c);

    if (h) {
        z.r1[F] |= FlagC;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn rr___(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const c = @boolToInt(z.r1[F] & FlagC != 0);
            const l = 0x01 != 0;
            z.r1[F] = 0;
            z.r1[X] = (r >> 1) | (u8(c) << 7);

            if (l) {
                z.r1[F] |= FlagC;
            }
            if (r == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn rr_hl___(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const c = @boolToInt(z.r1[F] & FlagC != 0);
    const l = 0x01 != 0;
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r >> 1) | (u8(c) << 7));

    if (l) {
        z.r1[F] |= FlagC;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn sla__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const c = r & 0x80 != 0;
            z.r1[F] = 0;
            z.r1[X] = r << 1;

            if (c) {
                z.r1[F] |= FlagC;
            }
            if (r & 0x7f == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn sla_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const c = r & 0x80 != 0;
    z.r1[F] = 0;
    z.write8(z.r2[HL], r << 1);

    if (c) {
        z.r1[F] |= FlagC;
    }
    if (r & 0x7f == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn sra__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const h = r & 0x80;
            z.r1[F] = 0;
            z.r1[X] = (r >> 1) | h;

            if (r & 1 != 0) {
                z.r1[F] |= FlagC;
            }
            if (z.r1[X] == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn sra_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const h = r & 0x80;
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r >> 1) | h);

    if (r & 1 != 0) {
        z.r1[F] |= FlagC;
    }
    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn srl__(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const h = r & 0x80;
            z.r1[F] = 0;
            z.r1[X] = r >> 1;

            if (r & 1 != 0) {
                z.r1[F] |= FlagC;
            }
            if (r >> 1 == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn srl_hl__(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const h = r & 0x80;
    z.r1[F] = 0;
    z.write8(z.r2[HL], r >> 1);

    if (r & 1 != 0) {
        z.r1[F] |= FlagC;
    }
    if (r >> 1 == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn swap_(comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            const r = z.r1[X];
            const h = r & 0x80;
            z.r1[F] = 0;
            z.r1[X] = (r >> 4) | (r << 4);

            if (r == 0) {
                z.r1[F] |= FlagZ;
            }
            assert(z.ticks == 8);
        }
    }.impl;
}

fn swap_hl_(z: *Z80) void {
    const r = z.read8(z.r2[HL]);
    const h = r & 0x80;
    z.r1[F] = 0;
    z.write8(z.r2[HL], (r >> 4) | (r << 4));

    if (r == 0) {
        z.r1[F] |= FlagZ;
    }
    assert(z.ticks == 16);
}

fn bt_hl(comptime i: u3) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            btv(i, z, z.read8(z.r2[HL]));
            assert(z.ticks == 16);
        }
    }.impl;
}

fn bt(comptime i: u3, comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            btv(i, z, z.r1[X]);
            assert(z.ticks == 8);
        }
    }.impl;
}

fn btv(comptime i: u3, z: *Z80, r: u8) void {
    z.r1[F] = FlagC | FlagH;
    if ((r & (u8(1) << i)) == 0) {
        z.r1[F] |= FlagZ;
    }
}

fn rs_hl(comptime i: u3) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            // TODO: Check cycle counts here
            var r = z.read8(z.r2[HL]);
            r &= ~(u8(1) << i);
            z.write8(z.r2[HL], r);
            assert(z.ticks == 16);
        }
    }.impl;
}

fn rs(comptime i: u3, comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] &= ~(u8(1) << i);
            assert(z.ticks == 8);
        }
    }.impl;
}

fn st_hl(comptime i: u3) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            var r = z.read8(z.r2[HL]);
            r |= u8(1) << i;
            z.write8(z.r2[HL], r);
            assert(z.ticks == 16);
        }
    }.impl;
}

fn st(comptime i: u3, comptime X: comptime_int) Instruction {
    return struct {
        fn impl(z: *Z80) void {
            z.r1[X] |= u8(1) << i;
            assert(z.ticks == 8);
        }
    }.impl;
}
