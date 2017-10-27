const std = @import("std");
const inst = @import("inst.zig");
const Mem = @import("mem.zig").Mem;
const printf = @import("std").io.stdout.printf;

pub const Registers = struct {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    flags: u8,
    sp: u16,
    pc: u16,
};

pub const Flags = struct {
    const Zero: u8      = 0b1000000;
    const Negative: u8  = 0b0100000;
    const HalfCarry: u8 = 0b0010000;
    const Carry: u8     = 0b0001000;
};

pub const Cpu = struct {
    // TODO: Store interrupt details on CPU instead.
    // TODO: Also store GPU here since we don't really have one itself.

    ime: bool,
    ticks: u64,
    regs: Registers,
    mem: &Mem,

    pub fn init(mem: &Mem) -> Cpu {
        Cpu {
            .ime = true,
            .ticks = 0,
            .regs = undefined,
            .mem = mem,
        }
    }

    const resetMemoryValues = []const u16 {
        0xFF05, 0x00, 0xFF06, 0x00, 0xFF07, 0x00, 0xFF10, 0x80, 0xFF11, 0xBF, 0xFF12, 0xF3,
        0xFF14, 0xBF, 0xFF16, 0x3F, 0xFF17, 0x00, 0xFF19, 0xBF, 0xFF1A, 0x7A, 0xFF1B, 0xFF,
        0xFF1C, 0x9F, 0xFF1E, 0xBF, 0xFF20, 0xFF, 0xFF21, 0x00, 0xFF22, 0x00, 0xFF23, 0xBF,
        0xFF24, 0x77, 0xFF25, 0xF3, 0xFF26, 0xF1, 0xFF40, 0x91, 0xFF42, 0x00, 0xFF43, 0x00,
        0xFF45, 0x00, 0xFF47, 0xFC, 0xFF48, 0xFF, 0xFF49, 0xFF, 0xFF4A, 0x00, 0xFF4B, 0x00,
        0xFFFF, 0x00,
    };

    pub fn reset(cpu: &Cpu) {
        cpu.ticks = 0;
        cpu.regs.a  = 0x01;
        cpu.regs.b  = 0x00;
        cpu.regs.c  = 0x13;
        cpu.regs.d  = 0x00;
        cpu.regs.e  = 0xD8;
        cpu.regs.h  = 0x01;
        cpu.regs.l  = 0x4D;
        cpu.regs.flags = 0xB0;
        cpu.regs.sp = 0xFFFE;
        cpu.regs.pc = 0x0100;

        var i: usize = 0;
        while (i < resetMemoryValues.len) : (i += 2) {
            cpu.mem.write8(resetMemoryValues[i], u8(resetMemoryValues[i + 1]));
        }
    }

    // Fetch, decode, execute a single instruction.
    pub fn step(cpu: &Cpu) {
        const opcode = cpu.mem.read8(cpu.regs.pc);
        cpu.regs.pc += 1;

        switch (opcode) {
            0x00 => cpu.call0(inst.nop),
            0x04 => cpu.call0(inst.inc_b),
            0x05 => cpu.call0(inst.dec_b),
            0x06 => cpu.call1(inst.ld_b),
            0x0C => cpu.call0(inst.inc_c),
            0x0D => cpu.call0(inst.dec_c),
            0x0E => cpu.call1(inst.ld_c),

            0x14 => cpu.call0(inst.inc_d),
            0x15 => cpu.call0(inst.dec_d),
            0x16 => cpu.call1(inst.ld_d),
            0x18 => cpu.call1(inst.jr_n),
            0x1C => cpu.call0(inst.inc_e),
            0x1D => cpu.call0(inst.dec_e),
            0x1E => cpu.call1(inst.ld_e),
            0x1F => cpu.call0(inst.rra),

            0x20 => cpu.call1(inst.jr_nz),
            0x21 => cpu.call2(inst.ld_hl),
            0x24 => cpu.call0(inst.inc_h),
            0x25 => cpu.call0(inst.dec_h),
            0x26 => cpu.call1(inst.ld_h),
            0x2C => cpu.call0(inst.inc_l),
            0x2D => cpu.call0(inst.dec_l),
            0x2E => cpu.call1(inst.ld_l),

            0x32 => cpu.call0(inst.ldd_hlp_a),
            0x37 => cpu.call0(inst.scf),
            0x3C => cpu.call0(inst.inc_a),
            0x3D => cpu.call0(inst.dec_a),
            0x3E => cpu.call1(inst.ld_a),

            0x41 => cpu.call0(inst.ld_b_c),
            0x42 => cpu.call0(inst.ld_b_d),
            0x43 => cpu.call0(inst.ld_b_e),
            0x44 => cpu.call0(inst.ld_b_h),
            0x45 => cpu.call0(inst.ld_b_l),
            0x47 => cpu.call0(inst.ld_b_a),
            0x48 => cpu.call0(inst.ld_c_b),
            0x4A => cpu.call0(inst.ld_c_d),
            0x4B => cpu.call0(inst.ld_c_e),
            0x4C => cpu.call0(inst.ld_c_h),
            0x4D => cpu.call0(inst.ld_c_l),
            0x4F => cpu.call0(inst.ld_c_a),

            0x50 => cpu.call0(inst.ld_d_b),
            0x51 => cpu.call0(inst.ld_d_c),
            0x53 => cpu.call0(inst.ld_d_e),
            0x54 => cpu.call0(inst.ld_d_h),
            0x55 => cpu.call0(inst.ld_d_l),
            0x57 => cpu.call0(inst.ld_d_a),
            0x58 => cpu.call0(inst.ld_e_b),
            0x59 => cpu.call0(inst.ld_e_c),
            0x5A => cpu.call0(inst.ld_e_d),
            0x5C => cpu.call0(inst.ld_e_h),
            0x5D => cpu.call0(inst.ld_e_l),
            0x5F => cpu.call0(inst.ld_e_a),

            0x60 => cpu.call0(inst.ld_h_b),
            0x61 => cpu.call0(inst.ld_h_c),
            0x62 => cpu.call0(inst.ld_h_d),
            0x63 => cpu.call0(inst.ld_h_e),
            0x65 => cpu.call0(inst.ld_h_l),
            0x67 => cpu.call0(inst.ld_h_a),
            0x68 => cpu.call0(inst.ld_l_b),
            0x69 => cpu.call0(inst.ld_l_c),
            0x6A => cpu.call0(inst.ld_l_d),
            0x6B => cpu.call0(inst.ld_l_e),
            0x6C => cpu.call0(inst.ld_l_h),
            0x6F => cpu.call0(inst.ld_l_a),

            0x78 => cpu.call0(inst.ld_a_b),
            0x79 => cpu.call0(inst.ld_a_c),
            0x7A => cpu.call0(inst.ld_a_d),
            0x7B => cpu.call0(inst.ld_a_e),
            0x7C => cpu.call0(inst.ld_a_h),
            0x7D => cpu.call0(inst.ld_a_l),
            0x88 => cpu.call0(inst.adc_b),
            0x89 => cpu.call0(inst.adc_c),
            0x8A => cpu.call0(inst.adc_d),
            0x8B => cpu.call0(inst.adc_e),
            0x8C => cpu.call0(inst.adc_h),
            0x8D => cpu.call0(inst.adc_l),

            0xAF => cpu.call0(inst.xor_a),

            0xC3 => cpu.call2(inst.jp),

            // 0xDF => cpu.call0(inst.rst_18),
            0xD9 => cpu.call0(inst.reti),

            0xE0 => cpu.call1(inst.ld_ff_n_ap),

            0xFF => cpu.call0(inst.rst_38),

            else => cpu.unknownInstruction(),
        }

        cpu.ticks += inst.tick_table[opcode];
    }

    fn call0(cpu: &Cpu, comptime func: fn(&Cpu)) {
        @inlineCall(func, cpu);
    }

    fn call1(cpu: &Cpu, comptime func: fn(&Cpu, u8)) {
        const operand = cpu.mem.read8(cpu.regs.pc);
        cpu.regs.pc += 1;
        @inlineCall(func, cpu, operand);
    }

    fn call2(cpu: &Cpu, comptime func: fn(&Cpu, u16)) {
        const operand = cpu.mem.read16(cpu.regs.pc);
        cpu.regs.pc += 2;
        @inlineCall(func, cpu, operand);
    }

    pub fn pushStack16(cpu: &Cpu, value: u16) {
        cpu.regs.sp -= 2;
        cpu.mem.write16(cpu.regs.sp, value);
    }

    pub fn popStack16(cpu: &Cpu) -> u16 {
        const value = cpu.mem.read16(cpu.regs.sp);
        cpu.regs.sp += 2;
        value
    }

    pub fn debugPrint(cpu: &const Cpu) -> %void {
        // NOTE: Evaluation exceeded 1000 backwards branches if all one printf.
        %return printf(
            \\last opcode  : {X2}
            \\ticks        : {}
            \\registers
            \\  a     : {X2}
            \\  b     : {X2}
            \\  c     : {X2}
            \\  d     : {X2}
            \\  e     : {X2}
            \\  h     : {X2}
            \\  l     : {X2}
            \\  flags : {X2}
            \\  sp    : {X4}
            \\  pc    : {X4}
            \\
            , cpu.mem.read8(cpu.regs.pc -% 1)
            , cpu.ticks
            , cpu.regs.a
            , cpu.regs.b
            , cpu.regs.c
            , cpu.regs.d
            , cpu.regs.e
            , cpu.regs.h
            , cpu.regs.l
            , cpu.regs.flags
            , cpu.regs.sp
            , cpu.regs.pc
        );
    }

    fn unknownInstruction(cpu: &const Cpu) -> noreturn {
        %%printf("\n<== unknown instruction! ==>\n");
        %%cpu.debugPrint();
        std.os.exit(1);
    }
};
