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
    f: u8,
    sp: u16,
    pc: u16,
};

pub const Cpu = struct {
    ticks: u64,
    regs: Registers,
    mem: Mem,

    pub fn init() -> Cpu {
        Cpu {
            .ticks = 0,
            .regs = undefined,
            .mem = undefined,
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
        cpu.regs.f  = 0xB0;
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

    pub fn debugPrint(cpu: &const Cpu) -> %void {
        // NOTE: Evaluation exceeded 1000 backwards branches if all one printf.
        %return printf(
            \\last opcode  : {X}
            \\ticks        : {}
            \\registers
            \\  a     : {X}
            \\  b     : {X}
            \\  c     : {X}
            \\  d     : {X}
            \\  e     : {X}
            \\  h     : {X}
            \\  l     : {X}
            \\  flags : {X}
            \\  sp    : {X}
            \\  pc    : {X}
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
            , cpu.regs.f
            , cpu.regs.sp
            , cpu.regs.pc
        );
    }

    fn unknownInstruction(cpu: &const Cpu) -> noreturn {
        %%printf("\n<== unknown instruction! ==>\n");
        %%cpu.debugPrint();
        std.os.abort();
    }
};
