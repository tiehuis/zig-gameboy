const std = @import("std");
const Mem = @import("mem.zig").Mem;
const printf = @import("std").io.stdout.printf;

const Registers = struct {
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

    // Fetch, decode, execute a single instruction.
    pub fn step(cpu: &Cpu) {
        const inst = cpu.mem.read8(cpu.regs.pc);
        cpu.regs.pc += 1;

        switch (inst) {
            else => cpu.unknownInstruction(),
        }
    }

    pub fn debugPrint(cpu: &const Cpu) -> %void {
        // NOTE: Evaluation exceeded 1000 backwards branches if all one printf.
        %return printf(
            \\instruction : {}
            \\ticks       : {}
            \\registers:
            \\  a     : {}
            \\  b     : {}
            \\  c     : {}
            \\  d     : {}
            \\  e     : {}
            \\  h     : {}
            \\  l     : {}
            \\  flags : {}
            \\  sp    : {}
            \\  pc    : {}
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
