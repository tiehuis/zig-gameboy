const cpuz = @import("cpu.zig");
const Cpu = cpuz.Cpu;
const Flags = cpuz.Flags;

pub const tick_table = []const u8 {
    2, 6, 4, 4, 2, 2, 4, 4, 10, 4, 4, 4, 2, 2, 4, 4, // 0x0_
    2, 6, 4, 4, 2, 2, 4, 4,  4, 4, 4, 4, 2, 2, 4, 4, // 0x1_
    0, 6, 4, 4, 2, 2, 4, 2,  0, 4, 4, 4, 2, 2, 4, 2, // 0x2_
    4, 6, 4, 4, 6, 6, 6, 2,  0, 4, 4, 4, 2, 2, 4, 2, // 0x3_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0x4_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0x5_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0x6_
    4, 4, 4, 4, 4, 4, 2, 4,  2, 2, 2, 2, 2, 2, 4, 2, // 0x7_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0x8_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0x9_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0xA_
    2, 2, 2, 2, 2, 2, 4, 2,  2, 2, 2, 2, 2, 2, 4, 2, // 0xB_
    0, 6, 0, 6, 0, 8, 4, 8,  0, 2, 0, 0, 0, 6, 4, 8, // 0xC_
    0, 6, 0, 0, 0, 8, 4, 8,  0, 8, 0, 0, 0, 0, 4, 8, // 0xD_
    6, 6, 4, 0, 0, 8, 4, 8,  8, 2, 8, 0, 0, 0, 4, 8, // 0xE_
    6, 6, 4, 2, 0, 8, 4, 8,  6, 4, 8, 2, 0, 0, 4, 8  // 0xF_
};

fn adc(cpu: &Cpu, value: u8) {
    const carry_flag = if (cpu.regs.flags & Flags.Carry != 0) u8(1) else 0;
    const mvalue = value +% carry_flag;
    const result = u16(cpu.regs.a) +% mvalue;

    if (result & 0xFF00 != 0) {
        cpu.regs.flags |= Flags.Carry;
    } else {
        cpu.regs.flags &= ~Flags.Carry;
    }

    if (mvalue == cpu.regs.a) {
        cpu.regs.flags |= Flags.Zero;
    } else {
        cpu.regs.flags &= ~Flags.Zero;
    }

    if ((mvalue & 0x0F) + (cpu.regs.a & 0x0F) > 0x0F) {
        cpu.regs.flags |= Flags.HalfCarry;
    } else {
        cpu.regs.flags &= ~Flags.HalfCarry;
    }

    cpu.regs.flags |= Flags.Negative;
    cpu.regs.a = u8(result & 0xFF);
}

fn xor(cpu: &Cpu, value: u8) {
    cpu.regs.a ^= value;

    if (cpu.regs.a != 0) {
        cpu.regs.flags &= ~Flags.Zero;
    } else {
        cpu.regs.flags |= Flags.Zero;
    }

    cpu.regs.flags &= ~(Flags.Carry | Flags.Negative);
    cpu.regs.flags |= Flags.HalfCarry;
}

fn inc(cpu: &Cpu, value: u8) -> u8 {
    if (value & 0x0F != 0x0F) {
        cpu.regs.flags &= ~Flags.HalfCarry;
    } else {
        cpu.regs.flags |= Flags.HalfCarry;
    }

    var result = value +% 1;
    if (result != 0) {
        cpu.regs.flags &= ~Flags.Zero;
    } else {
        cpu.regs.flags |= Flags.Zero;
    }

    cpu.regs.flags |= Flags.Negative;
    result
}

fn dec(cpu: &Cpu, value: u8) -> u8 {
    if (value & 0x0F != 0) {
        cpu.regs.flags &= ~Flags.HalfCarry;
    } else {
        cpu.regs.flags |= Flags.HalfCarry;
    }

    var result = value -% 1;
    if (result != 0) {
        cpu.regs.flags &= ~Flags.Zero;
    } else {
        cpu.regs.flags |= Flags.Zero;
    }

    cpu.regs.flags |= Flags.Negative;
    result
}

// 0x00
pub fn nop(cpu: &Cpu) {}

// 0x05
pub fn dec_b(cpu: &Cpu) {
    cpu.regs.b = dec(cpu, cpu.regs.b);
}

// 0x06
pub fn ld_b(cpu: &Cpu, op: u8) {
    cpu.regs.b = op;
}

// 0x14
pub fn inc_d(cpu: &Cpu) {
    cpu.regs.d = inc(cpu, cpu.regs.d);
}

// 0x15
pub fn dec_d(cpu: &Cpu) {
    cpu.regs.d = dec(cpu, cpu.regs.d);
}

// 0x1F
pub fn rra(cpu: &Cpu) {
    const carry = if (cpu.regs.flags & Flags.Carry != 0) u8(0b10000000) else 0;

    if (cpu.regs.a & 0x01 != 0) {
        cpu.regs.flags |= Flags.Carry;
    } else {
        cpu.regs.flags &= ~Flags.Carry;
    }

    cpu.regs.a >>= 1;
    cpu.regs.a = cpu.regs.a +% carry;
    cpu.regs.flags &= ~(Flags.Negative | Flags.Zero | Flags.HalfCarry);
}

// 0x20
pub fn jr_nz(cpu: &Cpu, op: u8) {
    if (cpu.regs.flags & Flags.Zero != 0) {
        cpu.ticks += 8;
    } else {
        cpu.regs.pc += op;
        cpu.ticks += 12;
    }
}

// 0x21
pub fn ld_hl(cpu: &Cpu, op: u16) {
    // TODO: Use a union bitfield here.
    cpu.regs.h = u8(op >> 8);
    cpu.regs.l = u8(op & 0xFF);
}

// 0x32
pub fn ldd_hlp_a(cpu: &Cpu) {
    var hl = (u16(cpu.regs.h) << 8) | u16(cpu.regs.l);
    cpu.mem.write8(hl, cpu.regs.a);
    hl = hl -% 1;
    cpu.regs.h = u8(hl >> 8);
    cpu.regs.l = u8(hl & 0xFF);
}

// 0x78
pub fn ld_a_b(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.b;
}

// 0x79
pub fn ld_a_c(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.c;
}

// 0x7A
pub fn ld_a_d(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.d;
}

// 0x7B
pub fn ld_a_e(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.e;
}

// 0x7C
pub fn ld_a_h(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.h;
}

// 0x7D
pub fn ld_a_l(cpu: &Cpu) {
    cpu.regs.a = cpu.regs.l;
}

// 0x88
pub fn adc_b(cpu: &Cpu) {
    adc(cpu, cpu.regs.b);
}

// 0x89
pub fn adc_c(cpu: &Cpu) {
    adc(cpu, cpu.regs.c);
}

// 0x8A
pub fn adc_d(cpu: &Cpu) {
    adc(cpu, cpu.regs.d);
}

// 0x8B
pub fn adc_e(cpu: &Cpu) {
    adc(cpu, cpu.regs.e);
}

// 0x8C
pub fn adc_h(cpu: &Cpu) {
    adc(cpu, cpu.regs.h);
}

// 0x8D
pub fn adc_l(cpu: &Cpu) {
    adc(cpu, cpu.regs.l);
}

// 0x0E
pub fn ld_c(cpu: &Cpu, op: u8) {
    cpu.regs.c = op;
}

// 0xC3
pub fn jp(cpu: &Cpu, op: u16) {
    cpu.regs.pc = op;
}

// 0xAF
pub fn xor_a(cpu: &Cpu) {
    xor(cpu, cpu.regs.a);
}

// 0xDF
pub fn rst_18(cpu: &Cpu) {
    cpu.pushStack16(cpu.regs.pc);
    cpu.regs.pc = 0x0018;
}

// 0xE0
pub fn ld_ff_n_ap(cpu: &Cpu, op: u8) {
    cpu.mem.write8(0xFF00 + u16(op), cpu.regs.a);
}

// 0xFF
pub fn rst_38(cpu: &Cpu) {
    cpu.pushStack16(cpu.regs.pc);
    cpu.regs.pc = 0x0038;
}
