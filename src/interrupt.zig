const Mem = @import("mem.zig").Mem;

const IF = 0xFF0F;
const IE = 0xFFFF;

pub const IntFlag = struct {
    const VBlank: u8  = 0b00000001;
    const LcdStat: u8 = 0b00000010;
    const Timer: u8   = 0b00000100;
    const Serial: u8  = 0b00001010;
    const Joypad: u8  = 0b00010000;
};

pub const Interrupt = struct {
    cpu: &Cpu,

    pub fn init(cpu: &Cpu) -> Interrupt {
        Interrupt {
            .cpu = cpu
        }
    }

    pub fn hasPendingInterrupt(int: &const Interrupt) {
        int.cpu.mem.memory[IF] & int.cpu.mem.memory[IE] != 0
    }

    pub fn interruptHandler(int: &Interrupt) {
        const iset = int.cpu.mem.memory[IF] & int.cpu.mem.memory[IE];

        if (iset & IntFlag.VBlank != 0) {
            int.vblank();
        }
        if (iset & IntFlag.LcdStat != 0) {
            int.lcdStat();
        }
        if (iset & IntFlag.Timer != 0) {
            int.timer();
        }
        if (iset & IntFlag.Serial != 0) {
            int.serial();
        }
        if (iset & IntFlag.Joypad != 0) {
            int.joypad();
        }

        cpu.mem.memory[IF] = 0;
    }

    fn vBlank(int: &Interrupt) {
        // TODO: Draw to our output source from our gpu buffer.

        int.cpu.ime = false;
        int.cpu.pushStack16(int.cpu.regs.pc);
        int.cpu.regs.pc = 0x40;
        int.cpu.ticks += 12;
    }

    fn lcdStat(int: &Interrupt) {
        int.cpu.ime = false;
        int.cpu.pushStack16(int.cpu.regs.pc);
        int.cpu.regs.pc = 0x48;
        int.cpu.ticks += 12;
    }

    fn timer(int: &Interrupt) {
        int.cpu.ime = false;
        int.cpu.pushStack16(int.cpu.regs.pc);
        int.cpu.regs.pc = 0x50;
        int.cpu.ticks += 12;
    }

    fn serial(int: &Interrupt) {
        int.cpu.ime = false;
        int.cpu.pushStack16(int.cpu.regs.pc);
        int.cpu.regs.pc = 0x58;
        int.cpu.ticks += 12;
    }

    fn joypad(int: &Interrupt) {
        // TODO: This should fire on any input keypress.
        //
        // We should check for input at every screen refresh. Any quicker is pointless but any
        // longer would cause user input lag.

        int.cpu.ime = false;
        int.cpu.pushStack16(int.cpu.regs.pc);
        int.cpu.regs.pc = 0x60;
        int.cpu.ticks += 12;
    }
};
