use crate::{
    instr::{Addr16, Addr8, Cond, Fetch, Op, Reg16, Reg8},
    mem::Mem,
};

//
// Statics
//

const CARRY_OFFSET: usize = 4;
const HALF_CARRY_OFFSET: usize = 5;
const ADD_SUB_OFFSET: usize = 6;
const ZERO_OFFSET: usize = 7;

const CARRY_FLAG: u8 = 1 << CARRY_OFFSET;
const HALF_CARRY_FLAG: u8 = 1 << HALF_CARRY_OFFSET;
const ADD_SUB_FLAG: u8 = 1 << ADD_SUB_OFFSET;
const ZERO_FLAG: u8 = 1 << ZERO_OFFSET;

pub const CYCLES_PER_SEC: usize = 4_194_304; // 4.194304 MHz

//
// Registers
//

pub struct Regs {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub h: u8,
    pub l: u8,
    pub f: u8,
    pub sp: u16,
    pub pc: u16,
}

impl Regs {
    fn new() -> Regs {
        Regs {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            f: 0,
            sp: 0,
            pc: 0,
        }
    }
}

trait CpuReg<T> {
    fn load(self, regs: &Regs) -> T;
    fn store(self, regs: &mut Regs, val: T);
}

impl CpuReg<u8> for Reg8 {
    fn load(self, regs: &Regs) -> u8 {
        match self {
            Reg8::A => regs.a,
            Reg8::B => regs.b,
            Reg8::C => regs.c,
            Reg8::D => regs.d,
            Reg8::E => regs.e,
            Reg8::H => regs.h,
            Reg8::L => regs.l,
        }
    }

    fn store(self, regs: &mut Regs, val: u8) {
        match self {
            Reg8::A => regs.a = val,
            Reg8::B => regs.b = val,
            Reg8::C => regs.c = val,
            Reg8::D => regs.d = val,
            Reg8::E => regs.e = val,
            Reg8::H => regs.h = val,
            Reg8::L => regs.l = val,
        }
    }
}

impl CpuReg<u16> for Reg16 {
    fn load(self, regs: &Regs) -> u16 {
        match self {
            Reg16::AF => (u16::from(regs.a) << 8) | u16::from(regs.f),
            Reg16::BC => (u16::from(regs.b) << 8) | u16::from(regs.c),
            Reg16::DE => (u16::from(regs.d) << 8) | u16::from(regs.e),
            Reg16::HL => (u16::from(regs.h) << 8) | u16::from(regs.l),
            Reg16::SP => regs.sp,
            Reg16::PC => regs.pc,
        }
    }

    fn store(self, regs: &mut Regs, val: u16) {
        match self {
            Reg16::AF => {
                regs.a = (val >> 8) as u8;
                regs.f = val as u8 & 0xf0
            }
            Reg16::BC => {
                regs.b = (val >> 8) as u8;
                regs.c = val as u8
            }
            Reg16::DE => {
                regs.d = (val >> 8) as u8;
                regs.e = val as u8
            }
            Reg16::HL => {
                regs.h = (val >> 8) as u8;
                regs.l = val as u8
            }
            Reg16::SP => regs.sp = val,
            Reg16::PC => regs.pc = val,
        }
    }
}

trait CpuAddr<T> {
    fn cycles(self) -> u8;
    fn load(self, cpu: &mut Cpu, mem: &dyn Mem) -> T;
    fn store(self, cpu: &mut Cpu, mem: &mut dyn Mem, val: T);
}

impl CpuAddr<u8> for Addr8 {
    fn cycles(self) -> u8 {
        // Every (byte) memory access costs 4 cycles
        match self {
            Addr8::Reg8Dir(_) => 0, // register access is "free"
            Addr8::Imm(_) | Addr8::Ind(_) | Addr8::Reg8Ind(_) => 4, // one memory access
            Addr8::Reg16Ind(_) | Addr8::Reg16IndInc(_) | Addr8::Reg16IndDec(_) => 8, // two memory accesses
            Addr8::Imm16Ind(_) => 12, // three memory accesses
        }
    }

    fn load(self, cpu: &mut Cpu, mem: &dyn Mem) -> u8 {
        match self {
            Addr8::Imm(val) => val,
            Addr8::Ind(offset) => mem.loadb(0xff00 + u16::from(offset)),
            Addr8::Imm16Ind(addr) => mem.loadb(addr),
            Addr8::Reg8Dir(r) => r.load(&cpu.regs),
            Addr8::Reg8Ind(r) => {
                let offset = r.load(&cpu.regs);
                mem.loadb(0xff00 + u16::from(offset))
            }
            Addr8::Reg16Ind(r) | Addr8::Reg16IndInc(r) | Addr8::Reg16IndDec(r) => {
                let addr = r.load(&cpu.regs);
                let result = mem.loadb(addr);
                match self {
                    Addr8::Reg16IndInc(_) => r.store(&mut cpu.regs, addr + 1),
                    Addr8::Reg16IndDec(_) => r.store(&mut cpu.regs, addr - 1),
                    _ => (),
                }
                result
            }
        }
    }

    fn store(self, cpu: &mut Cpu, mem: &mut dyn Mem, val: u8) {
        match self {
            Addr8::Ind(offset) => mem.storeb(0xff00 + u16::from(offset), val),
            Addr8::Imm16Ind(addr) => mem.storeb(addr, val),
            Addr8::Reg8Dir(r) => r.store(&mut cpu.regs, val),
            Addr8::Reg8Ind(r) => {
                let offset = r.load(&cpu.regs);
                mem.storeb(0xff00 + u16::from(offset), val)
            }
            Addr8::Reg16Ind(r) | Addr8::Reg16IndInc(r) | Addr8::Reg16IndDec(r) => {
                let addr = r.load(&cpu.regs);
                mem.storeb(addr, val);
                match self {
                    Addr8::Reg16IndInc(_) => r.store(&mut cpu.regs, addr + 1),
                    Addr8::Reg16IndDec(_) => r.store(&mut cpu.regs, addr - 1),
                    _ => (),
                }
            }
            _ => panic!("invalid addressing mode for 8-bit store"),
        }
    }
}

impl CpuAddr<u16> for Addr16 {
    fn cycles(self) -> u8 {
        // Every (byte) memory access costs 4 cycles
        match self {
            Addr16::Reg16Dir(_) => 0, // register access is "free"
            Addr16::Imm(_) => 8,      // two memory accesses
            Addr16::Ind(_) => 16,     // four memory accesses
        }
    }

    fn load(self, cpu: &mut Cpu, mem: &dyn Mem) -> u16 {
        match self {
            Addr16::Imm(val) => val,
            Addr16::Ind(addr) => mem.loadw(addr),
            Addr16::Reg16Dir(r) => r.load(&cpu.regs),
        }
    }

    fn store(self, cpu: &mut Cpu, mem: &mut dyn Mem, val: u16) {
        match self {
            Addr16::Ind(addr) => mem.storew(addr, val),
            Addr16::Reg16Dir(r) => r.store(&mut cpu.regs, val),
            _ => panic!("invalid addressing mode for 16-bit store"),
        }
    }
}

trait CpuCond {
    fn eval(self, regs: &Regs) -> bool;
}

impl CpuCond for Cond {
    fn eval(self, regs: &Regs) -> bool {
        match self {
            Cond::None => true,
            Cond::Z => (regs.f & ZERO_FLAG) != 0,
            Cond::NZ => (regs.f & ZERO_FLAG) == 0,
            Cond::C => (regs.f & CARRY_FLAG) != 0,
            Cond::NC => (regs.f & CARRY_FLAG) == 0,
        }
    }
}

//
// CPU
//

pub struct Cpu {
    pub regs: Regs,
    ime: bool,
    halted: bool,
    pub cycles: u64,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            regs: Regs::new(),
            ime: false, // TODO: Are interrupts enabled at boot?
            halted: false,
            cycles: 0u64,
        }
    }

    pub fn step(&mut self, mem: &mut dyn Mem) -> u8 {
        if self.halted {
            if mem.loadb(0xff0f) != 0 {
                // Wake up on interrupt
                self.halted = false;
            } else {
                // NOP
                self.cycles += 4;
                return 4;
            }
        }

        // Check for interrupts
        if self.ime {
            let enabled_mask = mem.loadb(0xffff);
            let request_mask = mem.loadb(0xff0f);
            let effective = enabled_mask & request_mask;
            // Get the lowest set bit, which is the IRQ with the highest priority
            let irq = effective.trailing_zeros();
            if irq < 5 {
                // Valid IRQ (0-4)
                self.execute(Op::Push(Addr16::Reg16Dir(Reg16::PC)), mem); // Push PC
                self.regs.pc = 0x40 + irq as u16 * 0x08; // Jump to ISR
                self.ime = false; // Disable interrupts
                mem.storeb(0xff0f, request_mask & !(1 << irq)); // Clear IRQ flag
                self.cycles += 20;
                return 20; // 5 machine cycles, according to GBCPUman.pdf
            }
        }

        let op = Op::decode(&mut CpuMem { cpu: self, mem });

        let elapsed = self.execute(op, mem);

        self.cycles += u64::from(elapsed);
        elapsed
    }

    // Flags helpers

    fn get_flag(&self, flag: u8) -> bool {
        (self.regs.f & flag) != 0
    }

    fn set_flag(&mut self, flag: u8, on: bool) {
        if on {
            self.regs.f |= flag;
        } else {
            self.regs.f &= !flag;
        }
    }

    // Addition helper
    fn add_(&mut self, a: u8, b: u8, c: u8) {
        let result = u32::from(a)
            .wrapping_add(u32::from(b))
            .wrapping_add(u32::from(c));
        self.regs.a = result as u8;
        let zero = self.regs.a == 0;
        self.set_flag(ZERO_FLAG, zero);
        self.set_flag(ADD_SUB_FLAG, false);
        self.set_flag(
            HALF_CARRY_FLAG,
            ((a & 0xf).wrapping_add(b & 0xf).wrapping_add(c) & 0x10) != 0,
        );
        self.set_flag(CARRY_FLAG, (result & 0x100) != 0);
    }

    // Subtraction helper
    fn sub_(&mut self, a: u8, b: u8, c: u8) {
        let result = (u32::from(a))
            .wrapping_sub(u32::from(b))
            .wrapping_sub(u32::from(c));
        self.regs.a = result as u8;
        let zero = self.regs.a == 0;
        self.set_flag(ZERO_FLAG, zero);
        self.set_flag(ADD_SUB_FLAG, true);
        self.set_flag(
            HALF_CARRY_FLAG,
            ((a & 0xf).wrapping_sub(b & 0xf).wrapping_sub(c) & 0x10) != 0,
        ); // ???
        self.set_flag(CARRY_FLAG, (result & 0x100) != 0); // ???
    }

    // Add to SP helper
    fn addsp_(&mut self, rel: i8) -> u16 {
        let sp = self.regs.sp;
        let rel = rel as u16;
        let result = sp.wrapping_add(rel);
        self.set_flag(ZERO_FLAG, false);
        self.set_flag(ADD_SUB_FLAG, false);
        self.set_flag(HALF_CARRY_FLAG, (((sp & 0xf) + (rel & 0xf)) & 0x10) != 0);
        self.set_flag(CARRY_FLAG, (((sp & 0xff) + (rel & 0xff)) & 0x100) != 0);
        result
    }

    // Opcode implementation.
    //
    // Returns number of elapsed cyles.
    //
    // Sources:
    //   * http://www.z80.info/z80code.txt
    //   * http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf
    //   * http://www.zilog.com/docs/z80/um0080.pdf
    fn execute(&mut self, op: Op, mem: &mut dyn Mem) -> u8 {
        match op {
            //
            // Misc/control
            //
            Op::Nop => 4,

            Op::Ei => {
                self.ime = true;
                4
            }

            Op::Di => {
                self.ime = false;
                4
            }

            Op::Halt => {
                self.halted = true;
                4
            }

            Op::Stop(_) => {
                //panic!("instruction not implemented: stop")
                4
            }

            //
            // Jump/call
            //
            Op::Jp(cond, addr) => {
                if cond.eval(&self.regs) {
                    self.execute(Op::Ld16(Addr16::Reg16Dir(Reg16::PC), addr), mem);
                }
                4 + addr.cycles()
            }

            Op::Jr(cond, rel) => {
                if cond.eval(&self.regs) {
                    let addr = self.regs.pc as i16 + i16::from(rel);
                    self.execute(
                        Op::Ld16(Addr16::Reg16Dir(Reg16::PC), Addr16::Imm(addr as u16)),
                        mem,
                    );
                }
                8
            }

            Op::Call(cond, addr) => {
                if cond.eval(&self.regs) {
                    self.execute(Op::Push(Addr16::Reg16Dir(Reg16::PC)), mem);
                    self.execute(Op::Ld16(Addr16::Reg16Dir(Reg16::PC), addr), mem);
                }
                12
            }

            Op::Rst(addr) => {
                self.execute(Op::Call(Cond::None, Addr16::Imm(u16::from(addr))), mem);
                32
            }

            Op::Ret(cond) => {
                if cond.eval(&self.regs) {
                    self.execute(Op::Pop(Addr16::Reg16Dir(Reg16::PC)), mem);
                }
                8
            }

            Op::Reti => {
                self.execute(Op::Pop(Addr16::Reg16Dir(Reg16::PC)), mem);
                self.execute(Op::Ei, mem);
                8
            }

            //
            // Load/store/move
            //
            Op::Ld8(dst, src) => {
                let val = src.load(self, mem);
                dst.store(self, mem, val);
                4 + dst.cycles() + src.cycles()
            }

            Op::Ld16(dst, src) => {
                let val = src.load(self, mem);
                dst.store(self, mem, val);
                4 + dst.cycles() + src.cycles()
                // TODO: LD SP, HL takes 8 cycles
            }

            Op::Ldh(dst, src) => {
                self.execute(Op::Ld8(dst, src), mem);
                4 + dst.cycles() + src.cycles()
            }

            Op::Ldhl(rel) => {
                let result = self.addsp_(rel);
                self.regs.h = (result >> 8) as u8;
                self.regs.l = (result & 0xff) as u8;
                12
            }

            Op::Push(src) => {
                self.regs.sp -= 2;
                let val = src.load(self, mem);
                mem.storew(self.regs.sp, val);
                16 + src.cycles()
            }

            Op::Pop(dst) => {
                let val = mem.loadw(self.regs.sp);
                dst.store(self, mem, val);
                self.regs.sp += 2;
                12 + dst.cycles()
            }

            //
            // Arithmetic/logic
            //
            Op::Add8(src) => {
                let a = self.regs.a;
                let b = src.load(self, mem);
                self.add_(a, b, 0);

                4 + src.cycles()
            }

            Op::Add16(dst, src) => {
                let op1 = u32::from(dst.load(self, mem));
                let op2 = u32::from(src.load(self, mem));
                let result = op1 + op2;
                dst.store(self, mem, result as u16);

                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(
                    HALF_CARRY_FLAG,
                    (((op1 & 0xfff) + (op2 & 0xfff)) & 0x1000) != 0,
                );
                self.set_flag(CARRY_FLAG, (result & 0x1_0000) != 0);

                8
            }

            Op::Addsp(rel) => {
                self.regs.sp = self.addsp_(rel);

                16
            }

            Op::Adc(src) => {
                let a = self.regs.a;
                let b = src.load(self, mem);
                let c = (self.regs.f >> CARRY_OFFSET) & 1;
                self.add_(a, b, c);

                4 + src.cycles()
            }

            Op::Sub(src) => {
                let a = self.regs.a;
                let b = src.load(self, mem);
                self.sub_(a, b, 0);

                4 + src.cycles()
            }

            Op::Sbc(src) => {
                let a = self.regs.a;
                let b = src.load(self, mem);
                let c = (self.regs.f >> CARRY_OFFSET) & 1;
                self.sub_(a, b, c);

                4 + src.cycles()
            }

            Op::Inc8(dst) => {
                let val = dst.load(self, mem);
                let result = val.wrapping_add(1);
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, val & 0xf == 0xf);
                // Note: carry flag not affected

                4 + 2 * dst.cycles()
            }

            Op::Inc16(dst) => {
                let val = dst.load(self, mem);
                dst.store(self, mem, val.wrapping_add(1));

                8
            }

            Op::Dec8(dst) => {
                let result = dst.load(self, mem).wrapping_sub(1);
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, true);
                self.set_flag(HALF_CARRY_FLAG, result & 0xf == 0xf);
                // Note: carry flag is not affected

                4 + 2 * dst.cycles()
            }

            Op::Dec16(dst) => {
                let val = dst.load(self, mem);
                dst.store(self, mem, val.wrapping_sub(1));

                8
            }

            Op::And(src) => {
                self.regs.a &= src.load(self, mem);

                let zero = self.regs.a == 0;
                self.set_flag(ZERO_FLAG, zero);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, true); // Yes, this is correct
                self.set_flag(CARRY_FLAG, false);

                4 + src.cycles()
            }

            Op::Or(src) => {
                self.regs.a |= src.load(self, mem);

                let zero = self.regs.a == 0;
                self.set_flag(ZERO_FLAG, zero);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, false);

                4 + src.cycles()
            }

            Op::Xor(src) => {
                self.regs.a ^= src.load(self, mem);

                let zero = self.regs.a == 0;
                self.set_flag(ZERO_FLAG, zero);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, false);

                4 + src.cycles()
            }

            Op::Cp(src) => {
                // TODO: Optimize
                let a = self.regs.a;
                let b = src.load(self, mem);
                self.sub_(a, b, 0);
                self.regs.a = a;

                4 + src.cycles()
            }

            Op::Cpl => {
                self.regs.a = !self.regs.a;

                self.set_flag(ADD_SUB_FLAG, true);
                self.set_flag(HALF_CARRY_FLAG, true);

                4
            }

            Op::Scf => {
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, true);

                4
            }

            Op::Ccf => {
                self.regs.f ^= CARRY_FLAG;
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);

                4
            }

            Op::Daa => {
                // http://forums.nesdev.com/viewtopic.php?t=9088
                let mut a = u16::from(self.regs.a);

                if self.get_flag(ADD_SUB_FLAG) {
                    if self.get_flag(HALF_CARRY_FLAG) {
                        a = a.wrapping_sub(0x06) & 0xff;
                    }
                    if self.get_flag(CARRY_FLAG) {
                        a = a.wrapping_sub(0x60);
                    }
                } else {
                    if self.get_flag(HALF_CARRY_FLAG) || (a & 0xf) > 9 {
                        a = a.wrapping_add(0x06);
                    }
                    if self.get_flag(CARRY_FLAG) || a > 0x9f {
                        a = a.wrapping_add(0x60);
                    }
                }

                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(ZERO_FLAG, false);

                if (a & 0x100) != 0 {
                    self.set_flag(CARRY_FLAG, true);
                }

                a &= 0xff;

                if a == 0 {
                    self.set_flag(ZERO_FLAG, true);
                }

                self.regs.a = a as u8;

                4
            }

            //
            // Rotation/shift/bit
            //
            Op::Rlca => {
                self.execute(Op::Rlc(Addr8::Reg8Dir(Reg8::A)), mem);

                self.set_flag(ZERO_FLAG, false);

                4
            }

            Op::Rla => {
                self.execute(Op::Rl(Addr8::Reg8Dir(Reg8::A)), mem);

                self.set_flag(ZERO_FLAG, false);

                4
            }

            Op::Rrca => {
                self.execute(Op::Rrc(Addr8::Reg8Dir(Reg8::A)), mem);

                self.set_flag(ZERO_FLAG, false);

                4
            }

            Op::Rra => {
                self.execute(Op::Rr(Addr8::Reg8Dir(Reg8::A)), mem);

                self.set_flag(ZERO_FLAG, false);

                4
            }

            Op::Rlc(dst) => {
                let val = dst.load(self, mem);
                dst.store(self, mem, (val << 1) | ((val & 0x80) >> 7));

                self.set_flag(ZERO_FLAG, val == 0); // zero iff zero before
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x80) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Rl(dst) => {
                let val = dst.load(self, mem);
                let result = (val << 1) | ((self.regs.f & CARRY_FLAG) >> CARRY_OFFSET);
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x80) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Rrc(dst) => {
                let val = dst.load(self, mem);
                dst.store(self, mem, (val >> 1) | ((val & 0x01) << 7));

                self.set_flag(ZERO_FLAG, val == 0); // zero iff zero before
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x01) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Rr(dst) => {
                let val = dst.load(self, mem);
                let result = (val >> 1) | ((self.regs.f & CARRY_FLAG) << (7 - CARRY_OFFSET));
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x01) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Sla(dst) => {
                let val = dst.load(self, mem);
                let result = val << 1;
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x80) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Sra(dst) => {
                let val = dst.load(self, mem);
                let result = (val & 0x80) | (val >> 1);
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x01) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Srl(dst) => {
                let val = dst.load(self, mem);
                let result = val >> 1;
                dst.store(self, mem, result);

                self.set_flag(ZERO_FLAG, result == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, (val & 0x01) != 0);

                8 + 2 * dst.cycles()
            }

            Op::Bit(bit, src) => {
                let val = src.load(self, mem);

                self.set_flag(ZERO_FLAG, (val & (1 << bit)) == 0);
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, true);

                8 + src.cycles() // TODO: BIT b, (HL) takes 16 cycles
            }

            Op::Res(bit, dst) => {
                let val = dst.load(self, mem);

                dst.store(self, mem, val & !(1 << bit));

                8 + 2 * dst.cycles()
            }

            Op::Set(bit, dst) => {
                let val = dst.load(self, mem);

                dst.store(self, mem, val | (1 << bit));

                8 + 2 * dst.cycles()
            }

            Op::Swap(dst) => {
                let val = dst.load(self, mem);
                dst.store(self, mem, val.rotate_left(4));

                self.set_flag(ZERO_FLAG, val == 0); // zero iff zero before
                self.set_flag(ADD_SUB_FLAG, false);
                self.set_flag(HALF_CARRY_FLAG, false);
                self.set_flag(CARRY_FLAG, false);

                8 + 2 * dst.cycles()
            }

            // Undefined/illegal
            Op::Undef(opcode) => {
                panic!("illegal instruction: {:02X}", opcode)
            }
        }
    }
}

struct CpuMem<'a> {
    cpu: &'a mut Cpu,
    mem: &'a mut dyn Mem,
}

impl Fetch for CpuMem<'_> {
    fn fetch(&mut self) -> u8 {
        let result = self.mem.loadb(self.cpu.regs.pc);
        self.cpu.regs.pc += 1;
        result
    }
}
