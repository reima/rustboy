use crate::{
    instr::{Addr16, Addr8, Cond, Fetch, Op},
    mem,
};

pub struct Disasm<'a> {
    pub mem: &'a dyn mem::Mem,
    pub pc: u16,
}

impl<'a> Disasm<'a> {
    pub fn disasm(&mut self) -> String {
        let op = Op::decode(self);

        match op {
            // Misc/control
            Op::Nop => "NOP".to_string(),
            Op::Ei => "EI".to_string(),
            Op::Di => "DI".to_string(),
            Op::Halt => "HALT".to_string(),
            Op::Stop(val) => format!("STOP {}", val),

            // Jump/call
            Op::Jp(cond, addr) => unary16(with_cond("JP", cond).as_ref(), addr),
            Op::Jr(cond, rel) => format!("{} {}", with_cond("JR", cond), rel),
            Op::Call(cond, addr) => unary16(with_cond("CALL", cond).as_ref(), addr),
            Op::Rst(addr) => format!("RST ${:02X}", addr),
            Op::Ret(cond) => format!("RET {}", cond_to_str(cond)),
            Op::Reti => "RETI".to_string(),

            // Load/store/move
            Op::Ld8(dst, src) => binary8("LD", dst, src),
            Op::Ld16(dst, src) => binary16("LD", dst, src),
            Op::Ldh(dst, src) => binary8("LDH", dst, src),
            Op::Ldhl(rel) => format!("LDHL SP, {}", rel),
            Op::Push(src) => unary16("PUSH", src),
            Op::Pop(dst) => unary16("POP", dst),

            // Arithmetic/logic
            Op::Add8(src) => unary8("ADD A,", src),
            Op::Add16(dst, src) => binary16("ADD", dst, src),
            Op::Addsp(rel) => format!("ADD SP, {}", rel),
            Op::Adc(src) => unary8("ADC A,", src),
            Op::Sub(src) => unary8("SUB", src),
            Op::Sbc(src) => unary8("SBC A,", src),
            Op::Inc8(dst) => unary8("INC", dst),
            Op::Inc16(dst) => unary16("INC", dst),
            Op::Dec8(dst) => unary8("DEC", dst),
            Op::Dec16(dst) => unary16("DEC", dst),
            Op::And(src) => unary8("AND", src),
            Op::Or(src) => unary8("OR", src),
            Op::Xor(src) => unary8("XOR", src),
            Op::Cp(src) => unary8("CP", src),
            Op::Cpl => "CPL".to_string(),
            Op::Scf => "SCF".to_string(),
            Op::Ccf => "CCF".to_string(),
            Op::Daa => "DAA".to_string(),

            // Rotation/shift/bit
            Op::Rlca => "RLCA".to_string(),
            Op::Rla => "RLA".to_string(),
            Op::Rrca => "RRCA".to_string(),
            Op::Rra => "RRA".to_string(),
            Op::Rlc(dst) => unary8("RLC", dst),
            Op::Rl(dst) => unary8("RL", dst),
            Op::Rrc(dst) => unary8("RRC", dst),
            Op::Rr(dst) => unary8("RR", dst),
            Op::Sla(dst) => unary8("SLA", dst),
            Op::Sra(dst) => unary8("SRA", dst),
            Op::Srl(dst) => unary8("SRL", dst),
            Op::Bit(bit, src) => format!("BIT {}, {}", bit, addr8_to_str(src)),
            Op::Res(bit, dst) => format!("RES {}, {}", bit, addr8_to_str(dst)),
            Op::Set(bit, dst) => format!("SET {}, {}", bit, addr8_to_str(dst)),
            Op::Swap(dst) => unary8("SWAP", dst),

            // Undefined/illegal
            Op::Undef(opcode) => format!("UNDEFINED ${:02X}", opcode),
        }
    }
}

fn addr16_to_str(addr: Addr16) -> String {
    match addr {
        Addr16::Imm(val) => format!("${:04X}", val),
        Addr16::Ind(addr) => format!("(${:04X})", addr),
        Addr16::Reg16Dir(r) => format!("{:?}", r),
    }
}

fn addr8_to_str(addr: Addr8) -> String {
    match addr {
        Addr8::Imm(val) => format!("${:02X}", val),
        Addr8::Ind(offset) => format!("($FF00+${:02X})", offset),
        Addr8::Imm16Ind(addr) => format!("(${:04X})", addr),
        Addr8::Reg8Dir(r) => format!("{:?}", r),
        Addr8::Reg8Ind(r) => format!("($FF00+{:?})", r),
        Addr8::Reg16Ind(r) => format!("({:?})", r),
        Addr8::Reg16IndInc(r) => format!("({:?}+)", r),
        Addr8::Reg16IndDec(r) => format!("({:?}-)", r),
    }
}

fn cond_to_str(cond: Cond) -> &'static str {
    match cond {
        Cond::None => "",
        Cond::Z => "Z",
        Cond::NZ => "NZ",
        Cond::C => "C",
        Cond::NC => "NC",
    }
}

fn with_cond(mnemonic: &str, cond: Cond) -> String {
    match cond {
        Cond::None => mnemonic.to_string(),
        _ => format!("{} {},", mnemonic, cond_to_str(cond)),
    }
}

fn unary8(mnemonic: &str, addr: Addr8) -> String {
    format!("{} {}", mnemonic, addr8_to_str(addr))
}

fn binary8(mnemonic: &str, addr1: Addr8, addr2: Addr8) -> String {
    format!(
        "{} {}, {}",
        mnemonic,
        addr8_to_str(addr1),
        addr8_to_str(addr2)
    )
}

fn unary16(mnemonic: &str, addr: Addr16) -> String {
    format!("{} {}", mnemonic, addr16_to_str(addr))
}

fn binary16(mnemonic: &str, addr1: Addr16, addr2: Addr16) -> String {
    format!(
        "{} {}, {}",
        mnemonic,
        addr16_to_str(addr1),
        addr16_to_str(addr2)
    )
}

impl<'a> Fetch for Disasm<'a> {
    fn fetch(&mut self) -> u8 {
        let result = self.mem.loadb(self.pc);
        self.pc += 1;
        result
    }
}
