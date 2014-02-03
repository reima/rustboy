use cpu::{Addr8, Addr16, Cond, Decoder};
use cpu;

pub struct Disasm;

fn addr16_to_str(addr: Addr16) -> ~str {
  match addr {
    cpu::Imm16(val)    => format!("${:04X}", val),
    cpu::Ind16(addr)   => format!("(${:04X})", addr),
    cpu::Reg16(r)      => r.to_str(),
    cpu::Reg16Ind16(r) => format!("({:s})", r.to_str()),
  }
}

fn addr8_to_str(addr: Addr8) -> ~str {
  match addr {
    cpu::Imm8(val)       => format!("${:02X}", val),
    cpu::Ind8(offset)    => format!("($FF00+${:02X})", offset),
    cpu::Imm16Ind8(addr) => format!("(${:04X})", addr),
    cpu::Reg8(r)         => r.to_str(),
    cpu::Reg8Ind8(r)     => format!("($FF00+{:s})", r.to_str()),
    cpu::Reg16Ind8(r)    => format!("({:s})", r.to_str()),
    cpu::Reg16Ind8Inc(r) => format!("({:s}+)", r.to_str()),
    cpu::Reg16Ind8Dec(r) => format!("({:s}-)", r.to_str()),
  }
}

fn cond_to_str(cond: Cond) -> &'static str {
  match cond {
    cpu::CondNone => "",
    cpu::CondZ    => "Z",
    cpu::CondNZ   => "NZ",
    cpu::CondC    => "C",
    cpu::CondNC   => "NC",
  }
}

fn with_cond(mnemonic: &str, cond: Cond) -> ~str {
  match cond {
    cpu::CondNone => mnemonic.to_owned(),
    _             => format!("{:s} {:s},", mnemonic, cond_to_str(cond)),
  }
}

fn unary8(mnemonic: &str, addr: Addr8) -> ~str {
  format!("{:s} {:s}", mnemonic, addr8_to_str(addr))
}

fn binary8(mnemonic: &str, addr1: Addr8, addr2: Addr8) -> ~str {
  format!("{:s} {:s}, {:s}", mnemonic, addr8_to_str(addr1), addr8_to_str(addr2))
}

fn unary16(mnemonic: &str, addr: Addr16) -> ~str {
  format!("{:s} {:s}", mnemonic, addr16_to_str(addr))
}

fn binary16(mnemonic: &str, addr1: Addr16, addr2: Addr16) -> ~str {
  format!("{:s} {:s}, {:s}", mnemonic, addr16_to_str(addr1), addr16_to_str(addr2))
}

impl Decoder<~str> for Disasm {
  // Misc/control
  fn di  (&mut self)          -> ~str { ~"DI" }
  fn ei  (&mut self)          -> ~str { ~"EI" }
  fn halt(&mut self)          -> ~str { ~"HALT" }
  fn nop (&mut self)          -> ~str { ~"NOP" }
  fn stop(&mut self, val: u8) -> ~str { format!("STOP {:u}", val) }

  // Jump/call
  fn call(&mut self, cond: Cond, addr: Addr16) -> ~str { unary16(with_cond("CALL", cond), addr) }
  fn jp  (&mut self, cond: Cond, addr: Addr16) -> ~str { unary16(with_cond("JP",   cond), addr) }
  fn jr  (&mut self, cond: Cond, rel: i8)      -> ~str { format!("{:s} {:d}", with_cond("JR", cond), rel) }
  fn ret (&mut self, cond: Cond)               -> ~str { format!("RET {:s}", cond_to_str(cond)) }
  fn reti(&mut self)                           -> ~str {        ~"RETI" }
  fn rst (&mut self, addr: u8)                 -> ~str { format!("RST ${:02X}", addr) }

  // Load/store/move
  fn ld8 (&mut self, dst: Addr8,  src: Addr8)  -> ~str { binary8 ("LD", dst, src) }
  fn ld16(&mut self, dst: Addr16, src: Addr16) -> ~str { binary16("LD", dst, src) }
  fn ldh (&mut self, dst: Addr8,  src: Addr8)  -> ~str { binary8 ("LDH", dst, src) }

  fn pop (&mut self, dst: Addr16)              -> ~str { unary16 ("POP", dst) }
  fn push(&mut self, src: Addr16)              -> ~str { unary16 ("PUSH", src) }

  // Arithmetic/logic
  fn adc  (&mut self, src: Addr8)               -> ~str { unary8  ("ADC A,", src) }
  fn add8 (&mut self, src: Addr8)               -> ~str { unary8  ("ADD A,", src) }
  fn add16(&mut self, dst: Addr16, src: Addr16) -> ~str { binary16("ADD", dst, src) }
  fn and  (&mut self, src: Addr8)               -> ~str { unary8  ("AND", src) }
  fn ccf  (&mut self)                           -> ~str {         ~"CCF" }
  fn cp   (&mut self, src: Addr8)               -> ~str { unary8  ("CP", src) }
  fn cpl  (&mut self)                           -> ~str {         ~"CPL" }
  fn daa  (&mut self)                           -> ~str {         ~"DAA" }
  fn dec8 (&mut self, dst: Addr8)               -> ~str { unary8  ("DEC", dst) }
  fn dec16(&mut self, dst: Addr16)              -> ~str { unary16 ("DEC", dst) }
  fn inc8 (&mut self, dst: Addr8)               -> ~str { unary8  ("INC", dst) }
  fn inc16(&mut self, dst: Addr16)              -> ~str { unary16 ("INC", dst) }
  fn or   (&mut self, src: Addr8)               -> ~str { unary8  ("XOR", src) }
  fn sbc  (&mut self, src: Addr8)               -> ~str { unary8  ("SBC A,", src) }
  fn scf  (&mut self)                           -> ~str {         ~"SCF" }
  fn sub  (&mut self, src: Addr8)               -> ~str { unary8  ("SUB", src) }
  fn xor  (&mut self, src: Addr8)               -> ~str { unary8  ("XOR", src) }

  // Rotation/shift/bit
  fn bit (&mut self, bit: u8, src: Addr8) -> ~str { format!("BIT {:u}, {:s}", bit, addr8_to_str(src)) }
  fn res (&mut self, bit: u8, dst: Addr8) -> ~str { format!("RES {:u}, {:s}", bit, addr8_to_str(dst)) }
  fn rl  (&mut self, dst: Addr8)          -> ~str { unary8 ("RL", dst) }
  fn rla (&mut self)                      -> ~str {        ~"RLA" }
  fn rlc (&mut self, dst: Addr8)          -> ~str { unary8 ("RLC", dst) }
  fn rlca(&mut self)                      -> ~str {        ~"RLCA" }
  fn rr  (&mut self, dst: Addr8)          -> ~str { unary8 ("RR", dst) }
  fn rra (&mut self)                      -> ~str {        ~"RAA" }
  fn rrc (&mut self, dst: Addr8)          -> ~str { unary8 ("RRC", dst) }
  fn rrca(&mut self)                      -> ~str {        ~"RRCA" }
  fn set (&mut self, bit: u8, dst: Addr8) -> ~str { format!("SET {:u}, {:s}", bit, addr8_to_str(dst)) }
  fn sla (&mut self, dst: Addr8)          -> ~str { unary8 ("SLA", dst) }
  fn sra (&mut self, dst: Addr8)          -> ~str { unary8 ("SRA", dst) }
  fn srl (&mut self, dst: Addr8)          -> ~str { unary8 ("SRL", dst) }
  fn swap(&mut self, dst: Addr8)          -> ~str { unary8 ("SWAP", dst) }

  // Undefined/illegal
  fn undef(&mut self) -> ~str { ~"undefined opcode" }
}
