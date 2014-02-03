use mem;

//
// Statics
//

static CARRY_FLAG:      u8 = 1 << 4;
static HALF_CARRY_FLAG: u8 = 1 << 5;
static ADD_SUB_FLAG:    u8 = 1 << 6;
static ZERO_FLAG:       u8 = 1 << 7;


//
// Registers
//

struct Regs {
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
}

impl Regs {
  fn new() -> Regs {
    Regs { a: 0, b: 0, c: 0, d: 0, e: 0, h: 0, l: 0, f: 0, sp: 0, pc: 0 }
  }
}

struct RegsAlt {
  af: u16,
  bc: u16,
  de: u16,
  hl: u16,
  sp: u16,
  pc: u16,
}


//
// Instruction decoding
//

#[deriving(ToStr)]
pub enum Reg8 {
  A, B, C, D, E, H, L
}

impl<M: mem::Mem> Reg8 {
  fn load(&self, cpu: &Cpu<M>) -> u8 {
    match *self {
      A => cpu.regs.a,
      B => cpu.regs.b,
      C => cpu.regs.c,
      D => cpu.regs.d,
      E => cpu.regs.e,
      H => cpu.regs.h,
      L => cpu.regs.l,
    }
  }

  fn store(&self, cpu: &mut Cpu<M>, val: u8) {
    match *self {
      A => cpu.regs.a = val,
      B => cpu.regs.b = val,
      C => cpu.regs.c = val,
      D => cpu.regs.d = val,
      E => cpu.regs.e = val,
      H => cpu.regs.h = val,
      L => cpu.regs.l = val,
    }
  }
}

#[deriving(ToStr)]
pub enum Reg16 {
  AF, BC, DE, HL, SP, PC
}

impl<M: mem::Mem> Reg16 {
  fn load(&self, cpu: &Cpu<M>) -> u16 {
    match *self {
      AF => (cpu.regs.a as u16 << 8) | cpu.regs.f as u16,
      BC => (cpu.regs.b as u16 << 8) | cpu.regs.c as u16,
      DE => (cpu.regs.d as u16 << 8) | cpu.regs.e as u16,
      HL => (cpu.regs.h as u16 << 8) | cpu.regs.l as u16,
      SP => cpu.regs.sp,
      PC => cpu.regs.pc,
    }
  }

  fn store(&self, cpu: &mut Cpu<M>, val: u16) {
    match *self {
      AF => { cpu.regs.a = (val >> 8) as u8; cpu.regs.f = val as u8 }
      BC => { cpu.regs.b = (val >> 8) as u8; cpu.regs.c = val as u8 }
      DE => { cpu.regs.d = (val >> 8) as u8; cpu.regs.e = val as u8 }
      HL => { cpu.regs.h = (val >> 8) as u8; cpu.regs.l = val as u8 }
      SP => cpu.regs.sp = val,
      PC => cpu.regs.pc = val,
    }
  }
}

pub enum Addressing8 {
  Immediate8(u8),
  Indirect8(u8),
  Immediate16Indirect8(u16),
  Reg8(Reg8),
  Reg8Indirect8(Reg8),
  Reg16Indirect8(Reg16),
  Reg16Indirect8Inc(Reg16),
  Reg16Indirect8Dec(Reg16),
}

impl Addressing8 {
  fn load<M: mem::Mem>(&self, cpu: &mut Cpu<M>) -> u8 {
    match *self {
      Immediate8(val)      => val,
      Indirect8(offset)    => cpu.mem.loadb(0xff00 + offset as u16),
      Immediate16Indirect8(addr) => cpu.mem.loadb(addr),
      Reg8(r)              => r.load(cpu),
      Reg8Indirect8(r)     => {
        let offset = r.load(cpu);
        cpu.mem.loadb(0xff00 + offset as u16)
      }
      Reg16Indirect8(r)    => {
        let addr = r.load(cpu);
        cpu.mem.loadb(addr)
      }
      Reg16Indirect8Inc(r) => {
        let prev = r.load(cpu);
        let result = cpu.mem.loadb(prev);
        r.store(cpu, prev + 1);
        result
      }
      Reg16Indirect8Dec(r) => {
        let prev = r.load(cpu);
        let result = cpu.mem.loadb(prev);
        r.store(cpu, prev - 1);
        result
      }
    }
  }

  fn store<M: mem::Mem>(&self, cpu: &mut Cpu<M>, val: u8) {
    match *self {
      Indirect8(offset) => cpu.mem.storeb(0xff00 + offset as u16, val),
      Reg8(r)           => r.store(cpu, val),
      Reg8Indirect8(r)  => {
        let offset = r.load(cpu);
        cpu.mem.storeb(0xff00 + offset as u16, val)
      }
      Reg16Indirect8(r) => {
        let addr = r.load(cpu);
        cpu.mem.storeb(addr, val)
      },
      Reg16Indirect8Inc(r) => {
        let prev = r.load(cpu);
        cpu.mem.storeb(prev, val);
        r.store(cpu, prev + 1);
      },
      Reg16Indirect8Dec(r) => {
        let prev = r.load(cpu);
        cpu.mem.storeb(prev, val);
        r.store(cpu, prev - 1);
      },
      _  => fail!("invalid addressing mode for 8-bit store")
    }
  }
}

pub enum Addressing16 {
  Immediate16(u16),
  Indirect16(u16),
  Reg16(Reg16),
  Reg16Indirect16(Reg16),
}

impl Addressing16 {
  fn load<M: mem::Mem>(&self, cpu: &mut Cpu<M>) -> u16 {
    match *self {
      Immediate16(val)   => val,
      Indirect16(addr)   => cpu.mem.loadw(addr),
      Reg16(r)           => r.load(cpu),
      Reg16Indirect16(r) => { let addr = r.load(cpu); cpu.mem.loadw(addr) }
    }
  }

  fn store<M: mem::Mem>(&self, cpu: &mut Cpu<M>, val: u16) {
    match *self {
      Indirect16(addr)   => cpu.mem.storew(addr, val),
      Reg16(r)           => r.store(cpu, val),
      Reg16Indirect16(r) => { let addr = r.load(cpu); cpu.mem.storew(addr, val) }
      _ => fail!("invalid addressing mode for 16-bit store")
    }
  }
}

pub enum Condition {
  NoCond, CondZ, CondNZ, CondC, CondNC
}

pub trait Decoder<R> {
  // Misc/control
  fn di(&mut self) -> R;
  fn ei(&mut self) -> R;
  fn halt(&mut self) -> R;
  fn nop(&mut self) -> R;
  fn stop(&mut self, val: u8) -> R;

  // Jump/call
  fn call(&mut self, Condition, addr: Addressing16) -> R;
  fn jp(&mut self, Condition, addr: Addressing16) -> R;
  fn jr(&mut self, Condition, rel: i8) -> R;
  fn ret(&mut self, Condition) -> R;
  fn reti(&mut self) -> R;
  fn rst(&mut self, addr: u8) -> R;

  // Load/store/move
  fn ld8(&mut self, dst: Addressing8, src: Addressing8) -> R;
  fn ld16(&mut self, dst: Addressing16, src: Addressing16) -> R;
  fn ldh(&mut self, dst: Addressing8, src: Addressing8) -> R;
  fn pop(&mut self, dst: Addressing16) -> R;
  fn push(&mut self, src: Addressing16) -> R;

  // Arithmetic/logic
  fn adc(&mut self, src: Addressing8) -> R;
  fn add8(&mut self, src: Addressing8) -> R;
  fn add16(&mut self, dst: Addressing16, src: Addressing16) -> R;
  fn and(&mut self, src: Addressing8) -> R;
  fn ccf(&mut self) -> R;
  fn cp(&mut self, src: Addressing8) -> R;
  fn cpl(&mut self) -> R;
  fn daa(&mut self) -> R;
  fn dec8(&mut self, dst: Addressing8) -> R;
  fn dec16(&mut self, dst: Addressing16) -> R;
  fn inc8(&mut self, dst: Addressing8) -> R;
  fn inc16(&mut self, dst: Addressing16) -> R;
  fn or(&mut self, src: Addressing8) -> R;
  fn sbc(&mut self, src: Addressing8) -> R;
  fn scf(&mut self) -> R;
  fn sub(&mut self, src: Addressing8) -> R;
  fn xor(&mut self, src: Addressing8) -> R;

  // Rotation/shift/bit
  fn bit(&mut self, bit: u8, src: Addressing8) -> R;
  fn res(&mut self, bit: u8, dst: Addressing8) -> R;
  fn rl(&mut self, dst: Addressing8) -> R;
  fn rla(&mut self) -> R;
  fn rlc(&mut self, dst: Addressing8) -> R;
  fn rlca(&mut self) -> R;
  fn rr(&mut self, dst: Addressing8) -> R;
  fn rra(&mut self) -> R;
  fn rrc(&mut self, dst: Addressing8) -> R;
  fn rrca(&mut self) -> R;
  fn set(&mut self, bit: u8, dst: Addressing8) -> R;
  fn sla(&mut self, dst: Addressing8) -> R;
  fn sra(&mut self, dst: Addressing8) -> R;
  fn srl(&mut self, dst: Addressing8) -> R;
  fn swap(&mut self, dst: Addressing8) -> R;

  // Undefined/illegal
  fn undef(&mut self) -> R;
}

pub fn decode<M: mem::Mem, R, D: Decoder<R>>(mem: &mut M,
                                             pc: &mut u16,
                                             d: &mut D) -> R {
  let fetchb = || -> u8 { let result = mem.loadb(*pc); *pc += 1; result };
  let fetchw = || -> u16 {
    let lo = fetchb();
    let hi = fetchb();
    (hi as u16 << 8) | lo as u16
  };

  let opcode = fetchb();
  match opcode {
    // 0x00
    0x00 => d.nop(),
    0x01 => { let imm = fetchw(); d.ld16(Reg16(BC), Immediate16(imm)) }
    0x02 => d.ld8(Reg16Indirect8(BC), Reg8(A)),
    0x03 => d.inc16(Reg16(BC)),
    0x04 => d.inc8(Reg8(B)),
    0x05 => d.dec8(Reg8(B)),
    0x06 => { let imm = fetchb(); d.ld8(Reg8(B), Immediate8(imm)) }
    0x07 => d.rlca(),

    0x08 => { let ind = fetchw(); d.ld16(Indirect16(ind), Reg16(SP)) }
    0x09 => d.add16(Reg16(HL), Reg16(BC)),
    0x0a => d.ld8(Reg8(A), Reg16Indirect8(BC)),
    0x0b => d.dec16(Reg16(BC)),
    0x0c => d.inc8(Reg8(C)),
    0x0d => d.dec8(Reg8(C)),
    0x0e => { let imm = fetchb(); d.ld8(Reg8(C), Immediate8(imm)) }
    0x0f => d.rrca(),

    // 0x10
    0x10 => { let val = fetchb(); d.stop(val) }
    0x11 => { let imm = fetchw(); d.ld16(Reg16(DE), Immediate16(imm)) }
    0x12 => d.ld8(Reg16Indirect8(DE), Reg8(A)),
    0x13 => d.inc16(Reg16(DE)),
    0x14 => d.inc8(Reg8(D)),
    0x15 => d.dec8(Reg8(D)),
    0x16 => { let imm = fetchb(); d.ld8(Reg8(D), Immediate8(imm)) }
    0x17 => d.rla(),

    0x18 => { let imm = fetchb(); d.jr(NoCond, imm as i8) }
    0x19 => d.add16(Reg16(HL), Reg16(DE)),
    0x1a => d.ld8(Reg8(A), Reg16Indirect8(DE)),
    0x1b => d.dec16(Reg16(DE)),
    0x1c => d.inc8(Reg8(E)),
    0x1d => d.dec8(Reg8(E)),
    0x1e => { let imm = fetchb(); d.ld8(Reg8(E), Immediate8(imm)) }
    0x1f => d.rra(),

    // 0x20
    0x20 => { let imm = fetchb(); d.jr(CondNZ, imm as i8) }
    0x21 => { let imm = fetchw(); d.ld16(Reg16(HL), Immediate16(imm)) }
    0x22 => d.ld8(Reg16Indirect8Inc(HL), Reg8(A)),
    0x23 => d.inc16(Reg16(HL)),
    0x24 => d.inc8(Reg8(H)),
    0x25 => d.dec8(Reg8(H)),
    0x26 => { let imm = fetchb(); d.ld8(Reg8(H), Immediate8(imm)) }
    0x27 => d.daa(),

    0x28 => { let imm = fetchb(); d.jr(CondZ, imm as i8) }
    0x29 => d.add16(Reg16(HL), Reg16(HL)),
    0x2a => d.ld8(Reg8(A), Reg16Indirect8Inc(HL)),
    0x2b => d.dec16(Reg16(HL)),
    0x2c => d.inc8(Reg8(L)),
    0x2d => d.dec8(Reg8(L)),
    0x2e => { let imm = fetchb(); d.ld8(Reg8(L), Immediate8(imm)) }
    0x2f => d.cpl(),

    // 0x30
    0x30 => { let imm = fetchb(); d.jr(CondNC, imm as i8) }
    0x31 => { let imm = fetchw(); d.ld16(Reg16(SP), Immediate16(imm)) }
    0x32 => d.ld8(Reg16Indirect8Dec(HL), Reg8(A)),
    0x33 => d.inc16(Reg16(SP)),
    0x34 => d.inc8(Reg16Indirect8(HL)),
    0x35 => d.dec8(Reg16Indirect8(HL)),
    0x36 => { let imm = fetchb(); d.ld8(Reg16Indirect8(HL), Immediate8(imm)) }
    0x37 => d.scf(),

    0x38 => { let imm = fetchb(); d.jr(CondC, imm as i8) }
    0x39 => d.add16(Reg16(HL), Reg16(SP)),
    0x3a => d.ld8(Reg8(A), Reg16Indirect8Dec(HL)),
    0x3b => d.dec16(Reg16(SP)),
    0x3c => d.inc8(Reg8(A)),
    0x3d => d.dec8(Reg8(A)),
    0x3e => { let imm = fetchb(); d.ld8(Reg8(A), Immediate8(imm)) }
    0x3f => d.ccf(),

    // 0x40
    0x40 => d.ld8(Reg8(B), Reg8(B)),
    0x41 => d.ld8(Reg8(B), Reg8(C)),
    0x42 => d.ld8(Reg8(B), Reg8(D)),
    0x43 => d.ld8(Reg8(B), Reg8(E)),
    0x44 => d.ld8(Reg8(B), Reg8(H)),
    0x45 => d.ld8(Reg8(B), Reg8(L)),
    0x46 => d.ld8(Reg8(B), Reg16Indirect8(HL)),
    0x47 => d.ld8(Reg8(B), Reg8(A)),

    0x48 => d.ld8(Reg8(C), Reg8(B)),
    0x49 => d.ld8(Reg8(C), Reg8(C)),
    0x4a => d.ld8(Reg8(C), Reg8(D)),
    0x4b => d.ld8(Reg8(C), Reg8(E)),
    0x4c => d.ld8(Reg8(C), Reg8(H)),
    0x4d => d.ld8(Reg8(C), Reg8(L)),
    0x4e => d.ld8(Reg8(C), Reg16Indirect8(HL)),
    0x4f => d.ld8(Reg8(C), Reg8(A)),

    // 0x50
    0x50 => d.ld8(Reg8(D), Reg8(B)),
    0x51 => d.ld8(Reg8(D), Reg8(C)),
    0x52 => d.ld8(Reg8(D), Reg8(D)),
    0x53 => d.ld8(Reg8(D), Reg8(E)),
    0x54 => d.ld8(Reg8(D), Reg8(H)),
    0x55 => d.ld8(Reg8(D), Reg8(L)),
    0x56 => d.ld8(Reg8(D), Reg16Indirect8(HL)),
    0x57 => d.ld8(Reg8(D), Reg8(A)),

    0x58 => d.ld8(Reg8(E), Reg8(B)),
    0x59 => d.ld8(Reg8(E), Reg8(C)),
    0x5a => d.ld8(Reg8(E), Reg8(D)),
    0x5b => d.ld8(Reg8(E), Reg8(E)),
    0x5c => d.ld8(Reg8(E), Reg8(H)),
    0x5d => d.ld8(Reg8(E), Reg8(L)),
    0x5e => d.ld8(Reg8(E), Reg16Indirect8(HL)),
    0x5f => d.ld8(Reg8(E), Reg8(A)),

    // 0x60
    0x60 => d.ld8(Reg8(H), Reg8(B)),
    0x61 => d.ld8(Reg8(H), Reg8(C)),
    0x62 => d.ld8(Reg8(H), Reg8(D)),
    0x63 => d.ld8(Reg8(H), Reg8(E)),
    0x64 => d.ld8(Reg8(H), Reg8(H)),
    0x65 => d.ld8(Reg8(H), Reg8(L)),
    0x66 => d.ld8(Reg8(H), Reg16Indirect8(HL)),
    0x67 => d.ld8(Reg8(H), Reg8(A)),

    0x68 => d.ld8(Reg8(L), Reg8(B)),
    0x69 => d.ld8(Reg8(L), Reg8(C)),
    0x6a => d.ld8(Reg8(L), Reg8(D)),
    0x6b => d.ld8(Reg8(L), Reg8(E)),
    0x6c => d.ld8(Reg8(L), Reg8(H)),
    0x6d => d.ld8(Reg8(L), Reg8(L)),
    0x6e => d.ld8(Reg8(L), Reg16Indirect8(HL)),
    0x6f => d.ld8(Reg8(L), Reg8(A)),

    // 0x70
    0x70 => d.ld8(Reg16Indirect8(HL), Reg8(B)),
    0x71 => d.ld8(Reg16Indirect8(HL), Reg8(C)),
    0x72 => d.ld8(Reg16Indirect8(HL), Reg8(D)),
    0x73 => d.ld8(Reg16Indirect8(HL), Reg8(E)),
    0x74 => d.ld8(Reg16Indirect8(HL), Reg8(H)),
    0x75 => d.ld8(Reg16Indirect8(HL), Reg8(L)),
    0x76 => d.halt(),
    0x77 => d.ld8(Reg16Indirect8(HL), Reg8(A)),

    0x78 => d.ld8(Reg8(A), Reg8(B)),
    0x79 => d.ld8(Reg8(A), Reg8(C)),
    0x7a => d.ld8(Reg8(A), Reg8(D)),
    0x7b => d.ld8(Reg8(A), Reg8(E)),
    0x7c => d.ld8(Reg8(A), Reg8(H)),
    0x7d => d.ld8(Reg8(A), Reg8(L)),
    0x7e => d.ld8(Reg8(A), Reg16Indirect8(HL)),
    0x7f => d.ld8(Reg8(A), Reg8(A)),

    // 0x80
    0x80 => d.add8(Reg8(B)),
    0x81 => d.add8(Reg8(C)),
    0x82 => d.add8(Reg8(D)),
    0x83 => d.add8(Reg8(E)),
    0x84 => d.add8(Reg8(H)),
    0x85 => d.add8(Reg8(L)),
    0x86 => d.add8(Reg16Indirect8(HL)),
    0x87 => d.add8(Reg8(A)),

    0x88 => d.adc(Reg8(B)),
    0x89 => d.adc(Reg8(C)),
    0x8a => d.adc(Reg8(D)),
    0x8b => d.adc(Reg8(E)),
    0x8c => d.adc(Reg8(H)),
    0x8d => d.adc(Reg8(L)),
    0x8e => d.adc(Reg16Indirect8(HL)),
    0x8f => d.adc(Reg8(A)),

    // 0x90
    0x90 => d.sub(Reg8(B)),
    0x91 => d.sub(Reg8(C)),
    0x92 => d.sub(Reg8(D)),
    0x93 => d.sub(Reg8(E)),
    0x94 => d.sub(Reg8(H)),
    0x95 => d.sub(Reg8(L)),
    0x96 => d.sub(Reg16Indirect8(HL)),
    0x97 => d.sub(Reg8(A)),

    0x98 => d.sbc(Reg8(B)),
    0x99 => d.sbc(Reg8(C)),
    0x9a => d.sbc(Reg8(D)),
    0x9b => d.sbc(Reg8(E)),
    0x9c => d.sbc(Reg8(H)),
    0x9d => d.sbc(Reg8(L)),
    0x9e => d.sbc(Reg16Indirect8(HL)),
    0x9f => d.sbc(Reg8(A)),

    // 0xa0
    0xa0 => d.and(Reg8(B)),
    0xa1 => d.and(Reg8(C)),
    0xa2 => d.and(Reg8(D)),
    0xa3 => d.and(Reg8(E)),
    0xa4 => d.and(Reg8(H)),
    0xa5 => d.and(Reg8(L)),
    0xa6 => d.and(Reg16Indirect8(HL)),
    0xa7 => d.and(Reg8(A)),

    0xa8 => d.xor(Reg8(B)),
    0xa9 => d.xor(Reg8(C)),
    0xaa => d.xor(Reg8(D)),
    0xab => d.xor(Reg8(E)),
    0xac => d.xor(Reg8(H)),
    0xad => d.xor(Reg8(L)),
    0xae => d.xor(Reg16Indirect8(HL)),
    0xaf => d.xor(Reg8(A)),

    // 0xb0
    0xb0 => d.or(Reg8(B)),
    0xb1 => d.or(Reg8(C)),
    0xb2 => d.or(Reg8(D)),
    0xb3 => d.or(Reg8(E)),
    0xb4 => d.or(Reg8(H)),
    0xb5 => d.or(Reg8(L)),
    0xb6 => d.or(Reg16Indirect8(HL)),
    0xb7 => d.or(Reg8(A)),

    0xb8 => d.cp(Reg8(B)),
    0xb9 => d.cp(Reg8(C)),
    0xba => d.cp(Reg8(D)),
    0xbb => d.cp(Reg8(E)),
    0xbc => d.cp(Reg8(H)),
    0xbd => d.cp(Reg8(L)),
    0xbe => d.cp(Reg16Indirect8(HL)),
    0xbf => d.cp(Reg8(A)),

    // 0xc0
    0xc0 => d.ret(CondNZ),
    0xc1 => d.pop(Reg16(BC)),
    0xc2 => { let imm = fetchw(); d.jp(CondNZ, Immediate16(imm)) }
    0xc3 => { let imm = fetchw(); d.jp(NoCond, Immediate16(imm)) }
    0xc4 => { let imm = fetchw(); d.call(CondNZ, Immediate16(imm)) }
    0xc5 => d.push(Reg16(BC)),
    0xc6 => { let imm = fetchb(); d.add8(Immediate8(imm)) }
    0xc7 => d.rst(0x00),

    0xc8 => d.ret(CondZ),
    0xc9 => d.ret(NoCond),
    0xca => { let imm = fetchw(); d.jp(CondZ, Immediate16(imm)) }
    0xcb => {
      let extra = fetchb();

      let addr = match extra & 0x07 {
        0x00 => Reg8(B),
        0x01 => Reg8(C),
        0x02 => Reg8(D),
        0x03 => Reg8(E),
        0x04 => Reg8(H),
        0x05 => Reg8(L),
        0x06 => Reg16Indirect8(HL),
        0x07 => Reg8(A),
        _    => fail!("logic error"),
      };

      match extra & 0xf8 {
        0x00 => d.rlc(addr),
        0x08 => d.rrc(addr),
        0x10 => d.rl(addr),
        0x18 => d.rr(addr),
        0x20 => d.sla(addr),
        0x28 => d.sra(addr),
        0x30 => d.swap(addr),
        0x38 => d.srl(addr),
        0x40 => d.bit(0, addr),
        0x48 => d.bit(1, addr),
        0x50 => d.bit(2, addr),
        0x58 => d.bit(3, addr),
        0x60 => d.bit(4, addr),
        0x68 => d.bit(5, addr),
        0x70 => d.bit(6, addr),
        0x78 => d.bit(7, addr),
        0x80 => d.res(0, addr),
        0x88 => d.res(1, addr),
        0x90 => d.res(2, addr),
        0x98 => d.res(3, addr),
        0xa0 => d.res(4, addr),
        0xa8 => d.res(5, addr),
        0xb0 => d.res(6, addr),
        0xb8 => d.res(7, addr),
        0xc0 => d.set(0, addr),
        0xc8 => d.set(1, addr),
        0xd0 => d.set(2, addr),
        0xd8 => d.set(3, addr),
        0xe0 => d.set(4, addr),
        0xe8 => d.set(5, addr),
        0xf0 => d.set(6, addr),
        0xf8 => d.set(7, addr),
        _    => fail!("logic error")
      }
    }
    0xcc => { let imm = fetchw(); d.call(CondZ, Immediate16(imm)) }
    0xcd => { let imm = fetchw(); d.call(NoCond, Immediate16(imm)) }
    0xce => { let imm = fetchb(); d.adc(Immediate8(imm)) }
    0xcf => d.rst(0x08),

    // 0xd0
    0xd0 => d.ret(CondNC),
    0xd1 => d.pop(Reg16(DE)),
    0xd2 => { let imm = fetchw(); d.jp(CondNC, Immediate16(imm)) }
    /*0xd3 => d.nop(),*/
    0xd4 => { let imm = fetchw(); d.call(CondNC, Immediate16(imm)) }
    0xd5 => d.push(Reg16(DE)),
    0xd6 => { let imm = fetchb(); d.sub(Immediate8(imm)) }
    0xd7 => d.rst(0x10),

    0xd8 => d.ret(CondC),
    0xd9 => d.reti(),
    0xda => { let imm = fetchw(); d.jp(CondC, Immediate16(imm)) }
    /*0xdb => d.nop(),*/
    0xdc => { let imm = fetchw(); d.call(CondC, Immediate16(imm)) }
    /*0xdd => d.nop(),*/
    0xde => { let imm = fetchb(); d.sbc(Immediate8(imm)) }
    0xdf => d.rst(0x18),

    // 0xe0
    0xe0 => { let ind = fetchb(); d.ldh(Indirect8(ind), Reg8(A)) }
    0xe1 => d.pop(Reg16(HL)),
    0xe2 => d.ld8(Reg8Indirect8(C), Reg8(A)),
    /*0xe3 => d.nop(),*/
    /*0xe4 => d.nop(),*/
    0xe5 => d.push(Reg16(HL)),
    0xe6 => { let imm = fetchb(); d.and(Immediate8(imm)) }
    0xe7 => d.rst(0x20),

    /*0xe8 => d.add16(Reg16(SP), ???),*/ // TODO
    0xe9 => d.jp(NoCond, Reg16Indirect16(HL)),
    0xea => { let ind = fetchw(); d.ld8(Immediate16Indirect8(ind), Reg8(A)) }
    /*0xeb => d.nop(),*/
    /*0xec => d.nop(),*/
    /*0xed => d.nop(),*/
    0xee => { let imm = fetchb(); d.xor(Immediate8(imm)) }
    0xef => d.rst(0x28),

    // 0xf0
    0xf0 => { let ind = fetchb(); d.ldh(Reg8(A), Indirect8(ind)) }
    0xf1 => d.pop(Reg16(AF)),
    0xf2 => d.ld8(Reg8(A), Reg8Indirect8(C)),
    0xf3 => d.di(),
    /*0xf4 => d.nop(),*/
    0xf5 => d.push(Reg16(AF)),
    0xf6 => { let imm = fetchb(); d.or(Immediate8(imm)) }
    0xf7 => d.rst(0x30),

    /*0xf8 => d.ld(/*...*/),*/ // TODO
    0xf9 => d.ld16(Reg16(SP), Reg16(HL)),
    0xfa => { let ind = fetchw(); d.ld8(Reg8(A), Immediate16Indirect8(ind)) }
    0xfb => d.ei(),
    /*0xfc => d.nop(),*/
    /*0xfd => d.nop(),*/
    0xfe => { let imm = fetchb(); d.cp(Immediate8(imm)) }
    0xff => d.rst(0x38),

    _    => d.undef()
  }
}

//
// CPU
//

pub struct Cpu<M> {
  regs: Regs,
  mem: M,
}

impl<M: mem::Mem> Cpu<M> {
  pub fn new(mem: M) -> Cpu<M> {
    Cpu { regs: Regs::new(), mem: mem }
  }

  pub fn fetch(&mut self) -> u8 {
    let result = self.mem.loadb(self.regs.pc);
    self.regs.pc += 1;
    result
  }
}

// impl<M: mem::Mem> Decoder<()> for Cpu<M> {
// }
