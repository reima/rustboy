use mem;

//
// Statics
//

static CARRY_OFFSET:      u8 = 4;
static HALF_CARRY_OFFSET: u8 = 5;
static ADD_SUB_OFFSET:    u8 = 6;
static ZERO_OFFSET:       u8 = 7;

static CARRY_FLAG:        u8 = 1 << CARRY_OFFSET;
static HALF_CARRY_FLAG:   u8 = 1 << HALF_CARRY_OFFSET;
static ADD_SUB_FLAG:      u8 = 1 << ADD_SUB_OFFSET;
static ZERO_FLAG:         u8 = 1 << ZERO_OFFSET;


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

pub enum Addr8 {
  Imm8(u8),
  Ind8(u8),
  Imm16Ind8(u16),
  Reg8(Reg8),
  Reg8Ind8(Reg8),
  Reg16Ind8(Reg16),
  Reg16Ind8Inc(Reg16),
  Reg16Ind8Dec(Reg16),
}

impl Addr8 {
  fn load<M: mem::Mem>(&self, cpu: &mut Cpu<M>) -> u8 {
    match *self {
      Imm8(val)       => val,
      Ind8(offset)    => cpu.mem.loadb(0xff00 + offset as u16),
      Imm16Ind8(addr) => cpu.mem.loadb(addr),
      Reg8(r)         => r.load(cpu),
      Reg8Ind8(r)     => {
        let offset = r.load(cpu);
        cpu.mem.loadb(0xff00 + offset as u16)
      }
      Reg16Ind8(r) |
      Reg16Ind8Inc(r) |
      Reg16Ind8Dec(r) => {
        let addr = r.load(cpu);
        let result = cpu.mem.loadb(addr);
        match *self {
          Reg16Ind8Inc(_) => r.store(cpu, addr + 1),
          Reg16Ind8Dec(_) => r.store(cpu, addr - 1),
          _ => ()
        }
        result
      }
    }
  }

  fn store<M: mem::Mem>(&self, cpu: &mut Cpu<M>, val: u8) {
    match *self {
      Ind8(offset) => cpu.mem.storeb(0xff00 + offset as u16, val),
      Reg8(r)      => r.store(cpu, val),
      Reg8Ind8(r)  => {
        let offset = r.load(cpu);
        cpu.mem.storeb(0xff00 + offset as u16, val)
      }
      Reg16Ind8(r) |
      Reg16Ind8Inc(r) |
      Reg16Ind8Dec(r) => {
        let addr = r.load(cpu);
        cpu.mem.storeb(addr, val);
        match *self {
          Reg16Ind8Inc(_) => r.store(cpu, addr + 1),
          Reg16Ind8Dec(_) => r.store(cpu, addr - 1),
          _ => ()
        }
      }
      _  => fail!("invalid addressing mode for 8-bit store")
    }
  }
}

pub enum Addr16 {
  Imm16(u16),
  Ind16(u16),
  Reg16(Reg16),
  Reg16Ind16(Reg16),
}

impl Addr16 {
  fn load<M: mem::Mem>(&self, cpu: &mut Cpu<M>) -> u16 {
    match *self {
      Imm16(val)    => val,
      Ind16(addr)   => cpu.mem.loadw(addr),
      Reg16(r)      => r.load(cpu),
      Reg16Ind16(r) => { let addr = r.load(cpu); cpu.mem.loadw(addr) }
    }
  }

  fn store<M: mem::Mem>(&self, cpu: &mut Cpu<M>, val: u16) {
    match *self {
      Ind16(addr)   => cpu.mem.storew(addr, val),
      Reg16(r)      => r.store(cpu, val),
      Reg16Ind16(r) => { let addr = r.load(cpu); cpu.mem.storew(addr, val) }
      _ => fail!("invalid addressing mode for 16-bit store")
    }
  }
}

pub enum Cond {
  CondNone,
  CondZ,
  CondNZ,
  CondC,
  CondNC
}

impl Cond {
  fn eval<M: mem::Mem>(self, cpu: &Cpu<M>) -> bool {
    match self {
      CondNone => true,
      CondZ    => (cpu.regs.f & ZERO_FLAG) != 0,
      CondNZ   => (cpu.regs.f & ZERO_FLAG) == 0,
      CondC    => (cpu.regs.f & CARRY_FLAG) != 0,
      CondNC   => (cpu.regs.f & CARRY_FLAG) == 0,
    }
  }
}

pub trait Decoder<R> {
  // Misc/control
  fn di  (&mut self)          -> R;
  fn ei  (&mut self)          -> R;
  fn halt(&mut self)          -> R;
  fn nop (&mut self)          -> R;
  fn stop(&mut self, val: u8) -> R;

  // Jump/call
  fn call(&mut self, Cond, addr: Addr16) -> R;
  fn jp  (&mut self, Cond, addr: Addr16) -> R;
  fn jr  (&mut self, Cond, rel: i8)      -> R;
  fn ret (&mut self, Cond)               -> R;
  fn reti(&mut self)                     -> R;
  fn rst (&mut self, addr: u8)           -> R;

  // Load/store/move
  fn ld8 (&mut self, dst: Addr8,  src: Addr8)  -> R;
  fn ld16(&mut self, dst: Addr16, src: Addr16) -> R;
  fn ldh (&mut self, dst: Addr8,  src: Addr8)  -> R;
  fn pop (&mut self, dst: Addr16)              -> R;
  fn push(&mut self, src: Addr16)              -> R;

  // Arithmetic/logic
  fn adc  (&mut self, src: Addr8)               -> R;
  fn add8 (&mut self, src: Addr8)               -> R;
  fn add16(&mut self, dst: Addr16, src: Addr16) -> R;
  fn and  (&mut self, src: Addr8)               -> R;
  fn ccf  (&mut self)                           -> R;
  fn cp   (&mut self, src: Addr8)               -> R;
  fn cpl  (&mut self)                           -> R;
  fn daa  (&mut self)                           -> R;
  fn dec8 (&mut self, dst: Addr8)               -> R;
  fn dec16(&mut self, dst: Addr16)              -> R;
  fn inc8 (&mut self, dst: Addr8)               -> R;
  fn inc16(&mut self, dst: Addr16)              -> R;
  fn or   (&mut self, src: Addr8)               -> R;
  fn sbc  (&mut self, src: Addr8)               -> R;
  fn scf  (&mut self)                           -> R;
  fn sub  (&mut self, src: Addr8)               -> R;
  fn xor  (&mut self, src: Addr8)               -> R;

  // Rotation/shift/bit
  fn bit (&mut self, bit: u8, src: Addr8) -> R;
  fn res (&mut self, bit: u8, dst: Addr8) -> R;
  fn rl  (&mut self, dst: Addr8)          -> R;
  fn rla (&mut self)                      -> R;
  fn rlc (&mut self, dst: Addr8)          -> R;
  fn rlca(&mut self)                      -> R;
  fn rr  (&mut self, dst: Addr8)          -> R;
  fn rra (&mut self)                      -> R;
  fn rrc (&mut self, dst: Addr8)          -> R;
  fn rrca(&mut self)                      -> R;
  fn set (&mut self, bit: u8, dst: Addr8) -> R;
  fn sla (&mut self, dst: Addr8)          -> R;
  fn sra (&mut self, dst: Addr8)          -> R;
  fn srl (&mut self, dst: Addr8)          -> R;
  fn swap(&mut self, dst: Addr8)          -> R;

  // Undefined/illegal
  fn undef(&mut self) -> R;
}

// Several instructions use a 3 bit part of the opcode to encode common
// operands, this method decodes the operand from the lower 3 bits
fn decode_addr(code: u8) -> Addr8 {
  match code & 0x07 {
    0x00 => Reg8(B),
    0x01 => Reg8(C),
    0x02 => Reg8(D),
    0x03 => Reg8(E),
    0x04 => Reg8(H),
    0x05 => Reg8(L),
    0x06 => Reg16Ind8(HL),
    0x07 => Reg8(A),
    _    => fail!("logic error"),
  }
}

// Source: http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
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
    0x01 => { let imm = fetchw();
            d.ld16(Reg16(BC), Imm16(imm)) }
    0x02 => d.ld8(Reg16Ind8(BC), Reg8(A)),
    0x03 => d.inc16(Reg16(BC)),
    0x04 => d.inc8(Reg8(B)),
    0x05 => d.dec8(Reg8(B)),
    0x06 => { let imm = fetchb(); d.ld8(Reg8(B), Imm8(imm)) }
    0x07 => d.rlca(),

    0x08 => { let ind = fetchw(); d.ld16(Ind16(ind), Reg16(SP)) }
    0x09 => d.add16(Reg16(HL), Reg16(BC)),
    0x0a => d.ld8(Reg8(A), Reg16Ind8(BC)),
    0x0b => d.dec16(Reg16(BC)),
    0x0c => d.inc8(Reg8(C)),
    0x0d => d.dec8(Reg8(C)),
    0x0e => { let imm = fetchb(); d.ld8(Reg8(C), Imm8(imm)) }
    0x0f => d.rrca(),

    // 0x10
    0x10 => { let val = fetchb(); d.stop(val) }
    0x11 => { let imm = fetchw(); d.ld16(Reg16(DE), Imm16(imm)) }
    0x12 => d.ld8(Reg16Ind8(DE), Reg8(A)),
    0x13 => d.inc16(Reg16(DE)),
    0x14 => d.inc8(Reg8(D)),
    0x15 => d.dec8(Reg8(D)),
    0x16 => { let imm = fetchb(); d.ld8(Reg8(D), Imm8(imm)) }
    0x17 => d.rla(),

    0x18 => { let imm = fetchb(); d.jr(CondNone, imm as i8) }
    0x19 => d.add16(Reg16(HL), Reg16(DE)),
    0x1a => d.ld8(Reg8(A), Reg16Ind8(DE)),
    0x1b => d.dec16(Reg16(DE)),
    0x1c => d.inc8(Reg8(E)),
    0x1d => d.dec8(Reg8(E)),
    0x1e => { let imm = fetchb(); d.ld8(Reg8(E), Imm8(imm)) }
    0x1f => d.rra(),

    // 0x20
    0x20 => { let imm = fetchb(); d.jr(CondNZ, imm as i8) }
    0x21 => { let imm = fetchw(); d.ld16(Reg16(HL), Imm16(imm)) }
    0x22 => d.ld8(Reg16Ind8Inc(HL), Reg8(A)),
    0x23 => d.inc16(Reg16(HL)),
    0x24 => d.inc8(Reg8(H)),
    0x25 => d.dec8(Reg8(H)),
    0x26 => { let imm = fetchb(); d.ld8(Reg8(H), Imm8(imm)) }
    0x27 => d.daa(),

    0x28 => { let imm = fetchb(); d.jr(CondZ, imm as i8) }
    0x29 => d.add16(Reg16(HL), Reg16(HL)),
    0x2a => d.ld8(Reg8(A), Reg16Ind8Inc(HL)),
    0x2b => d.dec16(Reg16(HL)),
    0x2c => d.inc8(Reg8(L)),
    0x2d => d.dec8(Reg8(L)),
    0x2e => { let imm = fetchb(); d.ld8(Reg8(L), Imm8(imm)) }
    0x2f => d.cpl(),

    // 0x30
    0x30 => { let imm = fetchb(); d.jr(CondNC, imm as i8) }
    0x31 => { let imm = fetchw(); d.ld16(Reg16(SP), Imm16(imm)) }
    0x32 => d.ld8(Reg16Ind8Dec(HL), Reg8(A)),
    0x33 => d.inc16(Reg16(SP)),
    0x34 => d.inc8(Reg16Ind8(HL)),
    0x35 => d.dec8(Reg16Ind8(HL)),
    0x36 => { let imm = fetchb(); d.ld8(Reg16Ind8(HL), Imm8(imm)) }
    0x37 => d.scf(),

    0x38 => { let imm = fetchb(); d.jr(CondC, imm as i8) }
    0x39 => d.add16(Reg16(HL), Reg16(SP)),
    0x3a => d.ld8(Reg8(A), Reg16Ind8Dec(HL)),
    0x3b => d.dec16(Reg16(SP)),
    0x3c => d.inc8(Reg8(A)),
    0x3d => d.dec8(Reg8(A)),
    0x3e => { let imm = fetchb(); d.ld8(Reg8(A), Imm8(imm)) }
    0x3f => d.ccf(),

    // 0x40-0x70
    0x40..0x75 | 0x77..0x7f
         => d.ld8(decode_addr(opcode >> 3), decode_addr(opcode)),
    0x76 => d.halt(),

    // 0x80
    0x80..0x87 => d.add8(decode_addr(opcode)),
    0x88..0x8f => d.adc(decode_addr(opcode)),

    // 0x90
    0x90..0x97 => d.sub(decode_addr(opcode)),
    0x98..0x9f => d.sbc(decode_addr(opcode)),

    // 0xa0
    0xa0..0xa7 => d.and(decode_addr(opcode)),
    0xa8..0xaf => d.xor(decode_addr(opcode)),

    // 0xb0
    0xb0..0xb7 => d.or(decode_addr(opcode)),
    0xb8..0xbf => d.cp(decode_addr(opcode)),

    // 0xc0
    0xc0 => d.ret(CondNZ),
    0xc1 => d.pop(Reg16(BC)),
    0xc2 => { let imm = fetchw(); d.jp(CondNZ, Imm16(imm)) }
    0xc3 => { let imm = fetchw(); d.jp(CondNone, Imm16(imm)) }
    0xc4 => { let imm = fetchw(); d.call(CondNZ, Imm16(imm)) }
    0xc5 => d.push(Reg16(BC)),
    0xc6 => { let imm = fetchb(); d.add8(Imm8(imm)) }
    0xc7 => d.rst(0x00),

    0xc8 => d.ret(CondZ),
    0xc9 => d.ret(CondNone),
    0xca => { let imm = fetchw(); d.jp(CondZ, Imm16(imm)) }
    0xcb => {
      let extra = fetchb();
      let addr = decode_addr(extra);

      match extra & 0xf8 {
        0x00 => d.rlc(addr),
        0x08 => d.rrc(addr),
        0x10 => d.rl(addr),
        0x18 => d.rr(addr),
        0x20 => d.sla(addr),
        0x28 => d.sra(addr),
        0x30 => d.swap(addr),
        0x38 => d.srl(addr),
        0x40..0x78 =>
                d.bit((extra >> 3) & 0b111, addr),
        0x80..0xb8 =>
                d.res((extra >> 3) & 0b111, addr),
        0xc0..0xf8 =>
                d.set((extra >> 3) & 0b111, addr),
        _ => fail!("logic error")
      }
    }
    0xcc => { let imm = fetchw(); d.call(CondZ, Imm16(imm)) }
    0xcd => { let imm = fetchw(); d.call(CondNone, Imm16(imm)) }
    0xce => { let imm = fetchb(); d.adc(Imm8(imm)) }
    0xcf => d.rst(0x08),

    // 0xd0
    0xd0 => d.ret(CondNC),
    0xd1 => d.pop(Reg16(DE)),
    0xd2 => { let imm = fetchw(); d.jp(CondNC, Imm16(imm)) }
    /*0xd3 => d.nop(),*/
    0xd4 => { let imm = fetchw(); d.call(CondNC, Imm16(imm)) }
    0xd5 => d.push(Reg16(DE)),
    0xd6 => { let imm = fetchb(); d.sub(Imm8(imm)) }
    0xd7 => d.rst(0x10),

    0xd8 => d.ret(CondC),
    0xd9 => d.reti(),
    0xda => { let imm = fetchw(); d.jp(CondC, Imm16(imm)) }
    /*0xdb => d.nop(),*/
    0xdc => { let imm = fetchw(); d.call(CondC, Imm16(imm)) }
    /*0xdd => d.nop(),*/
    0xde => { let imm = fetchb(); d.sbc(Imm8(imm)) }
    0xdf => d.rst(0x18),

    // 0xe0
    0xe0 => { let ind = fetchb(); d.ldh(Ind8(ind), Reg8(A)) }
    0xe1 => d.pop(Reg16(HL)),
    0xe2 => d.ld8(Reg8Ind8(C), Reg8(A)),
    /*0xe3 => d.nop(),*/
    /*0xe4 => d.nop(),*/
    0xe5 => d.push(Reg16(HL)),
    0xe6 => { let imm = fetchb(); d.and(Imm8(imm)) }
    0xe7 => d.rst(0x20),

    /*0xe8 => d.add16(Reg16(SP), ???),*/ // TODO
    0xe9 => d.jp(CondNone, Reg16Ind16(HL)),
    0xea => { let ind = fetchw(); d.ld8(Imm16Ind8(ind), Reg8(A)) }
    /*0xeb => d.nop(),*/
    /*0xec => d.nop(),*/
    /*0xed => d.nop(),*/
    0xee => { let imm = fetchb(); d.xor(Imm8(imm)) }
    0xef => d.rst(0x28),

    // 0xf0
    0xf0 => { let ind = fetchb(); d.ldh(Reg8(A), Ind8(ind)) }
    0xf1 => d.pop(Reg16(AF)),
    0xf2 => d.ld8(Reg8(A), Reg8Ind8(C)),
    0xf3 => d.di(),
    /*0xf4 => d.nop(),*/
    0xf5 => d.push(Reg16(AF)),
    0xf6 => { let imm = fetchb(); d.or(Imm8(imm)) }
    0xf7 => d.rst(0x30),

    /*0xf8 => d.ld(/*...*/),*/ // TODO
    0xf9 => d.ld16(Reg16(SP), Reg16(HL)),
    0xfa => { let ind = fetchw(); d.ld8(Reg8(A), Imm16Ind8(ind)) }
    0xfb => d.ei(),
    /*0xfc => d.nop(),*/
    /*0xfd => d.nop(),*/
    0xfe => { let imm = fetchb(); d.cp(Imm8(imm)) }
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

// Opcode implementation.
// Source: http://www.z80.info/z80code.txt
impl<M: mem::Mem> Decoder<()> for Cpu<M> {
  //
  // Misc/control
  //

  fn di  (&mut self)          { fail!("instruction not implemented: di") }
  fn ei  (&mut self)          { fail!("instruction not implemented: ei") }
  fn halt(&mut self)          { fail!("instruction not implemented: halt") }
  fn nop (&mut self)          { }
  fn stop(&mut self, val: u8) { fail!("instruction not implemented: stop") }

  //
  // Jump/call
  //

  fn call(&mut self, cond: Cond, addr: Addr16) {
    if cond.eval(self) {
      self.push(Reg16(PC));
      self.ld16(Reg16(PC), addr);
    }
  }

  fn jp(&mut self, cond: Cond, addr: Addr16) {
    if cond.eval(self) {
      self.ld16(Reg16(PC), addr);
    }
  }

  fn jr(&mut self, cond: Cond, rel: i8) {
    if cond.eval(self) {
      self.ld16(Reg16(PC), Imm16((self.regs.pc as i16 + rel as i16) as u16));
    }
  }

  fn ret(&mut self, cond: Cond) {
    if cond.eval(self) {
      self.pop(Reg16(PC));
    }
  }

  fn reti(&mut self) {
    // TODO: Tell someone an interrupt got handled?
    self.pop(Reg16(PC));
  }

  fn rst(&mut self, addr: u8) {
    self.call(CondNone, Imm16(addr as u16));
  }

  //
  // Load/store/move
  //

  fn ld8(&mut self, dst: Addr8,  src: Addr8) {
    let val = src.load(self);
    dst.store(self, val);
  }

  fn ld16(&mut self, dst: Addr16, src: Addr16) {
    let val = src.load(self);
    dst.store(self, val);
  }

  fn ldh (&mut self, dst: Addr8, src: Addr8) {
    self.ld8(dst, src);
  }

  fn pop(&mut self, dst: Addr16) {
    self.ld16(dst, Reg16Ind16(SP));
    self.regs.sp += 2;
  }

  fn push(&mut self, src: Addr16) {
    self.regs.sp -= 2;
    self.ld16(Reg16Ind16(SP), src);
  }

  //
  // Arithmetic/logic
  //

  fn adc(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a += src.load(self);
    if (self.regs.f & CARRY_FLAG) != 0 {
      self.regs.a += 1;
    }
  }

  fn add8(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a += src.load(self);
  }

  fn add16(&mut self, dst: Addr16, src: Addr16) {
    // TODO: Flags
    let op1 = dst.load(self);
    let op2 = src.load(self);
    dst.store(self, op1 + op2);
  }

  fn and(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a &= src.load(self);
  }

  fn ccf(&mut self) {
    // TODO: Flags
    self.regs.f ^= CARRY_FLAG;
  }

  fn cp(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a - src.load(self); // Senseless without flags, but well...
  }

  fn cpl(&mut self) {
    // TODO: Flags
    self.regs.a = !self.regs.a;
  }

  fn daa(&mut self) {
    fail!("instruction not implemented: daa");
  }

  fn dec8(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, val - 1);
  }

  fn dec16(&mut self, dst: Addr16) {
    let val = dst.load(self);
    dst.store(self, val - 1);
  }

  fn inc8(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, val + 1);
  }

  fn inc16(&mut self, dst: Addr16) {
    let val = dst.load(self);
    dst.store(self, val + 1);
  }

  fn or(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a |= src.load(self);
  }

  fn sbc(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a -= src.load(self);
    if (self.regs.f & CARRY_FLAG) != 0 {
      self.regs.a -= 1;
    }
  }

  fn scf(&mut self) {
    // TODO: Flags
    self.regs.f |= CARRY_FLAG;
  }

  fn sub(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a -= src.load(self);
  }

  fn xor(&mut self, src: Addr8) {
    // TODO: Flags
    self.regs.a ^= src.load(self);
  }

  //
  // Rotation/shift/bit
  //

  fn bit(&mut self, bit: u8, src: Addr8) {
    fail!("instruction not implemented: bit");
  }

  fn res(&mut self, bit: u8, dst: Addr8) {
    let val = dst.load(self);
    dst.store(self, val & !(1 << bit));
  }

  fn rl(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val << 1) | ((self.regs.f & CARRY_FLAG) >> CARRY_OFFSET));
  }

  fn rla(&mut self) {
    self.rl(Reg8(A));
  }

  fn rlc(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val << 1) | ((val & 0x80) >> 7));
  }

  fn rlca(&mut self) {
    self.rlc(Reg8(A));
  }

  fn rr(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val >> 1) | (((self.regs.f & CARRY_FLAG) >> CARRY_OFFSET) << 7));
  }

  fn rra(&mut self) {
    self.rr(Reg8(A));
  }

  fn rrc(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val >> 1) | ((val & 0x01) << 7));
  }

  fn rrca(&mut self) {
    self.rrc(Reg8(A));
  }

  fn set(&mut self, bit: u8, dst: Addr8) {
    let val = dst.load(self);
    dst.store(self, val | (1 << bit));
  }

  fn sla(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, val << 1);
  }

  fn sra(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val & 0x80) | (val >> 1));
  }

  fn srl(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, val >> 1);
  }

  fn swap(&mut self, dst: Addr8) {
    // TODO: Flags
    let val = dst.load(self);
    dst.store(self, (val >> 4) | (val << 4));
  }

  // Undefined/illegal
  fn undef(&mut self) { fail!("illegal instruction") }
}
