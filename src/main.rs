mod cartridge;
mod cpu;
mod disasm;
mod mem;

struct WorkRam {
  priv data: [u8, ..0x2000]
}

impl WorkRam {
  fn new() -> WorkRam {
    WorkRam { data: [0u8, ..0x2000] }
  }

  fn internal_addr(self, addr: u16) -> u16 {
    match addr {
      0xc000..0xdfff => addr - 0xc000,
      0xe000..0xfdff => addr - 0xe000, // mirror
      _ => fail!("invalid WRAM address: 0x{:04X}", addr),
    }
  }
}

impl mem::Mem for WorkRam {
  fn loadb(&mut self, addr: u16) -> u8 {
    self.data[self.internal_addr(addr)]
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    self.data[self.internal_addr(addr)] = val
  }
}

struct MemMap {
  cart: ~cartridge::Cartridge,
  wram: ~WorkRam,
}

impl MemMap {
  fn mem_from_addr<'a>(&'a mut self, addr: u16) -> &'a mut mem::Mem {
    match addr {
      0x0000..0x7fff | // ROM banks
      0xa000..0xbfff   // External RAM
                     => &mut *self.cart as &mut mem::Mem,
      // 0x8000..0x9fff | // VRAM
      // 0xfe00..0xfe9f   // OAM
      //                => &mut *self.video as &mut mem::Mem,
       0xc000..0xfdff   // WRAM banks (including mirror 0xe000-0xfdff)
                      => &mut *self.wram as &mut mem::Mem,
      // 0xff00..0xff7f | // I/O registers (includes Video, Sound, Joypad...)
      // 0xffff           // Interrupt enable register
      //                => &mut *self.ioreg as &mut mem::Mem,
      // 0xff80..0xfffe => &mut *self.hram as &mut mem::Mem,
      _ => fail!("unmapped memory at 0x{:04X}", addr),
    }
  }
}

impl mem::Mem for MemMap {
  fn loadb(&mut self, addr: u16) -> u8 {
    self.mem_from_addr(addr).loadb(addr)
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    self.mem_from_addr(addr).storeb(addr, val)
  }
}

fn main() {
  let args = std::os::args();
  if args.len() != 2 {
    println!("Usage: {:s} rom.gb", args[0]);
    return;
  }

  let cart = ~cartridge::Cartridge::from_path(&Path::new(args[1]));
  println!("Name: {:s}", cart.title);

  let memmap = MemMap { cart: cart, wram: ~WorkRam::new() };
  let mut cpu = cpu::Cpu::new(memmap);

  cpu.regs.pc = 0x100;
  loop {
    print!("AF={:02X}{:02X} ", cpu.regs.a, cpu.regs.f);
    print!("BC={:02X}{:02X} ", cpu.regs.b, cpu.regs.c);
    print!("DE={:02X}{:02X} ", cpu.regs.d, cpu.regs.e);
    print!("HL={:02X}{:02X} ", cpu.regs.h, cpu.regs.l);
    print!("SP={:04X} ", cpu.regs.sp);
    print!("PC={:04X} ", cpu.regs.pc);
    {
      let mut d = disasm::Disasm { mem: &mut cpu.mem, pc: cpu.regs.pc };
      println(cpu::decode(&mut d));
    }
    cpu.step();
  }
}

