mod cartridge;
mod cpu;
mod disasm;
mod interrupt;
mod mem;
mod ram;
mod serial;
mod sound;
mod timer;
mod video;


//
// Memory Map
//

struct Dummy;
impl mem::Mem for Dummy {
  fn loadb(&mut self, addr: u16) -> u8 {
    warn!("load in unmapped memory at 0x{:04X}", addr);
    0xff
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    warn!("store in unmapped memory at 0x{:04X}", addr);
  }
}

struct MemMap {
  cart: ~cartridge::Cartridge,
  wram: ram::WorkRam,
  timer: timer::Timer,
  intr: interrupt::InterruptCtrl,
  sound: sound::Sound,
  video: video::Video,
  serial: serial::SerialIO,
  dummy: Dummy,
}

impl MemMap {
  fn mem_from_addr<'a>(&'a mut self, addr: u16) -> &'a mut mem::Mem {
    match addr {
      0x0000..0x7fff | // ROM banks
      0xa000..0xbfff   // External RAM
                      => &mut *self.cart as &mut mem::Mem,
      0x8000..0x9fff | // VRAM
      0xfe00..0xfe9f | // OAM
      0xff40..0xff4b   // Video I/O
                      => &mut self.video as &mut mem::Mem,
      0xc000..0xfdff | // WRAM (including echo area 0xe000-0xfdff)
      0xff80..0xfffe   // HRAM
                      => &mut self.wram as &mut mem::Mem,
      0xff01..0xff02  => &mut self.serial as &mut mem::Mem,
      0xff04..0xff07  => &mut self.timer as &mut mem::Mem,
      0xff0f | 0xffff => &mut self.intr as &mut mem::Mem,
      0xff10..0xff3f  => &mut self.sound as &mut mem::Mem,
      _ => &mut self.dummy as &mut mem::Mem,
      //_ => fail!("unmapped memory at 0x{:04X}", addr),
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
  println!("Type: {:u}", cart.cartridge_type);

  let memmap = MemMap {
    cart: cart,
    wram: ram::WorkRam::new(),
    timer: timer::Timer::new(),
    intr: interrupt::InterruptCtrl::new(),
    sound: sound::Sound,
    video: video::Video,
    serial: serial::SerialIO::new(),
    dummy: Dummy,
  };
  let mut cpu = cpu::Cpu::new(memmap);

  cpu.regs.pc = 0x100;
  loop {
    if log_enabled!(std::logging::DEBUG) {
      debug!("AF={:02X}{:02X} BC={:02X}{:02X} DE={:02X}{:02X} HL={:02X}{:02X} SP={:04X} PC={:04X} CY={:u}",
             cpu.regs.a, cpu.regs.f,
             cpu.regs.b, cpu.regs.c,
             cpu.regs.d, cpu.regs.e,
             cpu.regs.h, cpu.regs.l,
             cpu.regs.sp,
             cpu.regs.pc,
             cpu.cycles);
      let mut d = disasm::Disasm { mem: &mut cpu.mem, pc: cpu.regs.pc };
      debug!("{:s}", cpu::decode(&mut d));
    }
    let cycles = cpu.step();
    cpu.mem.timer.tick(cycles);
  }
}
