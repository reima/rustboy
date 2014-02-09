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
    info!("load in unmapped memory at 0x{:04X}", addr);
    0xff
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    info!("store in unmapped memory at 0x{:04X}", addr);
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


fn disasm<M: mem::Mem>(mem: &mut M, addr: &mut u16) -> ~str {
  let mut d = disasm::Disasm { mem: mem, pc: *addr };
  let result = cpu::decode(&mut d);
  *addr = d.pc;
  result
}

fn disassemble<M: mem::Mem>(mem: &mut M, addr: u16) {
  let mut pc = addr;

  for _ in range(0, 5) {
    let start_addr = pc;
    let instr = disasm(mem, &mut pc);
    let end_addr = pc;
    print!("${:04X}", start_addr);
    for a in range(start_addr, end_addr) {
      print!(" {:02X}", mem.loadb(a));
    }
    println!("\t{:s}", instr);
  }
}

fn parse_addr(s: &str) -> Option<u16> {
  let mut slice = s;
  let mut radix = 10;
  if slice.starts_with("$") {
    slice = slice.slice_from(1);
    radix = 16;
  }
  std::num::from_str_radix::<u16>(slice, radix)
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
    video: video::Video::new(),
    serial: serial::SerialIO::new(),
    dummy: Dummy,
  };
  let mut cpu = cpu::Cpu::new(memmap);
  cpu.regs.pc = 0x100;

  let mut stdin = std::io::buffered::BufferedReader::new(std::io::stdio::stdin());

  let mut intr_listener = std::io::signal::Listener::new();
  intr_listener.register(std::io::signal::Interrupt);

  let mut done = false;
  let mut running = false;
  let mut breakpoints: ~[u16] = ~[];

  while !done {
    if running {
      match breakpoints.iter().find(|&bp| *bp == cpu.regs.pc) {
        Some(&bp) => {
          println!("Breakpoint at ${:04X}", bp);
          running = false
        },
        None => (),
      }

      match intr_listener.port.try_recv() {
        Some(std::io::signal::Interrupt) => {
          println("Interrupted");
          running = false
        },
        _ => (),
      }
    }

    if !running {
      loop {
        print("> ");
        std::io::stdio::flush();
        match stdin.read_line() {
          Some(line) => {
            let words = line.words().to_owned_vec();
            if words.len() >= 1 {
              let command = words[0];
              match command {
                &"q" => { done = true; break }, // quit
                &"s" => { break }, // step,
                &"r" => { running = true; break }, // run
                &"regs" => { // print registers
                  println!("AF={:02X}{:02X} BC={:02X}{:02X} DE={:02X}{:02X} HL={:02X}{:02X} SP={:04X} PC={:04X} CY={:u}",
                           cpu.regs.a, cpu.regs.f,
                           cpu.regs.b, cpu.regs.c,
                           cpu.regs.d, cpu.regs.e,
                           cpu.regs.h, cpu.regs.l,
                           cpu.regs.sp,
                           cpu.regs.pc,
                           cpu.cycles);
                },
                &"d" => { // disasm
                  let mut addr = cpu.regs.pc;
                  if words.len() >= 2 {
                    match parse_addr(words[1]) {
                      Some(a) => addr = a,
                      None    => error!("Invalid address: {:s}", words[1]),
                    }
                  }
                  disassemble(&mut cpu.mem, addr)
                },
                &"b" => { // breakpoint
                  if words.len() == 1 {
                    // show breakpoints
                    if breakpoints.len() == 0 {
                      println("No breakpoints");
                    } else {
                      println("Breakpoints:");
                      for bp in breakpoints.iter() {
                        println!("  ${:04X}", *bp);
                      }
                    }
                  } else if words.len() >= 2 {
                    // add/remove breakpoint
                    let mut add = true;
                    let mut arg = words[1];
                    if arg.starts_with("-") {
                      arg = arg.slice_from(1);
                      add = false;
                    }
                    match parse_addr(arg) {
                      Some(addr) => {
                        if add {
                          if !breakpoints.contains(&addr) {
                            breakpoints.push(addr);
                            breakpoints.sort();
                            println!("Breakpoint ${:04X} added", addr);
                          } else {

                          }
                        } else {
                          if breakpoints.contains(&addr) {
                            breakpoints.retain(|&bp| bp != addr);
                            println!("Breakpoint ${:04X} removed", addr);
                          } else {
                            error!("No such breakpoint ${:04X}", addr);
                          }
                        }
                      },
                      None => error!("Invalid address: {:s}", words[1]),
                    }
                  }
                },
                _ => error!("Unknown command: {:s}", command),
              }
            }
          },
          None => {
            println("\nType q to exit");
            intr_listener.port.recv();
          }
        }
      }
    }

    let cycles = cpu.step();
    cpu.mem.timer.tick(cycles);
    cpu.mem.video.tick(cycles);
  }
}
