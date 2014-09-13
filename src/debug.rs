use mem::Mem;
use cpu;
use disasm;
use std::io::stdio::{print, println};
use std::io::{stdio, File, BufferedReader, IoResult};
use std::num::from_str_radix;

//
// Debugger
//

fn disasm<M: Mem>(mem: &mut M, addr: &mut u16) -> String {
  let mut d = disasm::Disasm { mem: mem, pc: *addr };
  let result = cpu::decode(&mut d);
  *addr = d.pc;
  result
}

fn disassemble<M: Mem>(mem: &mut M, addr: u16) {
  let mut pc = addr;

  for _ in range(0u, 5u) {
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

fn print_mem<M: Mem>(mem: &mut M, addr: u16) {
  println("\t  0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F");

  let mut base = addr & 0xfff0;
  let mut start_offset = addr & 0xf;

  for _ in range(0u, 4u) {
    print!("${:04X}\t", base);
    for offset in range(0u16, 16u16) {
      if offset == 8 {
        print(" ");
      }
      if offset >= start_offset {
        print!(" {:02X}", mem.loadb(base + offset));
      } else {
        print("   ");
      }
    }
    println("");
    base += 0x10;
    start_offset = 0;

    if base < addr { // wrap-around
      break;
    }
  }
}

fn print_regs<M>(cpu: &cpu::Cpu<M>) {
  println!("AF={:02X}{:02X} BC={:02X}{:02X} DE={:02X}{:02X} HL={:02X}{:02X} SP={:04X} PC={:04X} CY={:u}",
           cpu.regs.a, cpu.regs.f,
           cpu.regs.b, cpu.regs.c,
           cpu.regs.d, cpu.regs.e,
           cpu.regs.h, cpu.regs.l,
           cpu.regs.sp,
           cpu.regs.pc,
           cpu.cycles);
}

fn expand_tile_row(tile: &[u8], palette: u8, row: uint, pixels: &mut [u8]) {
  for col in range(0u, 8u) {
    let low_bit  = (tile[2*row]   >> (7 - col)) & 1;
    let high_bit = (tile[2*row+1] >> (7 - col)) & 1;
    let value = (high_bit << 1) | low_bit;
    let color = 3 - ((palette >> (2 * value as uint)) & 0b11);
    pixels[col] = color;
  }
}

fn write_pgm(path: &Path, width: uint, height: uint, data: &[u8]) -> IoResult<()> {
  let mut output = File::create(path).unwrap();
  try!(output.write_line("P5"));
  try!(output.write_line(format!("{:u} {:u}", width, height).as_slice()));
  try!(output.write_line("3"));
  try!(output.write(data));

  Ok(())
}

fn dump_tiles<M: Mem>(m: &mut M) -> IoResult<()> {
  let mut data = [0u8, ..16*24*8*8];

  for num in range(0u, 384u) {
    let mut tile = [0u8, ..16];
    for offset in range(0u, 16u) {
      tile[offset] = m.loadb(0x8000u16 + num as u16 * 16u16 + offset as u16);
    }
    let row = num / 16;
    let col = num % 16;
    let pixels = data.mut_slice_from((row * 16 * 8 + col)*8);
    for row in range(0u, 8u) {
      expand_tile_row(tile, 0xe4, row, pixels.mut_slice_from(16*8*row));
    }
  }

  write_pgm(&Path::new("tiles.pgm"), 16*8, 24*8, data)
}

fn dump_bg<M: Mem>(m: &mut M) -> IoResult<()> {
  let mut tile_base = 0x8800;
  let mut tile_bias = 128u8;
  let mut map_base = 0x9800;

  let lcdc = m.loadb(0xff40);
  if (lcdc & 0b10000) != 0 {
    tile_base = 0x8000;
    tile_bias = 0u8;
  }
  if (lcdc & 0b01000) != 0 {
    map_base = 0x9c00;
  }

  // Load tiles
  let mut tiles = [0xffu8, ..256*16];
  for offset in range(0u, 256*16) {
    tiles[offset] = m.loadb((tile_base + offset) as u16);
  }

  // Load map
  let mut map = [0u8, ..32*32];
  for offset in range(0u, 32u*32u) {
    map[offset] = m.loadb((map_base + offset) as u16);
  }

  // Load palette
  let pal = m.loadb(0xff47);

  let mut data = [0u8, ..32*32*8*8];
  let row_pitch = 32*8;

  for row in range(0u, 32u) {
    for col in range(0u, 32u) {
      let tile_num = (map[row*32 + col] + tile_bias) as uint;
      let tile = tiles.slice_from(tile_num * 16);
      let pixels = data.mut_slice_from((row*row_pitch + col)*8);
      for tile_row in range(0u, 8u) {
        expand_tile_row(tile,
                        pal,
                        tile_row,
                        pixels.mut_slice_from(tile_row*row_pitch));
      }
    }
  }

  write_pgm(&Path::new("bg.pgm"), 32*8, 32*8, data)
}

fn parse_addr(s: &str) -> Option<u16> {
  let mut slice = s;
  let mut radix = 10;
  if slice.starts_with("$") {
    slice = slice.slice_from(1);
    radix = 16;
  }
  from_str_radix::<u16>(slice, radix)
}

pub struct Debugger {
  breakpoints: Vec<u16>,
}

pub enum DebuggerCommand {
  Quit,
  Step,
  Run,
}

impl Debugger {
  pub fn new() -> Debugger {
    Debugger { breakpoints: vec!() }
  }

  fn show_breakpoints(&self) {
    if self.breakpoints.len() == 0 {
      println("No breakpoints");
    } else {
      println("Breakpoints:");
      for bp in self.breakpoints.iter() {
        println!("  ${:04X}", *bp);
      }
    }
  }

  fn add_breakpoint(&mut self, addr: u16) {
    if !self.breakpoints.contains(&addr) {
      self.breakpoints.push(addr);
      self.breakpoints.sort();
      println!("Breakpoint ${:04X} added", addr);
    } else {
      println!("Breakpoint ${:04X} already exists", addr);
    }
  }

  fn remove_breakpoint(&mut self, addr: u16) {
    if self.breakpoints.contains(&addr) {
      self.breakpoints.retain(|&bp| bp != addr);
      println!("Breakpoint ${:04X} removed", addr);
    } else {
      error!("No such breakpoint ${:04X}", addr);
    }
  }

  fn dispatch<M: Mem>(&mut self, cpu: &mut cpu::Cpu<M>, words: Vec<&str>) -> Option<DebuggerCommand> {
    if words.len() == 0 {
      return None;
    }

    let command = words[0];
    match command {
      "q" => Some(Quit), // quit
      "s" => Some(Step), // step
      "r" => Some(Run), // run
      "regs" => { print_regs(cpu); None }, // print registers
      "m" => { // print memory
        if words.len() >= 2 {
          match parse_addr(words[1]) {
            Some(addr) => print_mem(&mut cpu.mem, addr),
            None       => error!("Invalid address: {:s}", words[1]),
          }
        }
        None
      },
      "d" => { // disasm
        let mut addr = cpu.regs.pc;
        if words.len() >= 2 {
          match parse_addr(words[1]) {
            Some(a) => addr = a,
            None    => error!("Invalid address: {:s}", words[1]),
          }
        }
        disassemble(&mut cpu.mem, addr);
        None
      },
      "b" => { // breakpoint
        if words.len() == 1 {
          self.show_breakpoints();
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
                self.add_breakpoint(addr);
              } else {
                self.remove_breakpoint(addr);
              }
            },
            None => error!("Invalid address: {:s}", words[1]),
          }
        }
        None
      },
      "tiles" => { // dump video tiles
        match dump_tiles(&mut cpu.mem) {
          Err(e) => error!("I/O error: {}", e),
          _ => (),
        }
        None
      },
      "bg" => { // dump video bg
        match dump_bg(&mut cpu.mem) {
          Err(e) => error!("I/O error: {}", e),
          _ => (),
        }
        None
      },
      _ => {
        error!("Unknown command: {:s}", command);
        None
      }
    }
  }

  pub fn prompt<M: Mem>(&mut self, cpu: &mut cpu::Cpu<M>) -> DebuggerCommand {
    let mut stdin = BufferedReader::new(stdio::stdin());

    loop {
      print("> ");
      stdio::flush();
      match stdin.read_line() {
        Ok(line) => {
          let words = line.as_slice().words().collect::<Vec<&str>>();
          match self.dispatch(cpu, words) {
            Some(command) => return command,
            None => (),
          }
        },
        Err(_) => {
          println("\nType q to exit");
        }
      }
    }
  }

  pub fn should_break<M>(&self, cpu: &cpu::Cpu<M>) -> bool {
    match self.breakpoints.iter().find(|&bp| *bp == cpu.regs.pc) {
      Some(&bp) => {
        println!("Breakpoint at ${:04X}", bp);
        return true
      },
      None => false,
    }
  }
}
