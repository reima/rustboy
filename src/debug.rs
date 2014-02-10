use mem::Mem;
use disasm;
use cpu;
use std::num::from_str_radix;
use std::io::{buffered, signal, stdio};

//
// Debugger
//

fn disasm<M: Mem>(mem: &mut M, addr: &mut u16) -> ~str {
  let mut d = disasm::Disasm { mem: mem, pc: *addr };
  let result = cpu::decode(&mut d);
  *addr = d.pc;
  result
}

fn disassemble<M: Mem>(mem: &mut M, addr: u16) {
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

fn print_mem<M: Mem>(mem: &mut M, addr: u16) {
  println("\t  0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F");

  let mut base = addr & 0xfff0;
  let mut start_offset = addr & 0xf;

  for _ in range(0, 4) {
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
  breakpoints: ~[u16],
  intr_listener: signal::Listener,
}

pub enum DebuggerCommand {
  Quit,
  Step,
  Run,
}

impl Debugger {
  pub fn new() -> Debugger {
    let mut intr_listener = signal::Listener::new();
    intr_listener.register(signal::Interrupt);

    Debugger { breakpoints: ~[], intr_listener: intr_listener }
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

  fn dispatch<M: Mem>(&mut self, cpu: &mut cpu::Cpu<M>, words: &[&str]) -> Option<DebuggerCommand> {
    if words.len() == 0 {
      return None;
    }

    let command = words[0];
    match command {
      &"q" => Some(Quit), // quit
      &"s" => Some(Step), // step
      &"r" => Some(Run), // run
      &"regs" => { print_regs(cpu); None }, // print registers
      &"m" => { // print memory
        if words.len() >= 2 {
          match parse_addr(words[1]) {
            Some(addr) => print_mem(&mut cpu.mem, addr),
            None       => error!("Invalid address: {:s}", words[1]),
          }
        }
        None
      },
      &"d" => { // disasm
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
      &"b" => { // breakpoint
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
      _ => {
        error!("Unknown command: {:s}", command);
        None
      }
    }
  }

  pub fn prompt<M: Mem>(&mut self, cpu: &mut cpu::Cpu<M>) -> DebuggerCommand {
    let mut stdin = buffered::BufferedReader::new(stdio::stdin());

    loop {
      print("> ");
      stdio::flush();
      match stdin.read_line() {
        Some(line) => {
          let words = line.words().to_owned_vec();
          match self.dispatch(cpu, words) {
            Some(command) => return command,
            None => (),
          }
        },
        None => {
          println("\nType q to exit");
          self.intr_listener.port.recv(); // grok the interrupt
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
      None => (),
    }

    match self.intr_listener.port.try_recv() {
      Some(signal::Interrupt) => {
        println("Interrupted");
        return true
      },
      _ => (),
    }

    false
  }
}
