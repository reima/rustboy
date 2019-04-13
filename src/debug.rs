use cpu;
use disasm;
use mem::Mem;
use std::fs::File;
use std::io::{stdin, stdout, Result, Write};
use std::path::Path;

//
// Debugger
//

fn disasm<M: Mem>(mem: &mut M, addr: &mut u16) -> String {
    let mut d = disasm::Disasm { mem, pc: *addr };
    let result = cpu::decode(&mut d);
    *addr = d.pc;
    result
}

fn disassemble<M: Mem>(mem: &mut M, addr: u16) {
    let mut pc = addr;

    for _ in 0usize..5usize {
        let start_addr = pc;
        let instr = disasm(mem, &mut pc);
        let end_addr = pc;
        print!("${:04X}", start_addr);
        for a in start_addr..end_addr {
            print!(" {:02X}", mem.loadb(a));
        }
        println!("\t{}", instr);
    }
}

fn print_mem<M: Mem>(mem: &mut M, addr: u16) {
    println!("\t  0  1  2  3  4  5  6  7   8  9  A  B  C  D  E  F");

    let mut base = addr & 0xfff0;
    let mut start_offset = addr & 0xf;

    for _ in 0usize..4usize {
        print!("${:04X}\t", base);
        for offset in 0u16..16u16 {
            if offset == 8 {
                print!(" ");
            }
            if offset >= start_offset {
                print!(" {:02X}", mem.loadb(base + offset));
            } else {
                print!("   ");
            }
        }
        println!();
        base += 0x10;
        start_offset = 0;

        if base < addr {
            // wrap-around
            break;
        }
    }
}

fn print_regs<M>(cpu: &cpu::Cpu<M>) {
    println!(
        "AF={:02X}{:02X} BC={:02X}{:02X} DE={:02X}{:02X} HL={:02X}{:02X} SP={:04X} PC={:04X} CY={}",
        cpu.regs.a,
        cpu.regs.f,
        cpu.regs.b,
        cpu.regs.c,
        cpu.regs.d,
        cpu.regs.e,
        cpu.regs.h,
        cpu.regs.l,
        cpu.regs.sp,
        cpu.regs.pc,
        cpu.cycles
    );
}

fn expand_tile_row(tile: &[u8], palette: u8, row: usize, pixels: &mut [u8]) {
    for col in 0usize..8usize {
        let low_bit = (tile[2 * row] >> (7 - col)) & 1;
        let high_bit = (tile[2 * row + 1] >> (7 - col)) & 1;
        let value = (high_bit << 1) | low_bit;
        let color = 3 - ((palette >> (2 * value)) & 0b11);
        pixels[col] = color;
    }
}

fn write_pgm(path: &Path, width: usize, height: usize, data: &[u8]) -> Result<()> {
    let mut output = File::create(path).unwrap();
    try!(write!(&mut output, "P5\n{} {}\n3\n", width, height));
    try!(output.write_all(data));

    Ok(())
}

fn dump_tiles<M: Mem>(m: &mut M) -> Result<()> {
    let mut data = [0u8; 16 * 24 * 8 * 8];

    for num in 0usize..384usize {
        let mut tile = [0u8; 16];
        for offset in 0usize..16usize {
            tile[offset] = m.loadb(0x8000u16 + num as u16 * 16u16 + offset as u16);
        }
        let row = num / 16;
        let col = num % 16;
        let pixels = &mut data[(row * 16 * 8 + col) * 8..];
        for row in 0usize..8usize {
            expand_tile_row(&tile, 0xe4, row, &mut pixels[16 * 8 * row..]);
        }
    }

    write_pgm(Path::new("tiles.pgm"), 16 * 8, 24 * 8, &data)
}

fn dump_bg<M: Mem>(m: &mut M) -> Result<()> {
    let mut tile_base = 0x8800;
    let mut tile_bias = 128u8;
    let mut map_base = 0x9800;

    let lcdc = m.loadb(0xff40);
    if (lcdc & 0b1_0000) != 0 {
        tile_base = 0x8000;
        tile_bias = 0u8;
    }
    if (lcdc & 0b0_1000) != 0 {
        map_base = 0x9c00;
    }

    // Load tiles
    let mut tiles = [0xffu8; 256 * 16];
    for offset in 0usize..256 * 16 {
        tiles[offset] = m.loadb((tile_base + offset) as u16);
    }

    // Load map
    let mut map = [0u8; 32 * 32];
    for offset in 0usize..32usize * 32usize {
        map[offset] = m.loadb((map_base + offset) as u16);
    }

    // Load palette
    let pal = m.loadb(0xff47);

    let mut data = [0u8; 32 * 32 * 8 * 8];
    let row_pitch = 32 * 8;

    for row in 0usize..32usize {
        for col in 0usize..32usize {
            let tile_num = map[row * 32 + col] + tile_bias;
            let tile = &tiles[tile_num as usize * 16..];
            let pixels = &mut data[(row * row_pitch + col) * 8..];
            for tile_row in 0usize..8usize {
                expand_tile_row(tile, pal, tile_row, &mut pixels[tile_row * row_pitch..]);
            }
        }
    }

    write_pgm(Path::new("bg.pgm"), 32 * 8, 32 * 8, &data)
}

fn parse_addr(s: &str) -> Option<u16> {
    let mut slice = s;
    let radix = if slice.starts_with('$') {
        slice = &slice[1..];
        16
    } else {
        10
    };
    u16::from_str_radix(slice, radix).ok()
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
        Debugger {
            breakpoints: vec![],
        }
    }

    fn show_breakpoints(&self) {
        if self.breakpoints.is_empty() {
            println!("No breakpoints");
        } else {
            println!("Breakpoints:");
            for bp in &self.breakpoints {
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

    fn dispatch<M: Mem>(
        &mut self,
        cpu: &mut cpu::Cpu<M>,
        words: &[&str],
    ) -> Option<DebuggerCommand> {
        if words.is_empty() {
            return None;
        }

        let command = words[0];
        match command {
            "q" => Some(DebuggerCommand::Quit), // quit
            "s" => Some(DebuggerCommand::Step), // step
            "r" => Some(DebuggerCommand::Run),  // run
            "regs" => {
                print_regs(cpu);
                None
            } // print registers
            "m" => {
                // print memory
                if words.len() >= 2 {
                    match parse_addr(words[1]) {
                        Some(addr) => print_mem(&mut cpu.mem, addr),
                        None => error!("Invalid address: {}", words[1]),
                    }
                }
                None
            }
            "d" => {
                // disasm
                let mut addr = cpu.regs.pc;
                if words.len() >= 2 {
                    match parse_addr(words[1]) {
                        Some(a) => addr = a,
                        None => error!("Invalid address: {}", words[1]),
                    }
                }
                disassemble(&mut cpu.mem, addr);
                None
            }
            "b" => {
                // breakpoint
                if words.len() == 1 {
                    self.show_breakpoints();
                } else if words.len() >= 2 {
                    // add/remove breakpoint
                    let mut add = true;
                    let mut arg = words[1];
                    if arg.starts_with('-') {
                        arg = &arg[1..];
                        add = false;
                    }
                    match parse_addr(arg) {
                        Some(addr) => {
                            if add {
                                self.add_breakpoint(addr);
                            } else {
                                self.remove_breakpoint(addr);
                            }
                        }
                        None => error!("Invalid address: {}", words[1]),
                    }
                }
                None
            }
            "tiles" => {
                // dump video tiles
                if let Err(e) = dump_tiles(&mut cpu.mem) {
                    error!("I/O error: {}", e);
                }
                None
            }
            "bg" => {
                // dump video bg
                if let Err(e) = dump_bg(&mut cpu.mem) {
                    error!("I/O error: {}", e);
                }
                None
            }
            _ => {
                error!("Unknown command: {}", command);
                None
            }
        }
    }

    pub fn prompt<M: Mem>(&mut self, cpu: &mut cpu::Cpu<M>) -> DebuggerCommand {
        loop {
            print!("> ");
            stdout().flush().unwrap();
            let mut line = String::new();
            match stdin().read_line(&mut line) {
                Ok(_) => {
                    fn is_whitespace(c: char) -> bool {
                        c.is_whitespace()
                    }
                    let is_whitespace: fn(char) -> bool = is_whitespace;
                    let words = line.split(is_whitespace).collect::<Vec<_>>();
                    if let Some(command) = self.dispatch(cpu, &words) {
                        return command;
                    }
                }
                Err(_) => {
                    println!("\nType q to exit");
                }
            }
        }
    }

    pub fn should_break<M>(&self, cpu: &cpu::Cpu<M>) -> bool {
        match self.breakpoints.iter().find(|&bp| *bp == cpu.regs.pc) {
            Some(&bp) => {
                println!("Breakpoint at ${:04X}", bp);
                true
            }
            None => false,
        }
    }
}
