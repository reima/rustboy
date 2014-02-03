mod cartridge;
mod cpu;
mod disasm;
mod mem;

fn main() {
  let args = std::os::args();
  if args.len() != 2 {
    println!("Usage: {:s} rom.gb", args[0]);
    return;
  }

  let mut cart = cartridge::Cartridge::from_path(&Path::new(args[1]));
  let mut d = disasm::Disasm;
  let mut pc = 0u16;
  while pc < 0x8000u16 - 0x150u16 {
    let temp = pc;
    println!("{:04X}: {:s}", temp + 0x150, cpu::decode(&mut cart.rom, &mut pc, &mut d));
  }
}

