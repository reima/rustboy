mod cartridge;
mod cpu;
mod disasm;
mod mem;

fn main() {
  let mut d = disasm::Disasm;

  let mut cart = cartridge::Cartridge::from_path(&Path::new("tetris.gb"));
  let mut pc = 0u16;
  while pc < 0x8000u16 - 0x150u16 {
    let temp = pc;
    println!("{:04X}: {:s}", temp + 0x150, cpu::decode(&mut cart.rom, &mut pc, &mut d));
  }
}
