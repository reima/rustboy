use mem;
use std::io;

static HEADER_OFFSET: i64 = 0x100;

pub struct Cartridge {
  title: ~str,
  cartridge_type: u8,
  rom_size: u8,
  ram_size: u8,
  rom: ~[u8],
}

impl Cartridge {
  pub fn from_path(path: &Path) -> Cartridge {
    Cartridge::from_file(&mut io::File::open(path).unwrap())
  }

  fn from_file(file: &mut io::File) -> Cartridge {
    use std::str;
    use std::vec;

    let mut header = [0, ..80];
    file.seek(HEADER_OFFSET, io::SeekSet);
    file.read(header);

    let title = str::from_utf8(header.slice(0x34, 0x43)).to_owned();
    let cartridge_type = header[0x47];
    let rom_size = header[0x48];
    let ram_size = header[0x49];

    let rom_size_bytes = 1 << (15 + rom_size);
    let mut rom = vec::from_elem(rom_size_bytes, 0u8);
    file.seek(0, io::SeekSet);
    file.read(rom);

    Cartridge {
      title: title,
      cartridge_type: cartridge_type,
      rom_size: rom_size,
      ram_size: ram_size,
      rom: rom,
    }
  }
}

impl mem::Mem for Cartridge {
  fn loadb(&mut self, addr: u16) -> u8 {
    self.rom.loadb(addr)
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    self.rom.storeb(addr, val)
  }
}
