use std::io::{File, SeekSet};
use std::vec::bytes::copy_memory;
use std::vec;

static HEADER_OFFSET: i64 = 0x100;

struct Header {
  entry: [u8, ..4],
  logo: [u8, ..48],
  title: [u8, ..15],
  cartridge_type: u8,
  rom_size: u8,
  ram_size: u8,
}

pub struct Cartridge {
  header: Header,
  rom: ~[u8],
}

impl Cartridge {
  pub fn from_path(path: &Path) -> Cartridge {
    Cartridge::from_file(&mut File::open(path).unwrap())
  }

  fn from_file(file: &mut File) -> Cartridge {
    let mut buffer = [0, ..80];
    file.seek(HEADER_OFFSET, SeekSet);
    file.read(buffer);

    let mut header = Header {
      entry: [0, ..4],
      logo: [0, ..48],
      title: [0, ..15],
      cartridge_type: buffer[67],
      rom_size: buffer[68],
      ram_size: buffer[69],
    };

    copy_memory(header.entry, buffer.slice(0, 4));
    copy_memory(header.logo, buffer.slice(4, 52));
    copy_memory(header.title, buffer.slice(52, 67));

    let rom_size_bytes = 1 << (15 + header.rom_size);
    let mut rom = vec::from_elem(rom_size_bytes, 0u8);
    file.read(rom);

    Cartridge { header: header, rom: rom }
  }
}
