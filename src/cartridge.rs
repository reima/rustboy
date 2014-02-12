use mem::Mem;
use std::io;

static HEADER_OFFSET: i64 = 0x100;
static ROM_BANK_SIZE: uint = 0x4000;

enum MBC {
  MBC1
}

pub struct Cartridge {
  title: ~str,
  cartridge_type: u8,
  rom_size: u8,
  ram_size: u8,
  rom_banks: ~[~[u8]],
  rom_bank: u8,
  mbc: Option<MBC>,
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
    let mbc =
      match cartridge_type {
        0x00 => None,
        0x01 => Some(MBC1),
        _ => fail!("unsupported cartridge type: 0x{:02X}", cartridge_type)
      };

    let rom_size = header[0x48];
    let rom_bank_count =
      match rom_size {
        0..7 => 2 << rom_size,
        0x52 => 72,
        0x53 => 80,
        0x54 => 96,
        _ => fail!("unsupported ROM size: 0x{:02X}", rom_size),
      };
    let mut rom_banks = ~[];
    file.seek(0, io::SeekSet);
    for bank in range(0, rom_bank_count) {
      let mut bank = vec::from_elem(ROM_BANK_SIZE, 0u8);
      file.read(bank);
      rom_banks.push(bank);
    }

    let ram_size = header[0x49];
    if ram_size != 0x00 {
      fail!("unsupported RAM size: 0x{:02X}", ram_size);
    }

    Cartridge {
      title: title,
      cartridge_type: cartridge_type,
      rom_size: rom_size,
      ram_size: ram_size,
      rom_banks: rom_banks,
      rom_bank: 1,
      mbc: mbc,
    }
  }
}

impl Mem for Cartridge {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0x0000..0x3fff => self.rom_banks[0].loadb(addr),
      0x4000..0x7fff => self.rom_banks[self.rom_bank].loadb(addr - 0x4000),
      0xa000..0xbfff => { debug!("RAM load at ${:04X}", addr); 0xff },
      _ => { debug!("unsupported cartridge address ${:04X}", addr); 0xff },
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match self.mbc {
      None => info!("store 0x{:02X} in cartridge ROM at ${:04X}", val, addr),
      Some(MBC1) => {
        match addr {
          0x0000..0x1fff => debug!("RAM enable"),
          0x2000..0x3fff => { // set lower 5 bits of ROM bank
            let bank_bits_0_4 = (val & 0b11111).max(&1u8); // treat 0 as 1
            self.rom_bank = (self.rom_bank & (0b11100000)) | bank_bits_0_4;
          },
          0x4000..0x5fff => { // set higher 2 bits of ROM bank
            let bank_bits_5_6 = (val & 0b11) << 5;
            self.rom_bank = (self.rom_bank & (0b10011111)) | bank_bits_5_6;
          },
          0x6000..0x7fff => debug!("ROM/RAM mode select at ${:04X}", addr),
          0xa000..0xbfff => debug!("RAM store at ${:04X}", addr),
          _ => debug!("unsupported cartridge address ${:04X}", addr),
        }
      },
    }
  }
}
