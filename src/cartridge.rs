use mem::Mem;
use std::cmp;
use std::fs::File;
use std::io::{Read, Result, Seek, SeekFrom};
use std::path::Path;

const HEADER_OFFSET: u64 = 0x100;
const ROM_BANK_SIZE: usize = 0x4000;

pub enum MBC {
    MBC1,
}

pub struct Cartridge {
    pub title: String,
    pub cartridge_type: u8,
    pub rom_size: u8,
    pub ram_size: u8,
    pub rom_banks: Vec<[u8; ROM_BANK_SIZE]>,
    pub rom_bank: u8,
    pub mbc: Option<MBC>,
}

impl Cartridge {
    pub fn from_path(path: &Path) -> Result<Cartridge> {
        Cartridge::from_file(&mut File::open(path).unwrap())
    }

    fn from_file(file: &mut File) -> Result<Cartridge> {
        use std::str;

        let mut header = [0u8; 80];
        file.seek(SeekFrom::Start(HEADER_OFFSET))?;
        file.read_exact(&mut header)?;

        let title = str::from_utf8(&header[0x34..0x43]).unwrap().to_string();

        let cartridge_type = header[0x47];
        let mbc = match cartridge_type {
            0x00 => None,
            0x01 => Some(MBC::MBC1),
            _ => panic!("unsupported cartridge type: 0x{:02X}", cartridge_type),
        };

        let rom_size = header[0x48];
        let rom_bank_count = match rom_size {
            0..=7 => 2 << rom_size,
            0x52 => 72,
            0x53 => 80,
            0x54 => 96,
            _ => panic!("unsupported ROM size: 0x{:02X}", rom_size),
        };
        let mut rom_banks = Vec::with_capacity(rom_bank_count);

        file.seek(SeekFrom::Start(0))?;

        for _ in 0..rom_bank_count {
            let mut bank = [0u8; ROM_BANK_SIZE];
            file.read_exact(&mut bank)?;
            rom_banks.push(bank);
        }

        let ram_size = header[0x49];
        if ram_size != 0x00 {
            panic!("unsupported RAM size: 0x{:02X}", ram_size);
        }

        let cart = Cartridge {
            title,
            cartridge_type,
            rom_size,
            ram_size,
            rom_banks,
            rom_bank: 1,
            mbc,
        };

        Ok(cart)
    }
}

impl Mem for Cartridge {
    fn loadb(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x3fff => self.rom_banks[0].loadb(addr),
            0x4000..=0x7fff => self.rom_banks[self.rom_bank as usize].loadb(addr - 0x4000),
            0xa000..=0xbfff => {
                debug!("RAM load at ${:04X}", addr);
                0xff
            }
            _ => {
                debug!("unsupported cartridge address ${:04X}", addr);
                0xff
            }
        }
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        match self.mbc {
            None => info!("store 0x{:02X} in cartridge ROM at ${:04X}", val, addr),
            Some(MBC::MBC1) => {
                match addr {
                    0x0000..=0x1fff => debug!("RAM enable"),
                    0x2000..=0x3fff => {
                        // set lower 5 bits of ROM bank
                        let bank_bits_0_4 = cmp::max(val & 0b1_1111, 1u8); // treat 0 as 1
                        self.rom_bank = (self.rom_bank & (0b1110_0000)) | bank_bits_0_4;
                    }
                    0x4000..=0x5fff => {
                        // set higher 2 bits of ROM bank
                        let bank_bits_5_6 = (val & 0b11) << 5;
                        self.rom_bank = (self.rom_bank & (0b1001_1111)) | bank_bits_5_6;
                    }
                    0x6000..=0x7fff => debug!("ROM/RAM mode select at ${:04X}", addr),
                    0xa000..=0xbfff => debug!("RAM store at ${:04X}", addr),
                    _ => debug!("unsupported cartridge address ${:04X}", addr),
                }
            }
        }
    }
}
