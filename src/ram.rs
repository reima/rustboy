use crate::mem;

//
// Work RAM
//

pub struct WorkRam {
    data: [u8; 0x207f], // 0x2000 WRAM + 0x7f HRAM
}

impl WorkRam {
    pub fn new() -> WorkRam {
        WorkRam {
            data: [0u8; 0x207f],
        }
    }
}

fn internal_addr(addr: u16) -> u16 {
    match addr {
        0xc000..=0xdfff => addr - 0xc000,          // WRAM
        0xe000..=0xfdff => addr - 0xe000,          // WRAM echo
        0xff80..=0xfffe => addr - 0xff80 + 0x2000, // HRAM
        _ => panic!("invalid WRAM address: 0x{:04X}", addr),
    }
}

impl mem::Mem for WorkRam {
    fn loadb(&self, addr: u16) -> u8 {
        self.data[internal_addr(addr) as usize]
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        self.data[internal_addr(addr) as usize] = val
    }
}
