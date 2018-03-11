pub trait Mem {
  fn loadb(&mut self, addr: u16) -> u8;
  fn storeb(&mut self, addr: u16, val: u8);

  fn loadw(&mut self, addr: u16) -> u16 {
    u16::from(self.loadb(addr)) | (u16::from(self.loadb(addr + 1)) << 8)
  }

  fn storew(&mut self, addr: u16, val: u16) {
    self.storeb(addr, (val & 0xff) as u8);
    self.storeb(addr + 1, ((val >> 8) & 0xff) as u8);
  }
}

impl Mem for Vec<u8> {
  fn loadb(&mut self, addr: u16) -> u8 {
    (*self)[addr as usize]
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    (*self)[addr as usize] = val;
  }
}

impl Mem for [u8] {
  fn loadb(&mut self, addr: u16) -> u8 {
    (*self)[addr as usize]
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    (*self)[addr as usize] = val;
  }
}
