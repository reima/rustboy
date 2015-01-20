pub trait Mem {
  fn loadb(&mut self, addr: u16) -> u8;
  fn storeb(&mut self, addr: u16, val: u8);

  fn loadw(&mut self, addr: u16) -> u16 {
    self.loadb(addr) as u16 | ((self.loadb(addr + 1) as u16) << 8)
  }

  fn storew(&mut self, addr: u16, val: u16) {
    self.storeb(addr, (val & 0xff) as u8);
    self.storeb(addr + 1, ((val >> 8) & 0xff) as u8);
  }
}

impl Mem for Vec<u8> {
  fn loadb(&mut self, addr: u16) -> u8 {
    (*self)[addr as uint]
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    (*self)[addr as uint] = val;
  }
}
