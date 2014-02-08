use mem;

//
// Interrupt Controller
//

pub struct InterruptCtrl {
  flag: u8,
  enable: u8,
}

impl InterruptCtrl {
  pub fn new() -> InterruptCtrl {
    InterruptCtrl { flag: 0, enable: 0 } // TODO: Is this the initial state?
  }
}

impl mem::Mem for InterruptCtrl {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0xff0f => self.flag,
      0xffff => self.enable,
      _ => fail!("invalid interrupt register"),
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match addr {
      0xff0f => self.flag = val,
      0xffff => self.enable = val,
      _ => fail!("invalid interrupt register"),
    }
  }
}
