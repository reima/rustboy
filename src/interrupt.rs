use mem;

//
// Interrupt Controller
//

pub const IRQ_VBLANK: u8 = 0b00001;
pub const IRQ_LCD:    u8 = 0b00010;
pub const IRQ_TIMER:  u8 = 0b00100;
pub const IRQ_SERIAL: u8 = 0b01000;
pub const IRQ_JOYPAD: u8 = 0b10000;

const IRQ_MASK: u8 = 0b11111;

pub struct InterruptCtrl {
  flag: u8,
  enable: u8,
}

impl InterruptCtrl {
  pub fn new() -> InterruptCtrl {
    InterruptCtrl { flag: 0, enable: 0 } // TODO: Is this the initial state?
  }

  pub fn irq(&mut self, num: u8) {
    self.flag |= num & IRQ_MASK;
  }
}

impl mem::Mem for InterruptCtrl {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0xff0f => self.flag,
      0xffff => self.enable,
      _ => panic!("invalid interrupt register"),
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match addr {
      0xff0f => self.flag = val,
      0xffff => self.enable = val,
      _ => panic!("invalid interrupt register"),
    }
  }
}
