use mem;

//
// Serial I/O
//

static SERIAL_TRANSFER_FLAG: u8 = 0x80;

pub struct SerialIO {
  data: u8, // SB register
  control: u8, // SC register
}

impl SerialIO {
  pub fn new() -> SerialIO {
    SerialIO { data: 0, control: 0 }
  }
}

impl mem::Mem for SerialIO {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0xff01 => self.data,
      0xff02 => self.control,
      _ => fail!("invalid serial I/O register"),
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match addr {
      0xff01 => {
        self.data = val;
      }
      0xff02 => {
        self.control = val;
        if (val & SERIAL_TRANSFER_FLAG) != 0 {
          // Start transfer
          // TODO: This should be done with 8192 bits per second
          // Debug output
          print!("{:c}", self.data.to_ascii().to_char());
          self.data = 0xff; // No external GameBoy present, receive dummy value
          // Reset transfer flag to indicate transfer has finished
          self.control &= !SERIAL_TRANSFER_FLAG;
        }
      }
      _ => fail!("invalid serial I/O register"),
    }
  }
}
