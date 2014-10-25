use mem;
use std::io::Writer;

//
// Serial I/O
//

const SERIAL_TRANSFER_FLAG: u8 = 0x80;

pub struct SerialIO<'a> {
  data: u8, // SB register
  control: u8, // SC register
  writer: Option<Box<Writer + 'a>>,
}

impl<'a> SerialIO<'a> {
  pub fn new(writer: Option<Box<Writer + 'a>>) -> SerialIO<'a> {
    SerialIO { data: 0, control: 0, writer: writer }
  }
}

impl<'a> mem::Mem for SerialIO<'a> {
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
          match self.writer {
            Some(ref mut writer) => { let r = writer.write_u8(self.data); },
            None => (),
          }
          // No external GameBoy present, receive dummy value
          self.data = 0xff;
          // Reset transfer flag to indicate transfer has finished
          self.control &= !SERIAL_TRANSFER_FLAG;
        }
      }
      _ => fail!("invalid serial I/O register"),
    }
  }
}
