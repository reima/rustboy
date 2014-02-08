use mem;

//
// Video
//

pub struct Video;

impl mem::Mem for Video {
  fn loadb(&mut self, addr: u16) -> u8 {
    0xff
  }

  fn storeb(&mut self, addr: u16, val: u8) {
  }
}
