use mem;

//
// Sound
//

pub struct Sound;

impl mem::Mem for Sound {
  fn loadb(&mut self, addr: u16) -> u8 {
    0
  }

  fn storeb(&mut self, addr: u16, val: u8) {
  }
}
