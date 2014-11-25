use mem;

//
// Sound
//

pub struct Sound;

impl mem::Mem for Sound {
  fn loadb(&mut self, _ /* addr */: u16) -> u8 {
    0
  }

  fn storeb(&mut self, _ /* addr */: u16, _ /* val */: u8) {
  }
}
