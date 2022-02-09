use crate::mem;

//
// Sound
//

pub struct Sound;

impl mem::Mem for Sound {
    fn loadb(&self, _: u16) -> u8 {
        0
    }

    fn storeb(&mut self, _: u16, _: u8) {}
}
