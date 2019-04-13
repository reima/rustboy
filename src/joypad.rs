use mem;

//
// Joypad
//

const INPUT_RIGHT: u8 = 0b0000_0001;
const INPUT_LEFT: u8 = 0b0000_0010;
const INPUT_UP: u8 = 0b0000_0100;
const INPUT_DOWN: u8 = 0b0000_1000;
const INPUT_BUTTON_A: u8 = 0b0000_0001;
const INPUT_BUTTON_B: u8 = 0b0000_0010;
const INPUT_SELECT: u8 = 0b0000_0100;
const INPUT_START: u8 = 0b0000_1000;
const SELECT_DIRECTION_KEYS: u8 = 0b0001_0000;
const SELECT_BUTTON_KEYS: u8 = 0b0010_0000;

const INPUT_MASK: u8 = 0b0000_1111;
const SELECT_MASK: u8 = 0b0011_0000;

pub enum Button {
    Right = 0,
    Left = 1,
    Up = 2,
    Down = 3,
    A = 4,
    B = 5,
    Select = 6,
    Start = 7,
}

pub struct Joypad {
    p1: u8,             // P1 register
    pressed: [bool; 8], // Button pressed state
}

impl Joypad {
    pub fn new() -> Joypad {
        Joypad {
            p1: 0xcf,
            pressed: [false; 8],
        }
    }

    pub fn set_button(&mut self, button: Button, pressed: bool) {
        self.pressed[button as usize] = pressed;
        self.update_input();
    }

    // pub fn reset(&mut self) {
    //   for p in self.pressed.iter_mut() {
    //     *p = false;
    //   }
    // }

    fn update_input(&mut self) {
        // All bits are low-active, i.e. 0 means selected/pressed
        let mut input = INPUT_MASK;

        if (self.p1 & SELECT_DIRECTION_KEYS) == 0 {
            if self.pressed[Button::Right as usize] {
                input &= !INPUT_RIGHT;
            }
            if self.pressed[Button::Left as usize] {
                input &= !INPUT_LEFT;
            }
            if self.pressed[Button::Up as usize] {
                input &= !INPUT_UP;
            }
            if self.pressed[Button::Down as usize] {
                input &= !INPUT_DOWN;
            }
        }

        if (self.p1 & SELECT_BUTTON_KEYS) == 0 {
            if self.pressed[Button::A as usize] {
                input &= !INPUT_BUTTON_A;
            }
            if self.pressed[Button::B as usize] {
                input &= !INPUT_BUTTON_B;
            }
            if self.pressed[Button::Select as usize] {
                input &= !INPUT_SELECT;
            }
            if self.pressed[Button::Start as usize] {
                input &= !INPUT_START;
            }
        }

        self.p1 = (input & INPUT_MASK) | (self.p1 & !INPUT_MASK);
    }
}

impl mem::Mem for Joypad {
    fn loadb(&mut self, addr: u16) -> u8 {
        if addr != 0xff00 {
            panic!("invalid joypad register");
        }

        self.p1
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        if addr != 0xff00 {
            panic!("invalid joypad register");
        }

        self.p1 = (val & SELECT_MASK) | (self.p1 & !SELECT_MASK);

        self.update_input();
    }
}
