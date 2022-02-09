use crate::mem;

//
// Statics
//

const DIV_CYCLE_SHIFT: usize = 8;
const TIMA_CYCLE_SHIFT: &[u16] = &[
    10, // =   4,096 Hz
    4,  // = 262,144 Hz
    6,  // =  65,536 Hz
    8,  // =  16,384 Hz
];

const TIMER_START_FLAG: u8 = 0x04;
const TIMER_INPUT_CLOCK_MASK: u8 = 0x03;

//
// Timer
//

pub struct Timer {
    div_cycles: u16, // Increments each cycle, actual register value is high byte (clock divider 256)
    tima: u8,        // Actual register value
    tima_cycles_mod: u16, // Accumulated cycles below current TIMA_CYCLES
    tma: u8,
    tac: u8,
}

pub enum Signal {
    TIMAOverflow,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            div_cycles: 0,
            tima: 0,
            tima_cycles_mod: 0,
            tma: 0,
            tac: 0,
        }
    }

    pub fn tick(&mut self, cycles: u8) -> Option<Signal> {
        let mut result = None;
        self.div_cycles = self.div_cycles.wrapping_add(u16::from(cycles));
        if (self.tac & TIMER_START_FLAG) != 0 {
            self.tima_cycles_mod += u16::from(cycles);
            let shift = TIMA_CYCLE_SHIFT[(self.tac & TIMER_INPUT_CLOCK_MASK) as usize];
            let increment = self.tima_cycles_mod >> shift;
            if increment != 0 {
                // TIMA must be incremented
                if u16::from(self.tima) + increment > 0xff {
                    // Overflow, reset to TMA
                    self.tima = self.tma;
                    result = Some(Signal::TIMAOverflow);
                } else {
                    self.tima += increment as u8;
                }
                self.tima_cycles_mod -= increment << shift;
            }
        }
        result
    }
}

impl mem::Mem for Timer {
    fn loadb(&self, addr: u16) -> u8 {
        match addr {
            0xff04 => (self.div_cycles >> DIV_CYCLE_SHIFT) as u8, // DIV register
            0xff05 => self.tima,                                  // TIMA register
            0xff06 => self.tma,                                   // TMA register
            0xff07 => self.tac,                                   // TAC register
            _ => panic!("invalid timer register"),
        }
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        match addr {
            0xff04 => self.div_cycles = 0, // any value resets the register to 0
            0xff05 => self.tima = val,     // TODO: is this correct?
            0xff06 => self.tma = val,
            0xff07 => {
                self.tac = val;
                self.tima_cycles_mod = 0
            }
            _ => panic!("invalid timer register"),
        }
    }
}
