use mem;

//
// Statics
//

static DIV_CYCLE_SHIFT: u16 = 8;
static TIMA_CYCLE_SHIFT: &'static [u16] = &[
  10,  // =   4,096 Hz
  4,   // = 262,144 Hz
  6,   // =  65,536 Hz
  8,   // =  16,384 Hz
];

static TIMER_START_FLAG: u8 = 0x04;
static TIMER_INPUT_CLOCK_MASK: u8 = 0x03;


//
// Timer
//

pub struct Timer {
  div_cycles: u16, // Increments each cycle, actual register value is high byte (clock divider 256)
  tima: u8, // Actual register value
  tima_cycles_mod: u16, // Accumulated cycles below current TIMA_CYCLES
  tma: u8,
  tac: u8,
}

impl Timer {
  pub fn new() -> Timer {
    Timer { div_cycles: 0, tima: 0, tima_cycles_mod: 0, tma: 0, tac: 0 }
  }

  pub fn tick(&mut self, cycles: u8) {
    self.div_cycles += cycles as u16;
    if (self.tac & TIMER_START_FLAG) != 0 {
      self.tima_cycles_mod += cycles as u16;
      let shift = TIMA_CYCLE_SHIFT[self.tac & TIMER_INPUT_CLOCK_MASK];
      let increment = self.tima_cycles_mod >> shift;
      if increment != 0 {
        // TIMA must be incremented
        if self.tima as u16 + increment > 0xff {
          // Overflow, reset to TMA
          self.tima = self.tma;
          // TODO: Generate interrupt 50h
        } else {
          self.tima += increment as u8;
        }
        self.tima_cycles_mod -= increment << shift;
      }
    }
  }
}

impl mem::Mem for Timer {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0xff04 => (self.div_cycles >> DIV_CYCLE_SHIFT) as u8, // DIV register
      0xff05 => self.tima,                                  // TIMA register
      0xff06 => self.tma,                                   // TMA register
      0xff07 => self.tac,                                   // TAC register
      _ => fail!("invalid timer register"),
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match addr {
      0xff04 => self.div_cycles = 0, // any value resets the register to 0
      0xff05 => self.tima = val,     // TODO: is this correct?
      0xff06 => self.tma = val,
      0xff07 => { self.tac = val; self.tima_cycles_mod = 0 },
      _ => fail!("invalid timer register"),
    }
  }
}
