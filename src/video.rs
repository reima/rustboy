use mem;

//
// Video
//

static MODE0_CYCLES: u32 = 204;   // H-Blank
static MODE1_CYCLES: u32 = 4560;  // V-Blank
static MODE2_CYCLES: u32 = 80;    // Transfer to LCD
static MODE3_CYCLES: u32 = 172;   // Transfer to LCD

static MODE0_START: u32 = MODE0_CYCLES;
static MODE2_START: u32 = MODE0_START + MODE0_CYCLES;
static MODE3_START: u32 = MODE2_START + MODE2_CYCLES;

static ROW_CYCLES: u32 = MODE0_CYCLES + MODE2_CYCLES + MODE3_CYCLES;

static ROW_COUNT: u8 = 144; // After this many rows, V-Blank starts

static SCREEN_REFRESH_CYCLES: u32 = ROW_COUNT as u32 * ROW_CYCLES + MODE1_CYCLES;

pub struct Video {
  cycles: u32, // internal cycle count, wraps around at SCREEN_REFRESH_CYCLES

  mode: u8,  // LCD mode (0-3)

  flags: u8, // LCDC register
  stat: u8,  // STAT register

  ly: u8,    // LY register
  lyc: u8,   // LYC register

  scx: u8,   // SCX register
  scy: u8,   // SCY register
  wx: u8,    // WX register
  wy: u8,    // WY register

  bgp: u8,   // BGP register
  obp0: u8,  // OBP0 register
  obp1: u8,  // OBP1 register
}

impl Video {
  pub fn new() -> Video {
    Video {
      cycles: 0,
      mode: 0,
      flags: 0x91,
      stat: 0,
      ly: 0,
      lyc: 0,
      scx: 0,
      scy: 0,
      wx: 0,
      wy: 0,
      bgp: 0,
      obp0: 0,
      obp1: 0,
    }
  }

  pub fn tick(&mut self, cycles: u8) {
    self.cycles = (self.cycles + cycles as u32) % SCREEN_REFRESH_CYCLES;
    self.ly = (self.cycles / ROW_CYCLES) as u8;
    self.mode =
      if self.ly < ROW_COUNT {
        match self.cycles % ROW_CYCLES {
          MODE0_START .. (MODE2_START - 1) => 0,
          MODE2_START .. (MODE3_START - 1) => 2,
          _                                => 3,
        }
      } else {
        1 // V-Blank
      };
    self.update_stat()
  }

  fn update_stat(&mut self) {
    // Coincidence flag
    if self.ly == self.lyc {
      self.stat |= 0b100;
    } else {
      self.stat &= !0b100;
    }
    // Mode flag
    self.stat = (self.stat & 0b11111100) | (self.mode & 0b11);
  }
}

impl mem::Mem for Video {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0x8000..0x9fff => 0x00, // VRAM
      0xfe00..0xfe9f => 0x00, // OAM

      // I/O registers
      0xff40 => self.flags,
      0xff41 => self.stat,
      0xff42 => self.scy,
      0xff43 => self.scx,
      0xff44 => self.ly,
      0xff45 => self.lyc,
      0xff46 => 0xff, // Write-only DMA register
      0xff47 => self.bgp,
      0xff48 => self.obp0,
      0xff49 => self.obp1,
      0xff4a => self.wy,
      0xff4b => self.wx,
      _ => fail!("invalid video address: ${:04X}", addr),
    }
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    match addr {
      0x8000..0x9fff => (), // VRAM
      0xfe00..0xfe9f => (), // OAM

      // I/O registers
      0xff40 => self.flags = val,
      0xff41 => self.stat = val, // TODO: Mask to bits 3-6?
      0xff42 => self.scy = val,
      0xff43 => self.scx = val,
      0xff44 => (), // Read-only LY register
      0xff45 => self.lyc = val, // TODO: update_stat()?
      0xff46 => fail!("DMA transfer not implemented"),
      0xff47 => self.bgp = val,
      0xff48 => self.obp0 = val,
      0xff49 => self.obp1 = val,
      0xff4a => self.wy = val,
      0xff4b => self.wx = val,
      _ => fail!("invalid video address: ${:04X}", addr),
    }
  }
}
