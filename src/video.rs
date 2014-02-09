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

static STAT_MODE0_IRQ: u8        = 0b00001000;
static STAT_MODE1_IRQ: u8        = 0b00010000;
static STAT_MODE2_IRQ: u8        = 0b00100000;
static STAT_COINCIDENCE_IRQ: u8  = 0b01000000;
static STAT_IRQ_MASK: u8         = 0b01111000;
static STAT_COINCIDENCE_FLAG: u8 = 0b00000100;

pub struct Video {
  // Implementation state
  cycles: u32, // internal cycle count, wraps around at SCREEN_REFRESH_CYCLES

  mode: u8,  // LCD mode (0-3)
  dma: u8,   // DMA request, 0xff = no request, 0x00-0xf1 = requested base address

  // Registers
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

  // Memory
  vram: [u8, ..0x1800],
  oam: [u8, ..0xa0],
}


pub enum Signal {
  VBlank,
  LCD,
  DMA(u8),
}


impl Video {
  pub fn new() -> Video {
    Video {
      cycles: 0,
      mode: 0,
      dma: 0xff,
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
      vram: [0u8, ..0x1800],
      oam: [0u8, ..0xa0],
    }
  }

  pub fn tick(&mut self, cycles: u8) -> Option<Signal> {
    let old_ly = self.ly;
    let old_mode = self.mode;

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

    self.update_stat();

    if self.dma != 0xff {
      let base = self.dma;
      self.dma = 0xff;
      return Some(DMA(base));
    }

    // TODO: Implement behavior according to http://gameboy.mongenel.com/dmg/istat98.txt

    if self.mode != old_mode {
      if self.mode == 1 {
        return Some(VBlank) // TODO: Also generate LCD interrupt if STAT_MODE1_IRQ is set
      }

      match self.mode {
        0 => if self.stat & STAT_MODE0_IRQ != 0 { return Some(LCD) },
        1 => if self.stat & STAT_MODE1_IRQ != 0 { return Some(LCD) },
        2 => if self.stat & STAT_MODE2_IRQ != 0 { return Some(LCD) },
        _ => (),
      }
    }

    if self.ly != old_ly &&
       (self.stat & STAT_COINCIDENCE_IRQ != 0) &&
       (self.stat & STAT_COINCIDENCE_FLAG != 0) {
      return Some(LCD);
    }

    None
  }

  fn update_stat(&mut self) {
    // Coincidence flag
    if self.ly == self.lyc {
      self.stat |= STAT_COINCIDENCE_FLAG;
    } else {
      self.stat &= !STAT_COINCIDENCE_FLAG;
    }
    // Mode flag
    self.stat = (self.stat & STAT_IRQ_MASK) | self.mode;
  }
}

impl mem::Mem for Video {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0x8000..0x97ff => self.vram[addr - 0x8000],
      0x9800..0x9fff => 0xff, // unused part of VRAM
      0xfe00..0xfe9f => self.oam[addr - 0xfe00], // OAM

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
      0x8000..0x97ff => self.vram[addr - 0x8000] = val,
      0x9800..0x9fff => (), // unused part of VRAM
      0xfe00..0xfe9f => self.oam[addr - 0xfe00] = val,

      // I/O registers
      0xff40 => self.flags = val,
      0xff41 => self.stat = val & STAT_IRQ_MASK, // Only interrupt enable bits are writeable
      0xff42 => self.scy = val,
      0xff43 => self.scx = val,
      0xff44 => (), // Read-only LY register
      0xff45 => self.lyc = val, // TODO: update_stat()?
      0xff46 => if val <= 0xf1 { self.dma = val },
      0xff47 => self.bgp = val,
      0xff48 => self.obp0 = val,
      0xff49 => self.obp1 = val,
      0xff4a => self.wy = val,
      0xff4b => self.wx = val,
      _ => fail!("invalid video address: ${:04X}", addr),
    }
  }
}
