use mem;

//
// Video
//

static MODE0_CYCLES: uint = 204;   // H-Blank
static MODE1_CYCLES: uint = 4560;  // V-Blank
static MODE2_CYCLES: uint = 80;    // Transfer to LCD
static MODE3_CYCLES: uint = 172;   // Transfer to LCD

static MODE2_START: uint = 0;
static MODE3_START: uint = MODE2_START + MODE2_CYCLES;
static MODE0_START: uint = MODE3_START + MODE3_CYCLES;

static ROW_CYCLES: uint = MODE0_CYCLES + MODE2_CYCLES + MODE3_CYCLES;

pub static SCREEN_REFRESH_CYCLES: uint = SCREEN_HEIGHT * ROW_CYCLES + MODE1_CYCLES;

static STAT_MODE0_IRQ: u8        = 0b0000_1000;
static STAT_MODE1_IRQ: u8        = 0b0001_0000;
static STAT_MODE2_IRQ: u8        = 0b0010_0000;
static STAT_COINCIDENCE_IRQ: u8  = 0b0100_0000;
static STAT_IRQ_MASK: u8         = 0b0111_1000;
static STAT_COINCIDENCE_FLAG: u8 = 0b0000_0100;
static STAT_MODE_MASK: u8        = 0b0000_0011;

static FLAG_ENABLE_BG_WIN: u8 = 0b0000_0001;
static FLAG_ENABLE_OBJ: u8    = 0b0000_0010;
static FLAG_OBJ_SIZE: u8      = 0b0000_0100;
static FLAG_BG_MAP: u8        = 0b0000_1000;
static FLAG_BG_WIN_TILES: u8  = 0b0001_0000;
static FLAG_ENABLE_WIN: u8    = 0b0010_0000;
static FLAG_WIN_MAP: u8       = 0b0100_0000;
static FLAG_ENABLE: u8        = 0b1000_0000;

static TILES_BASE0: uint = 0x800;
static TILES_BIAS0: u8 = 128u8;
static TILES_BASE1: uint = 0x000;
static TILES_BIAS1: u8 = 0u8;

static BG_WIN_MAP_BASE0: uint = 0x1800;
static BG_WIN_MAP_BASE1: uint = 0x1c00;

static TILE_WIDTH: uint  = 8;
static TILE_HEIGHT: uint = 8;
static TILE_BYTES: uint  = 16;

static BG_WIDTH_TILES:  uint = 32;
static BG_HEIGHT_TILES: uint = 32;

static OBJ_FLAG_PALETTE:  u8 = 0b0001_0000;
static OBJ_FLAG_FLIP_X:   u8 = 0b0010_0000;
static OBJ_FLAG_FLIP_Y:   u8 = 0b0100_0000;
static OBJ_FLAG_PRIORITY: u8 = 0b1000_0000;

pub static SCREEN_WIDTH: uint = 160;
pub static SCREEN_HEIGHT: uint = 144;  // After this many rows, V-Blank starts

pub struct Video {
  // Implementation state
  cycles: uint, // internal cycle count, wraps around at SCREEN_REFRESH_CYCLES

  mode: u8,  // LCD mode (0-3), cycles through [2, 3, 0] for each row
  dma: u8,   // DMA request, 0xff = no request, 0x00-0xf1 = requested base address
  wy_saved: u8, // WY register at the start of the frame (changes during frame have no effect)

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
  vram: [u8, ..0x2000],
  oam: [u8, ..0xa0],

  // Screen buffer
  screen: [u8, ..SCREEN_WIDTH*SCREEN_HEIGHT*4],
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
      wy_saved: 0,
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
      vram: [0u8, ..0x2000],
      oam: [0u8, ..0xa0],
      screen: [0u8, ..SCREEN_WIDTH*SCREEN_HEIGHT*4], // BGRA
    }
  }

  pub fn tick(&mut self, cycles: u8) -> Option<Signal> {
    let old_ly = self.ly;
    let old_mode = self.mode;

    self.cycles = (self.cycles + cycles as uint) % SCREEN_REFRESH_CYCLES;
    self.ly = (self.cycles / ROW_CYCLES) as u8;

    self.mode =
      if self.ly < SCREEN_HEIGHT as u8 {
        match self.cycles % ROW_CYCLES {
          MODE2_START .. (MODE3_START - 1) => 2,
          MODE3_START .. (MODE0_START - 1) => 3,
          _                                => 0,
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

    if old_mode == 3 && self.mode == 0 {
      // H-Blank
      self.draw_row(self.ly as uint);
    }

    if old_mode == 1 && self.mode == 2 {
      // Beginning of new frame
      self.wy_saved = self.wy;
    }

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
    self.stat = (self.stat & !STAT_MODE_MASK) | self.mode;
  }

  fn draw_row(&mut self, row: uint) {
    if (self.flags & FLAG_ENABLE_BG_WIN) != 0 {
      self.draw_bg_win_row(row);
    }

    if (self.flags & FLAG_ENABLE_OBJ) != 0 {
      self.draw_obj_row(row);
    }
  }

  fn draw_bg_win_row(&mut self, screen_y: uint) {
    static WIN_OFFSET_X: uint = 7;

    let mut tiles_base = TILES_BASE0;
    let mut tiles_bias = TILES_BIAS0;
    if (self.flags & FLAG_BG_WIN_TILES) != 0 {
      tiles_base = TILES_BASE1;
      tiles_bias = TILES_BIAS1;
    }

    let bg_map_base =
      if (self.flags & FLAG_BG_MAP) == 0 {
        BG_WIN_MAP_BASE0
      } else {
        BG_WIN_MAP_BASE1
      };

    let win_map_base =
      if (self.flags & FLAG_WIN_MAP) == 0 {
        BG_WIN_MAP_BASE0
      } else {
        BG_WIN_MAP_BASE1
      };

    let draw_win = (self.flags & FLAG_ENABLE_WIN) != 0 && self.wy_saved as uint <= screen_y;

    let bg_map_y = (screen_y + self.scy as uint) % (BG_HEIGHT_TILES * TILE_HEIGHT);
    let win_map_y = screen_y - self.wy_saved as uint;

    for screen_x in range(0u, SCREEN_WIDTH) {
      let mut map_base;
      let mut map_x;
      let mut map_y;

      if draw_win && screen_x + WIN_OFFSET_X >= self.wx as uint {
        map_base = win_map_base;
        map_x = (screen_x + WIN_OFFSET_X - self.wx as uint);
        map_y = win_map_y;
      } else {
        map_base = bg_map_base;
        map_x = (screen_x + self.scx as uint) % (BG_WIDTH_TILES * TILE_WIDTH);
        map_y = bg_map_y;
      }

      let pixel = self.screen.mut_slice_from((screen_y * SCREEN_WIDTH + screen_x) * 4);
      let map_tile_x = map_x / TILE_WIDTH;
      let map_tile_y = map_y / TILE_HEIGHT;
      let tile_num = (self.vram[map_base + map_tile_y*BG_WIDTH_TILES + map_tile_x] + tiles_bias) as uint;
      let tile = self.vram.slice_from(tiles_base + tile_num*TILE_BYTES);

      let tile_x = map_x % TILE_WIDTH;
      let tile_y = map_y % TILE_HEIGHT;

      unpack_tile_pixel(tile, self.bgp, tile_x, tile_y, pixel, false);
    }
  }

  fn draw_obj_row(&mut self, screen_y: uint) {
    // Obj with coordinates (OFFSET_X, OFFSET_Y) is at (0, 0) on screen
    static OFFSET_X: uint = 8;
    static OFFSET_Y: uint = 16;

    static MAX_OBJS_PER_ROW: uint = 10;

    let obj_height =
      if (self.flags & FLAG_OBJ_SIZE) != 0 {
        2 * TILE_HEIGHT
      } else {
        TILE_HEIGHT
      };

    // Find objs in this row
    let mut objs = ~[];
    for obj_num in range(0, 40) {
      let obj_y = self.oam[obj_num * 4] as uint;
      if obj_y <= screen_y + OFFSET_Y && screen_y + OFFSET_Y < obj_y + obj_height {
        objs.push(obj_num);
      }
    }

    // Early out when no objs in this row
    if objs.len() == 0 {
      return;
    }

    // TODO: Restrict to 10 objs by priority

    // Sort by X coordinate, high to low
    objs.sort_by(|obj0, obj1|
      self.oam[(*obj1)*4 + 1].cmp(&self.oam[(*obj0)*4 + 1])
    );

    // Draw objs
    for obj in objs.iter() {
      let obj_y        = self.oam[(*obj) * 4] as uint;
      let obj_x        = self.oam[(*obj) * 4 + 1] as uint;
      let mut obj_tile = self.oam[(*obj) * 4 + 2] as uint;
      let obj_flags    = self.oam[(*obj) * 4 + 3];
      let mut tile_y = screen_y - obj_y + OFFSET_Y;
      let pal = if (obj_flags & OBJ_FLAG_PALETTE) == 0 { self.obp0 } else { self.obp1 };

      if (self.flags & FLAG_OBJ_SIZE) != 0 {
        // 8x16 objs
        let flip_y = (obj_flags & OBJ_FLAG_FLIP_Y) != 0;
        if (tile_y < TILE_HEIGHT && !flip_y) ||
           (tile_y >= TILE_HEIGHT && flip_y) {
          obj_tile &= !1; // upper subtile
        } else {
          obj_tile |= 1;  // lower subtile
        }
        tile_y %= TILE_HEIGHT;
      }

      if (obj_flags & OBJ_FLAG_FLIP_Y) != 0 {
        tile_y = TILE_HEIGHT - 1 - tile_y;
      }

      let tile = self.vram.slice_from(TILES_BASE1 + obj_tile*TILE_BYTES);

      for tile_x in range(0, TILE_WIDTH) {
        let screen_x = obj_x + tile_x;
        if OFFSET_X <= screen_x && screen_x < SCREEN_WIDTH + OFFSET_X {
          let pixel = self.screen.mut_slice_from((screen_y * SCREEN_WIDTH + screen_x - OFFSET_X) * 4);
          if (obj_flags & OBJ_FLAG_FLIP_X) != 0 {
            tile_x = TILE_WIDTH - 1 - tile_x;
          }
          unpack_tile_pixel(tile, pal, tile_x, tile_y, pixel, true);
        }
      }
    }
  }
}

impl mem::Mem for Video {
  fn loadb(&mut self, addr: u16) -> u8 {
    match addr {
      0x8000..0x9fff => self.vram[addr - 0x8000],
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
      0x8000..0x9fff => self.vram[addr - 0x8000] = val,
      0xfe00..0xfe9f => self.oam[addr - 0xfe00] = val,

      // I/O registers
      0xff40 => self.flags = val,
      0xff41 => self.stat = (val & STAT_IRQ_MASK) | (self.stat & !STAT_IRQ_MASK), // Only interrupt enable bits are writeable
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

fn unpack_tile_pixel(tile: &[u8],
                     palette: u8,
                     x: uint,
                     y: uint,
                     pixel: &mut [u8],
                     transp: bool) {
  static colors: &'static [& 'static[u8]] = &[
    &[224, 248, 208],
    &[136, 192, 112],
    &[52, 104, 86],
    &[8, 24, 32]
  ];

  let low_bit  = (tile[2*y]   >> (7 - x)) & 1;
  let high_bit = (tile[2*y+1] >> (7 - x)) & 1;
  let value = (high_bit << 1) | low_bit;
  let color = (palette >> (2 * value)) & 0b11;

  if transp && color == 0 {
    return;
  }

  pixel[0] = colors[color][2];
  pixel[1] = colors[color][1];
  pixel[2] = colors[color][0];
}
