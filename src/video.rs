use crate::mem;

//
// Video
//

const MODE0_CYCLES: usize = 204; // H-Blank
const MODE1_CYCLES: usize = 4560; // V-Blank
const MODE2_CYCLES: usize = 80; // Transfer to LCD
const MODE3_CYCLES: usize = 172; // Transfer to LCD

const MODE2_START: usize = 0;
const MODE2_END: usize = MODE2_START + MODE2_CYCLES - 1;
const MODE3_START: usize = MODE2_END + 1;
const MODE3_END: usize = MODE3_START + MODE3_CYCLES - 1;
//const MODE0_START: usize = MODE3_END + 1;

const ROW_CYCLES: usize = MODE0_CYCLES + MODE2_CYCLES + MODE3_CYCLES;

pub const SCREEN_REFRESH_CYCLES: usize = SCREEN_HEIGHT * ROW_CYCLES + MODE1_CYCLES;

const STAT_MODE0_IRQ: u8 = 0b0000_1000;
const STAT_MODE1_IRQ: u8 = 0b0001_0000;
const STAT_MODE2_IRQ: u8 = 0b0010_0000;
const STAT_COINCIDENCE_IRQ: u8 = 0b0100_0000;
const STAT_IRQ_MASK: u8 = 0b0111_1000;
const STAT_COINCIDENCE_FLAG: u8 = 0b0000_0100;
const STAT_MODE_MASK: u8 = 0b0000_0011;

const FLAG_ENABLE_BG_WIN: u8 = 0b0000_0001;
const FLAG_ENABLE_OBJ: u8 = 0b0000_0010;
const FLAG_OBJ_SIZE: u8 = 0b0000_0100;
const FLAG_BG_MAP: u8 = 0b0000_1000;
const FLAG_BG_WIN_TILES: u8 = 0b0001_0000;
const FLAG_ENABLE_WIN: u8 = 0b0010_0000;
const FLAG_WIN_MAP: u8 = 0b0100_0000;
//const FLAG_ENABLE: u8        = 0b1000_0000;

const TILES_BASE0: usize = 0x800;
const TILES_BIAS0: u8 = 128u8;
const TILES_BASE1: usize = 0x000;
const TILES_BIAS1: u8 = 0u8;

const BG_WIN_MAP_BASE0: usize = 0x1800;
const BG_WIN_MAP_BASE1: usize = 0x1c00;

const TILE_WIDTH: usize = 8;
const TILE_HEIGHT: usize = 8;
const TILE_BYTES: usize = 16;

const BG_WIDTH_TILES: usize = 32;
const BG_HEIGHT_TILES: usize = 32;

const OBJ_FLAG_PALETTE: u8 = 0b0001_0000;
const OBJ_FLAG_FLIP_X: u8 = 0b0010_0000;
const OBJ_FLAG_FLIP_Y: u8 = 0b0100_0000;
//const OBJ_FLAG_PRIORITY: u8 = 0b1000_0000;

pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144; // After this many rows, V-Blank starts

pub struct Video {
    // Implementation state
    cycles: usize, // internal cycle count, wraps around at SCREEN_REFRESH_CYCLES

    mode: u8,     // LCD mode (0-3), cycles through [2, 3, 0] for each row
    dma: u8,      // DMA request, 0xff = no request, 0x00-0xf1 = requested base address
    wy_saved: u8, // WY register at the start of the frame (changes during frame have no effect)

    // Registers
    flags: u8, // LCDC register
    stat: u8,  // STAT register

    ly: u8,  // LY register
    lyc: u8, // LYC register

    scx: u8, // SCX register
    scy: u8, // SCY register
    wx: u8,  // WX register
    wy: u8,  // WY register

    bgp: u8,  // BGP register
    obp0: u8, // OBP0 register
    obp1: u8, // OBP1 register

    // Memory
    vram: [u8; 0x2000],
    oam: [u8; 0xa0],

    // Screen buffer
    pub screen: [u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4],
}

pub enum Signal {
    VBlank,
    Lcd,
    Dma(u8),
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
            vram: [0u8; 0x2000],
            oam: [0u8; 0xa0],
            screen: [0u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4], // BGRA
        }
    }

    pub fn tick(&mut self, cycles: u8) -> Vec<Signal> {
        let mut signals = vec![];

        let old_ly = self.ly;
        let old_mode = self.mode;

        self.cycles = self.cycles.wrapping_add(cycles as usize) % SCREEN_REFRESH_CYCLES;
        self.ly = (self.cycles / ROW_CYCLES) as u8;

        self.mode = if self.ly < SCREEN_HEIGHT as u8 {
            match self.cycles % ROW_CYCLES {
                MODE2_START..=MODE2_END => 2,
                MODE3_START..=MODE3_END => 3,
                _ => 0,
            }
        } else {
            1 // V-Blank
        };

        self.update_stat();

        if self.dma != 0xff {
            signals.push(Signal::Dma(self.dma));
            self.dma = 0xff;
        }

        // TODO: Implement behavior according to http://gameboy.mongenel.com/dmg/istat98.txt

        if old_mode == 3 && self.mode == 0 {
            // H-Blank
            let row_y = self.ly as usize;
            self.draw_row(row_y);
        }

        if old_mode == 1 && self.mode == 2 {
            // Beginning of new frame
            self.wy_saved = self.wy;
        }

        let mut lcd_intr = false;

        if self.mode != old_mode {
            if self.mode == 1 {
                signals.push(Signal::VBlank);
            }

            match self.mode {
                0 => {
                    if self.stat & STAT_MODE0_IRQ != 0 {
                        lcd_intr = true
                    }
                }
                1 => {
                    if self.stat & STAT_MODE1_IRQ != 0 {
                        lcd_intr = true
                    }
                }
                2 => {
                    if self.stat & STAT_MODE2_IRQ != 0 {
                        lcd_intr = true
                    }
                }
                _ => (),
            }
        }

        if self.ly != old_ly
            && (self.stat & STAT_COINCIDENCE_IRQ != 0)
            && (self.stat & STAT_COINCIDENCE_FLAG != 0)
        {
            lcd_intr = true;
        }

        if lcd_intr {
            signals.push(Signal::Lcd);
        }

        signals
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

    fn draw_row(&mut self, row: usize) {
        if (self.flags & FLAG_ENABLE_BG_WIN) != 0 {
            self.draw_bg_win_row(row);
        }

        if (self.flags & FLAG_ENABLE_OBJ) != 0 {
            self.draw_obj_row(row);
        }
    }

    fn draw_bg_win_row(&mut self, screen_y: usize) {
        static WIN_OFFSET_X: usize = 7;

        let (tiles_base, tiles_bias) = if (self.flags & FLAG_BG_WIN_TILES) == 0 {
            (TILES_BASE0, TILES_BIAS0)
        } else {
            (TILES_BASE1, TILES_BIAS1)
        };

        let bg_map_base = if (self.flags & FLAG_BG_MAP) == 0 {
            BG_WIN_MAP_BASE0
        } else {
            BG_WIN_MAP_BASE1
        };

        let win_map_base = if (self.flags & FLAG_WIN_MAP) == 0 {
            BG_WIN_MAP_BASE0
        } else {
            BG_WIN_MAP_BASE1
        };

        let draw_win = (self.flags & FLAG_ENABLE_WIN) != 0 && self.wy_saved as usize <= screen_y;

        let bg_map_y = screen_y.wrapping_add(self.scy as usize) % (BG_HEIGHT_TILES * TILE_HEIGHT);
        let win_map_y = screen_y.wrapping_sub(self.wy_saved as usize);

        for screen_x in 0usize..SCREEN_WIDTH {
            let (map_base, map_x, map_y) =
                if draw_win && screen_x + WIN_OFFSET_X >= self.wx as usize {
                    (
                        win_map_base,
                        screen_x + WIN_OFFSET_X - self.wx as usize,
                        win_map_y,
                    )
                } else {
                    (
                        bg_map_base,
                        (screen_x + self.scx as usize) % (BG_WIDTH_TILES * TILE_WIDTH),
                        bg_map_y,
                    )
                };

            let pixel = &mut self.screen[((screen_y * SCREEN_WIDTH + screen_x) * 4)..];
            let map_tile_x = map_x / TILE_WIDTH;
            let map_tile_y = map_y / TILE_HEIGHT;
            let tile_num = self.vram[map_base
                .wrapping_add(map_tile_y * BG_WIDTH_TILES)
                .wrapping_add(map_tile_x)]
            .wrapping_add(tiles_bias) as usize;
            let tile = &self.vram[(tiles_base + tile_num * TILE_BYTES)..];

            let tile_x = map_x % TILE_WIDTH;
            let tile_y = map_y % TILE_HEIGHT;

            unpack_tile_pixel(tile, self.bgp, tile_x, tile_y, pixel, false);
        }
    }

    fn draw_obj_row(&mut self, screen_y: usize) {
        // Obj with coordinates (OFFSET_X, OFFSET_Y) is at (0, 0) on screen
        static OFFSET_X: usize = 8;
        static OFFSET_Y: usize = 16;

        //static MAX_OBJS_PER_ROW: usize = 10;

        let obj_height = if (self.flags & FLAG_OBJ_SIZE) != 0 {
            2 * TILE_HEIGHT
        } else {
            TILE_HEIGHT
        };

        // Find objs in this row
        let mut objs = vec![];
        for obj_num in 0usize..40usize {
            let obj_y = self.oam[obj_num * 4] as usize;
            if obj_y <= screen_y + OFFSET_Y && screen_y + OFFSET_Y < obj_y + obj_height {
                objs.push(obj_num);
            }
        }

        // Early out when no objs in this row
        if objs.is_empty() {
            return;
        }

        // TODO: Restrict to 10 objs by priority

        // Sort by X coordinate, high to low
        objs.sort_by(|obj0, obj1| self.oam[(*obj1) * 4 + 1].cmp(&self.oam[(*obj0) * 4 + 1]));

        // Draw objs
        for obj in &objs {
            let obj_y = self.oam[(*obj) * 4] as usize;
            let obj_x = self.oam[(*obj) * 4 + 1] as usize;
            let mut obj_tile = self.oam[(*obj) * 4 + 2] as usize;
            let obj_flags = self.oam[(*obj) * 4 + 3];
            let mut tile_y = screen_y.wrapping_sub(obj_y).wrapping_add(OFFSET_Y);
            let pal = if (obj_flags & OBJ_FLAG_PALETTE) == 0 {
                self.obp0
            } else {
                self.obp1
            };

            if (self.flags & FLAG_OBJ_SIZE) != 0 {
                // 8x16 objs
                let flip_y = (obj_flags & OBJ_FLAG_FLIP_Y) != 0;
                if (tile_y < TILE_HEIGHT && !flip_y) || (tile_y >= TILE_HEIGHT && flip_y) {
                    obj_tile &= !1; // upper subtile
                } else {
                    obj_tile |= 1; // lower subtile
                }
                tile_y %= TILE_HEIGHT;
            }

            if (obj_flags & OBJ_FLAG_FLIP_Y) != 0 {
                tile_y = TILE_HEIGHT - 1 - tile_y;
            }

            let tile = &self.vram[(TILES_BASE1 + obj_tile * TILE_BYTES)..];

            for mut tile_x in 0..TILE_WIDTH {
                let screen_x = obj_x + tile_x;
                if OFFSET_X <= screen_x && screen_x < SCREEN_WIDTH + OFFSET_X {
                    let pixel =
                        &mut self.screen[((screen_y * SCREEN_WIDTH + screen_x - OFFSET_X) * 4)..];
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
    fn loadb(&self, addr: u16) -> u8 {
        match addr {
            0x8000..=0x9fff => self.vram[(addr - 0x8000) as usize],
            0xfe00..=0xfe9f => self.oam[(addr - 0xfe00) as usize], // OAM

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
            _ => panic!("invalid video address: ${:04X}", addr),
        }
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        match addr {
            0x8000..=0x9fff => self.vram[(addr - 0x8000) as usize] = val,
            0xfe00..=0xfe9f => self.oam[(addr - 0xfe00) as usize] = val,

            // I/O registers
            0xff40 => self.flags = val,
            0xff41 => self.stat = (val & STAT_IRQ_MASK) | (self.stat & !STAT_IRQ_MASK), // Only interrupt enable bits are writeable
            0xff42 => self.scy = val,
            0xff43 => self.scx = val,
            0xff44 => (),             // Read-only LY register
            0xff45 => self.lyc = val, // TODO: update_stat()?
            0xff46 => {
                if val <= 0xf1 {
                    self.dma = val
                }
            }
            0xff47 => self.bgp = val,
            0xff48 => self.obp0 = val,
            0xff49 => self.obp1 = val,
            0xff4a => self.wy = val,
            0xff4b => self.wx = val,
            _ => panic!("invalid video address: ${:04X}", addr),
        }
    }
}

fn unpack_tile_pixel(tile: &[u8], palette: u8, x: usize, y: usize, pixel: &mut [u8], transp: bool) {
    static COLORS: &[&[u8]] = &[
        &[224, 248, 208],
        &[136, 192, 112],
        &[52, 104, 86],
        &[8, 24, 32],
    ];

    let low_bit = (tile[2 * y] >> (7 - x)) & 1;
    let high_bit = (tile[2 * y + 1] >> (7 - x)) & 1;
    let value = (high_bit << 1) | low_bit;
    let color = (palette >> (2 * value)) & 0b11;

    if transp && color == 0 {
        return;
    }

    pixel[0] = COLORS[color as usize][2];
    pixel[1] = COLORS[color as usize][1];
    pixel[2] = COLORS[color as usize][0];
}
