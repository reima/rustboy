extern mod sdl2;

use mem::Mem;
use std::io::stdio;

mod cartridge;
mod cpu;
mod debug;
mod disasm;
mod interrupt;
mod joypad;
mod mem;
mod ram;
mod serial;
mod sound;
mod timer;
mod video;

//
// Memory Map
//

struct Dummy;
impl Mem for Dummy {
  fn loadb(&mut self, addr: u16) -> u8 {
    //debug!("load in unmapped memory at 0x{:04X}", addr);
    0xff
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    //debug!("store in unmapped memory at 0x{:04X}", addr);
  }
}

struct MemMap {
  cart: ~cartridge::Cartridge,
  wram: ram::WorkRam,
  timer: timer::Timer,
  intr: interrupt::InterruptCtrl,
  sound: sound::Sound,
  video: video::Video,
  serial: serial::SerialIO,
  joypad: joypad::Joypad,
  dummy: Dummy,
}

impl MemMap {
  fn mem_from_addr<'a>(&'a mut self, addr: u16) -> &'a mut Mem {
    match addr {
      0x0000..0x7fff | // ROM banks
      0xa000..0xbfff   // External RAM
                      => &mut *self.cart as &mut Mem,
      0x8000..0x9fff | // VRAM
      0xfe00..0xfe9f | // OAM
      0xff40..0xff4b   // Video I/O
                      => &mut self.video as &mut Mem,
      0xc000..0xfdff | // WRAM (including echo area 0xe000-0xfdff)
      0xff80..0xfffe   // HRAM
                      => &mut self.wram as &mut Mem,
      0xff00          => &mut self.joypad as &mut Mem,
      0xff01..0xff02  => &mut self.serial as &mut Mem,
      0xff04..0xff07  => &mut self.timer as &mut Mem,
      0xff0f | 0xffff => &mut self.intr as &mut Mem,
      0xff10..0xff3f  => &mut self.sound as &mut Mem,
      _ => &mut self.dummy as &mut Mem,
      //_ => fail!("unmapped memory at 0x{:04X}", addr),
    }
  }
}

impl Mem for MemMap {
  fn loadb(&mut self, addr: u16) -> u8 {
    self.mem_from_addr(addr).loadb(addr)
  }

  fn storeb(&mut self, addr: u16, val: u8) {
    self.mem_from_addr(addr).storeb(addr, val)
  }
}


//
// Video Output
//

struct VideoOut {
  renderer: ~sdl2::render::Renderer,
  texture: ~sdl2::render::Texture,
}

impl VideoOut {
  fn new(scale: int) -> VideoOut {
    use sdl2::render::Renderer;

    sdl2::init([sdl2::InitVideo]);

    let window_width = video::SCREEN_WIDTH as int * scale;
    let window_height = video::SCREEN_HEIGHT as int * scale;

    let renderer = match Renderer::new_with_window(window_width,
                                                   window_height,
                                                   [sdl2::video::Resizable]) {
      Ok(renderer) => renderer,
      Err(err) => fail!("Failed to create renderer: {}", err)
    };

    let texture = match renderer.create_texture(sdl2::pixels::ARGB8888,
                                                sdl2::render::AccessStreaming,
                                                video::SCREEN_WIDTH as int,
                                                video::SCREEN_HEIGHT as int) {
      Ok(texture) => texture,
      Err(err) => fail!("Failed to create texture: {}", err),
    };

    VideoOut { renderer: renderer, texture: texture }
  }

  fn blit_and_present(&self, pixels: &[u8]) {
    self.texture.update(None, pixels, (video::SCREEN_WIDTH * 4) as int);
    self.renderer.copy(self.texture, None, None);
    self.renderer.present();
  }

  fn set_title(&self, title: &str) {
    match self.renderer.parent {
      sdl2::render::Window(ref w) => w.set_title(title),
      _ => (),
    }
  }
}


fn keymap(code: sdl2::keycode::KeyCode) -> Option<joypad::Button> {
  match code {
    sdl2::keycode::UpKey     => Some(joypad::Up),
    sdl2::keycode::DownKey   => Some(joypad::Down),
    sdl2::keycode::LeftKey   => Some(joypad::Left),
    sdl2::keycode::RightKey  => Some(joypad::Right),
    sdl2::keycode::ReturnKey => Some(joypad::Start),
    sdl2::keycode::RShiftKey => Some(joypad::Select),
    sdl2::keycode::CKey      => Some(joypad::ButtonA),
    sdl2::keycode::XKey      => Some(joypad::ButtonB),
    _ => None,
  }
}


fn main() {
  let args = std::os::args();
  if args.len() != 2 && !(args.len() == 3 && args[1] == ~"-d") {
    println!("Usage: {:s} [-d] rom.gb", args[0]);
    return;
  }

  let mut disassemble = false;
  let path =
    if args.len() == 2 {
      args[1]
    } else {
      disassemble = true;
      args[2]
    };

  let mut cart = match cartridge::Cartridge::from_path(&Path::new(path)) {
    Ok(cart) => ~cart,
    Err(e)   => fail!("I/O error: {:s}", e.to_str()),
  };

  if disassemble {
    // Disassemble only
    let mut d = disasm::Disasm { mem: &mut *cart, pc: 0 };
    while d.pc <= 0x7fff {
      let pc = d.pc;
      println!("${:04X}\t{:s}", pc, cpu::decode(&mut d));
    }
    return;
  }

  println!("Name: {:s}", cart.title);
  println!("Type: {:u}", cart.cartridge_type);

  let memmap = MemMap {
    cart: cart,
    wram: ram::WorkRam::new(),
    timer: timer::Timer::new(),
    intr: interrupt::InterruptCtrl::new(),
    sound: sound::Sound,
    video: video::Video::new(),
    serial: serial::SerialIO::new(Some(~stdio::stdout() as ~std::io::Writer)),
    joypad: joypad::Joypad::new(),
    dummy: Dummy,
  };
  let mut cpu = cpu::Cpu::new(memmap);
  cpu.regs.pc = 0x100;

  let video_out = VideoOut::new(4);
  video_out.set_title("Rustboy");

  let mut done = false;
  let mut running = false;
  let mut debugger = debug::Debugger::new();

  let counts_per_sec = sdl2::timer::get_performance_frequency();
  let counts_per_frame = counts_per_sec * video::SCREEN_REFRESH_CYCLES as u64 / cpu::CYCLES_PER_SEC as u64;
  let mut last_frame_start_count = sdl2::timer::get_performance_counter();

  let mut last_fps_update = last_frame_start_count;
  let mut frames = 0;

  println!("c/s: {:u}; c/f: {:u}", counts_per_sec, counts_per_frame);

  while !done {
    if running {
      if debugger.should_break(&cpu) {
        running = false;
      }
    }

    if !running {
      match debugger.prompt(&mut cpu) {
        debug::Quit => break,
        debug::Run  => running = true,
        debug::Step => (),
      }
    }

    let cycles = cpu.step();

    match cpu.mem.timer.tick(cycles) {
      Some(timer::TIMAOverflow) => cpu.mem.intr.irq(interrupt::IRQ_TIMER),
      None => (),
    }

    let mut new_frame = false;
    match cpu.mem.video.tick(cycles) {
      Some(video::DMA(base)) => {
        // Do DMA transfer instantaneously
        let base_addr = base as u16 << 8;
        for offset in range(0x00u16, 0xa0u16) {
          let val = cpu.mem.loadb(base_addr + offset);
          cpu.mem.storeb(0xfe00 + offset, val);
        }
      },
      Some(video::VBlank) => {
        video_out.blit_and_present(cpu.mem.video.screen);
        cpu.mem.intr.irq(interrupt::IRQ_VBLANK);
        new_frame = true;
      }
      Some(video::LCD)    => cpu.mem.intr.irq(interrupt::IRQ_LCD),
      None => (),
    }

    // Synchronize speed based on frame time
    if new_frame {
      let now = sdl2::timer::get_performance_counter();

      frames += 1;
      if now - last_fps_update > counts_per_sec {
        let fps = frames * (now - last_fps_update) / counts_per_sec;
        video_out.set_title(format!("Rustboy - {} fps", fps));
        last_fps_update = now;
        frames = 0;
      }

      let frame_time = now - last_frame_start_count;
      if frame_time < counts_per_frame {
        let delay_msec = (1_000 * (counts_per_frame - frame_time) / counts_per_sec) as uint;
        sdl2::timer::delay(delay_msec);
      }
      // TODO: What should we do when we take longer than counts_per_frame?
      last_frame_start_count = sdl2::timer::get_performance_counter();
    }

    loop {
      match sdl2::event::poll_event() {
        sdl2::event::QuitEvent(_) => { done = true; break }
        sdl2::event::KeyDownEvent(_, _, key, _, _) => {
          match keymap(key) {
            Some(button) => cpu.mem.joypad.set_button(button, true),
            None => {
              match key {
                sdl2::keycode::EscapeKey => { running = false },
                _ => (),
              }
            }
          }
        },
        sdl2::event::KeyUpEvent(_, _, key, _, _) => {
          match keymap(key) {
            Some(button) => cpu.mem.joypad.set_button(button, false),
            None => (),
          }
        }
        sdl2::event::NoEvent => break,
        _ => (),
      }
    }
  }
}
