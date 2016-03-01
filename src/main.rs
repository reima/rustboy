#[macro_use]
extern crate log;

extern crate sdl2;

use mem::Mem;
use std::io::stdout;
use std::path::Path;

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
  fn loadb(&mut self, _ /* addr */: u16) -> u8 {
    //debug!("load in unmapped memory at 0x{:04X}", addr);
    0xff
  }

  fn storeb(&mut self, _ /* addr */: u16, _ /* val */: u8) {
    //debug!("store in unmapped memory at 0x{:04X}", addr);
  }
}

struct MemMap<'a> {
  cart: Box<cartridge::Cartridge>,
  wram: ram::WorkRam,
  timer: timer::Timer,
  intr: interrupt::InterruptCtrl,
  sound: sound::Sound,
  video: video::Video,
  serial: serial::SerialIO<'a>,
  joypad: joypad::Joypad,
  dummy: Dummy,
}

impl<'a> MemMap<'a> {
  fn mem_from_addr(&mut self, addr: u16) -> &mut Mem {
    match addr {
      0x0000...0x7fff | // ROM banks
      0xa000...0xbfff   // External RAM
                       => &mut *self.cart as &mut Mem,
      0x8000...0x9fff | // VRAM
      0xfe00...0xfe9f | // OAM
      0xff40...0xff4b   // Video I/O
                       => &mut self.video as &mut Mem,
      0xc000...0xfdff | // WRAM (including echo area 0xe000-0xfdff)
      0xff80...0xfffe   // HRAM
                       => &mut self.wram as &mut Mem,
      0xff00           => &mut self.joypad as &mut Mem,
      0xff01...0xff02  => &mut self.serial as &mut Mem,
      0xff04...0xff07  => &mut self.timer as &mut Mem,
      0xff0f | 0xffff  => &mut self.intr as &mut Mem,
      0xff10...0xff3f  => &mut self.sound as &mut Mem,
      _ => &mut self.dummy as &mut Mem,
    }
  }
}

impl<'a> Mem for MemMap<'a> {
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
  renderer: sdl2::render::Renderer<'static>,
  texture: sdl2::render::Texture,
}

impl VideoOut {
  fn new(sdl_video: &sdl2::VideoSubsystem, scale: u32) -> VideoOut {
    let window_width = video::SCREEN_WIDTH as u32 * scale;
    let window_height = video::SCREEN_HEIGHT as u32 * scale;

    let window = sdl_video.window("rustboy", window_width, window_height)
      .position_centered()
      .build()
      .unwrap();

    let renderer = window.renderer().build().unwrap();

    let texture = renderer.create_texture_streaming(
        sdl2::pixels::PixelFormatEnum::ARGB8888,
        video::SCREEN_WIDTH as u32,
        video::SCREEN_HEIGHT as u32).unwrap();

    VideoOut { renderer: renderer, texture: texture }
  }

  fn blit_and_present(&mut self, pixels: &[u8]) {
    let _ = self.texture.update(None, pixels, video::SCREEN_WIDTH * 4);
    self.renderer.copy(&self.texture, None, None);
    self.renderer.present();
  }

  fn set_title(&mut self, title: &str) {
    self.renderer.window_mut().unwrap().set_title(title);
  }
}


fn keymap(code: sdl2::keyboard::Keycode) -> Option<joypad::Button> {
  match code {
    sdl2::keyboard::Keycode::Up     => Some(joypad::Button::Up),
    sdl2::keyboard::Keycode::Down   => Some(joypad::Button::Down),
    sdl2::keyboard::Keycode::Left   => Some(joypad::Button::Left),
    sdl2::keyboard::Keycode::Right  => Some(joypad::Button::Right),
    sdl2::keyboard::Keycode::Return => Some(joypad::Button::Start),
    sdl2::keyboard::Keycode::RShift => Some(joypad::Button::Select),
    sdl2::keyboard::Keycode::C      => Some(joypad::Button::A),
    sdl2::keyboard::Keycode::X      => Some(joypad::Button::B),
    _ => None,
  }
}


#[derive(PartialEq)]
enum State {
  Paused,
  Running,
  Step,
  Done,
}

fn main() {
  let args: Vec<_> = std::env::args().collect();
  if args.len() != 2 && !(args.len() == 3 && args[1] == "-d".to_string()) {
    println!("Usage: {} [-d] rom.gb", args[0]);
    return;
  }

  let mut disassemble = false;
  let path =
    if args.len() == 2 {
      &args[1]
    } else {
      disassemble = true;
      &args[2]
    };

  let mut cart = match cartridge::Cartridge::from_path(&Path::new(path)) {
    Ok(cart) => Box::new(cart),
    Err(e)   => panic!("I/O error: {}", e),
  };

  if disassemble {
    // Disassemble only
    let mut d = disasm::Disasm { mem: &mut *cart, pc: 0 };
    while d.pc <= 0x7fff {
      let pc = d.pc;
      println!("${:04X}\t{}", pc, cpu::decode(&mut d));
    }
    return;
  }

  println!("Name: {}", cart.title);
  println!("Type: {}", cart.cartridge_type);

  let memmap = MemMap {
    cart: cart,
    wram: ram::WorkRam::new(),
    timer: timer::Timer::new(),
    intr: interrupt::InterruptCtrl::new(),
    sound: sound::Sound,
    video: video::Video::new(),
    serial: serial::SerialIO::new(Some(Box::new(stdout()))),
    joypad: joypad::Joypad::new(),
    dummy: Dummy,
  };
  let mut cpu = cpu::Cpu::new(memmap);
  cpu.regs.pc = 0x100;

  let sdl_context = sdl2::init().unwrap();
  let sdl_video = sdl_context.video().unwrap();
  let mut sdl_events = sdl_context.event_pump().unwrap();
  let mut sdl_timer = sdl_context.timer().unwrap();

  let mut video_out = VideoOut::new(&sdl_video, 4);
  video_out.set_title("Rustboy");

  let mut state = State::Paused;
  let mut debugger = debug::Debugger::new();

  let counts_per_sec = sdl_timer.performance_frequency();
  let counts_per_frame = counts_per_sec * video::SCREEN_REFRESH_CYCLES as u64 / cpu::CYCLES_PER_SEC as u64;
  let mut last_frame_start_count = sdl_timer.performance_counter();

  let mut last_fps_update = last_frame_start_count;
  let mut frames = 0;

  println!("c/s: {}; c/f: {}", counts_per_sec, counts_per_frame);

  while state != State::Done {
    if state == State::Paused || state == State::Step {
      match debugger.prompt(&mut cpu) {
        debug::DebuggerCommand::Quit => break,
        debug::DebuggerCommand::Run  => state = State::Running,
        debug::DebuggerCommand::Step => state = State::Step,
      }
    }

    // Emulation loop
    loop {
      let cycles = cpu.step();

      match cpu.mem.timer.tick(cycles) {
        Some(timer::Signal::TIMAOverflow) => cpu.mem.intr.irq(interrupt::IRQ_TIMER),
        None => (),
      }

      let mut new_frame = false;
      let video_signals = cpu.mem.video.tick(cycles);
      for signal in video_signals.iter() {
        match *signal {
          video::Signal::DMA(base) => {
            // Do DMA transfer instantaneously
            let base_addr = (base as u16) << 8;
            for offset in (0x00u16..0xa0u16) {
              let val = cpu.mem.loadb(base_addr + offset);
              cpu.mem.storeb(0xfe00 + offset, val);
            }
          },
          video::Signal::VBlank => {
            video_out.blit_and_present(&cpu.mem.video.screen);
            cpu.mem.intr.irq(interrupt::IRQ_VBLANK);
            new_frame = true;
          }
          video::Signal::LCD    => cpu.mem.intr.irq(interrupt::IRQ_LCD),
        }
      }

      // Synchronize speed based on frame time
      if new_frame {
        let now = sdl_timer.performance_counter();
        let frame_time = now - last_frame_start_count;
        if frame_time < counts_per_frame {
          let delay_msec = 1_000 * (counts_per_frame - frame_time) / counts_per_sec;
          sdl_timer.delay(delay_msec as u32);
        }
        // TODO: What should we do when we take longer than counts_per_frame?
        last_frame_start_count = sdl_timer.performance_counter();

        frames += 1;
        if last_frame_start_count - last_fps_update > counts_per_sec {
          let fps = frames * (last_frame_start_count - last_fps_update) / counts_per_sec;
          video_out.set_title(format!("Rustboy - {} fps", fps).as_ref());
          last_fps_update = now;
          frames = 0;
        }

        // Exit emulation loop to handle events
        break;
      }

      if debugger.should_break(&cpu) {
        state = State::Paused;
        break;
      }

      if state == State::Step {
        // Stop emulation loop after one instruction
        break;
      }
    }

    // Event handling loop
    for event in sdl_events.poll_iter() {
      use sdl2::event::Event;

      match event {
        Event::Quit{..} => { state = State::Done; break }
        Event::KeyDown{keycode: Some(key), ..} => {
          match keymap(key) {
            Some(button) => cpu.mem.joypad.set_button(button, true),
            None => {
              match key {
                sdl2::keyboard::Keycode::Escape => { state = State::Paused },
                _ => (),
              }
            }
          }
        },
        Event::KeyUp{keycode: Some(key), ..} => {
          match keymap(key) {
            Some(button) => cpu.mem.joypad.set_button(button, false),
            None => (),
          }
        }
        _ => (),
      }
    }
  }
}
