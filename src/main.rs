#![allow(clippy::needless_range_loop)]

#[macro_use]
extern crate log;

extern crate sdl2;

use std::path::Path;
use std::time::{Duration, Instant};

mod cartridge;
mod cpu;
mod debug;
mod disasm;
mod gameboy;
mod interrupt;
mod joypad;
mod mem;
mod ram;
mod serial;
mod sound;
mod timer;
mod video;

//
// Video Output
//

struct VideoOut {
    renderer: Box<sdl2::render::WindowCanvas>,
    texture: sdl2::render::Texture<'static>,
    _texture_creator: sdl2::render::TextureCreator<sdl2::video::WindowContext>,
}

impl VideoOut {
    fn new(sdl_video: &sdl2::VideoSubsystem, scale: u32) -> VideoOut {
        let window_width = video::SCREEN_WIDTH as u32 * scale;
        let window_height = video::SCREEN_HEIGHT as u32 * scale;

        let window = sdl_video
            .window("rustboy", window_width, window_height)
            .position_centered()
            .build()
            .unwrap();

        let renderer = window.into_canvas().build().unwrap();

        let texture_creator = renderer.texture_creator();
        let texture_creator_pointer =
            &texture_creator as *const sdl2::render::TextureCreator<sdl2::video::WindowContext>;

        let texture = unsafe { &*texture_creator_pointer }
            .create_texture_streaming(
                sdl2::pixels::PixelFormatEnum::ARGB8888,
                video::SCREEN_WIDTH as u32,
                video::SCREEN_HEIGHT as u32,
            )
            .unwrap();

        VideoOut {
            renderer: Box::new(renderer),
            texture,
            _texture_creator: texture_creator,
        }
    }

    fn blit_and_present(&mut self, pixels: &[u8]) {
        let _ = self.texture.update(None, pixels, video::SCREEN_WIDTH * 4);
        let _ = self.renderer.copy(&self.texture, None, None);
        self.renderer.present();
    }

    fn set_title(&mut self, title: &str) {
        self.renderer.window_mut().set_title(title).unwrap();
    }
}

fn keymap(code: sdl2::keyboard::Keycode) -> Option<joypad::Button> {
    match code {
        sdl2::keyboard::Keycode::Up => Some(joypad::Button::Up),
        sdl2::keyboard::Keycode::Down => Some(joypad::Button::Down),
        sdl2::keyboard::Keycode::Left => Some(joypad::Button::Left),
        sdl2::keyboard::Keycode::Right => Some(joypad::Button::Right),
        sdl2::keyboard::Keycode::Return => Some(joypad::Button::Start),
        sdl2::keyboard::Keycode::RShift => Some(joypad::Button::Select),
        sdl2::keyboard::Keycode::C => Some(joypad::Button::A),
        sdl2::keyboard::Keycode::X => Some(joypad::Button::B),
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
    if args.len() != 2 && !(args.len() == 3 && args[1] == "-d") {
        println!("Usage: {} [-d] rom.gb", args[0]);
        return;
    }

    let mut disassemble = false;
    let path = if args.len() == 2 {
        &args[1]
    } else {
        disassemble = true;
        &args[2]
    };

    let mut cart = match cartridge::Cartridge::from_path(Path::new(path)) {
        Ok(cart) => cart,
        Err(e) => panic!("I/O error: {}", e),
    };

    if disassemble {
        // Disassemble only
        let mut d = disasm::Disasm {
            mem: &mut cart,
            pc: 0,
        };
        while d.pc <= 0x7fff {
            let pc = d.pc;
            println!("${:04X}\t{}", pc, cpu::decode(&mut d));
        }
        return;
    }

    println!("Name: {}", cart.title);
    println!("Type: {}", cart.cartridge_type);

    let mut gameboy = gameboy::GameBoy::new(cart);

    let sdl_context = sdl2::init().unwrap();
    let sdl_video = sdl_context.video().unwrap();
    let mut sdl_events = sdl_context.event_pump().unwrap();
    let mut sdl_timer = sdl_context.timer().unwrap();

    let mut video_out = VideoOut::new(&sdl_video, 4);
    video_out.set_title("Rustboy");

    let mut state = State::Paused;
    let mut debugger = debug::Debugger::new();

    let frame_duration =
        Duration::from_secs_f64(video::SCREEN_REFRESH_CYCLES as f64 / cpu::CYCLES_PER_SEC as f64);
    let mut last_frame_start = Instant::now();

    let mut last_fps_update = last_frame_start;
    let mut frames = 0;

    println!("f: {:?}", frame_duration);

    while state != State::Done {
        // Debugger
        if state == State::Paused || state == State::Step {
            match debugger.prompt(gameboy.cpu()) {
                debug::DebuggerCommand::Quit => break,
                debug::DebuggerCommand::Run => state = State::Running,
                debug::DebuggerCommand::Step => state = State::Step,
            }
        }

        // Emulation loop
        let mut new_frame;
        loop {
            new_frame = gameboy.step();

            if debugger.should_break(gameboy.cpu()) {
                state = State::Paused;
                break;
            }

            if new_frame || state == State::Step {
                break;
            }
        }

        if new_frame {
            video_out.blit_and_present(gameboy.pixels());

            // Synchronize speed based on frame time
            let now = Instant::now();
            let frame_time = now - last_frame_start;
            if frame_time < frame_duration {
                let delay_msec = (frame_duration - frame_time).as_millis();
                sdl_timer.delay(delay_msec as u32);
            }
            // TODO: What should we do when we take longer than frame_duration?
            last_frame_start = Instant::now();

            frames += 1;
            let time_since_last_fps_update = last_frame_start - last_fps_update;
            if time_since_last_fps_update > Duration::from_secs(1) {
                let fps = frames as f64 / (time_since_last_fps_update).as_secs_f64();
                video_out.set_title(format!("Rustboy - {:.02} fps", fps).as_ref());
                last_fps_update = now;
                frames = 0;
            }
        }

        // Event handling loop
        for event in sdl_events.poll_iter() {
            use sdl2::event::Event;

            match event {
                Event::Quit { .. } => {
                    state = State::Done;
                    break;
                }
                Event::KeyDown {
                    keycode: Some(key), ..
                } => match keymap(key) {
                    Some(button) => gameboy.set_button(button, true),
                    None => {
                        if key == sdl2::keyboard::Keycode::Escape {
                            state = State::Paused;
                        }
                    }
                },
                Event::KeyUp {
                    keycode: Some(key), ..
                } => {
                    if let Some(button) = keymap(key) {
                        gameboy.set_button(button, false)
                    }
                }
                _ => (),
            }
        }
    }
}
