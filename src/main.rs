#![allow(clippy::needless_range_loop)]

#[macro_use]
extern crate log;

use pixels::{wgpu::TextureFormat, Pixels, PixelsBuilder, SurfaceTexture};
use std::path::Path;
use std::time::{Duration, Instant};
use winit::{
    dpi::LogicalSize,
    event::{ElementState, Event, KeyboardInput, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

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
    window: Window,
    pixels: Pixels,
}

impl VideoOut {
    fn new(event_loop: &EventLoop<()>, scale: u32) -> VideoOut {
        let window_width = video::SCREEN_WIDTH as u32 * scale;
        let window_height = video::SCREEN_HEIGHT as u32 * scale;

        let size = LogicalSize::new(window_width, window_height);

        let window = WindowBuilder::new()
            .with_inner_size(size)
            .with_resizable(false)
            .build(event_loop)
            .unwrap();

        let size = window.inner_size();
        let texture = SurfaceTexture::new(size.width, size.height, &window);

        let pixels = PixelsBuilder::new(
            video::SCREEN_WIDTH as u32,
            video::SCREEN_HEIGHT as u32,
            texture,
        )
        .texture_format(TextureFormat::Bgra8UnormSrgb)
        .enable_vsync(false)
        .build()
        .unwrap();

        VideoOut { window, pixels }
    }

    fn blit(&mut self, pixels: &[u8]) {
        self.pixels.get_frame().copy_from_slice(pixels);
    }

    fn render(&mut self) {
        self.pixels.render().unwrap();
    }

    fn set_title(&mut self, title: &str) {
        self.window.set_title(title);
    }
}

fn keymap(code: VirtualKeyCode) -> Option<joypad::Button> {
    match code {
        VirtualKeyCode::Up => Some(joypad::Button::Up),
        VirtualKeyCode::Down => Some(joypad::Button::Down),
        VirtualKeyCode::Left => Some(joypad::Button::Left),
        VirtualKeyCode::Right => Some(joypad::Button::Right),
        VirtualKeyCode::Return => Some(joypad::Button::Start),
        VirtualKeyCode::RShift => Some(joypad::Button::Select),
        VirtualKeyCode::C => Some(joypad::Button::A),
        VirtualKeyCode::X => Some(joypad::Button::B),
        _ => None,
    }
}

#[derive(PartialEq)]
enum State {
    Paused,
    Running,
    WaitForSync,
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

    let event_loop = EventLoop::new();

    let mut video_out = VideoOut::new(&event_loop, 4);
    video_out.set_title("Rustboy");

    let mut state = State::Paused;
    let mut debugger = debug::Debugger::new();

    let frame_duration =
        Duration::from_secs_f64(video::SCREEN_REFRESH_CYCLES as f64 / cpu::CYCLES_PER_SEC as f64);
    let mut frame_start = Instant::now();

    let mut last_fps_update = frame_start;
    let mut frames = 0;

    println!(
        "ft: {:?} / fps: {:.02}",
        frame_duration,
        1.0 / frame_duration.as_secs_f64()
    );

    event_loop.run(move |event, _, control_flow| {
        if let Event::RedrawRequested(_) = event {
            println!("RedrawRequested");
            video_out.render();
            return;
        }

        if let Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } = event
        {
            state = State::Done;
        }

        if let Event::WindowEvent {
            event:
                WindowEvent::KeyboardInput {
                    input:
                        KeyboardInput {
                            virtual_keycode: Some(keycode),
                            state: keystate,
                            ..
                        },
                    ..
                },
            ..
        } = event
        {
            if keystate == ElementState::Pressed && keycode == VirtualKeyCode::Escape {
                state = State::Paused;
            }

            if let Some(button) = keymap(keycode) {
                gameboy.set_button(button, keystate == ElementState::Pressed);
            }
        }

        if let Event::MainEventsCleared = event {
            if state == State::Done {
                *control_flow = ControlFlow::Exit;
                return;
            }

            // Debugger
            if state == State::Paused || state == State::Step {
                match debugger.prompt(gameboy.cpu()) {
                    debug::DebuggerCommand::Quit => state = State::Done,
                    debug::DebuggerCommand::Run => state = State::Running,
                    debug::DebuggerCommand::Step => state = State::Step,
                }
            }

            // Emulation loop
            if state == State::Running {
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
                    video_out.blit(gameboy.pixels());
                    video_out.render();

                    state = State::WaitForSync;
                }
            }

            // Sync frame rate
            if state == State::WaitForSync {
                if frame_start.elapsed() >= frame_duration {
                    state = State::Running;

                    frame_start = Instant::now();

                    frames += 1;
                    if last_fps_update.elapsed() > Duration::from_secs(1) {
                        let fps = frames as f64 / last_fps_update.elapsed().as_secs_f64();
                        video_out.set_title(format!("Rustboy - {:.02} fps", fps).as_ref());
                        last_fps_update = frame_start;
                        frames = 0;
                    }
                }
            }
        }
    });
}
