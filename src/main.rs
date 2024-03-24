#![allow(clippy::needless_range_loop)]

#[macro_use]
extern crate log;

use pixels::{wgpu::BlendState, wgpu::TextureFormat, Pixels, PixelsBuilder, SurfaceTexture};
use std::time::{Duration, Instant};
use winit::{
    dpi::LogicalSize,
    event::{ElementState, Event, KeyEvent, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowBuilder},
};

mod cartridge;
mod cpu;
mod debug;
mod disasm;
mod gameboy;
mod instr;
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
        .blend_state(BlendState::REPLACE)
        .enable_vsync(false)
        .build()
        .unwrap();

        VideoOut { window, pixels }
    }

    fn blit(&mut self, pixels: &[u8]) {
        self.pixels.frame_mut().copy_from_slice(pixels);
    }

    fn render(&mut self) {
        self.pixels.render().unwrap();
    }

    fn set_title(&mut self, title: &str) {
        self.window.set_title(title);
    }
}

fn keymap(code: KeyCode) -> Option<joypad::Button> {
    match code {
        KeyCode::ArrowUp => Some(joypad::Button::Up),
        KeyCode::ArrowDown => Some(joypad::Button::Down),
        KeyCode::ArrowLeft => Some(joypad::Button::Left),
        KeyCode::ArrowRight => Some(joypad::Button::Right),
        KeyCode::Enter => Some(joypad::Button::Start),
        KeyCode::ShiftRight => Some(joypad::Button::Select),
        KeyCode::KeyC => Some(joypad::Button::A),
        KeyCode::KeyX => Some(joypad::Button::B),
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

#[derive(argh::FromArgs)]
#[argh(description = "rustboy is an emulator for the Nintendo GameBoy.")]
struct Args {
    #[argh(switch, short = 'd')]
    #[argh(description = "disassemble ROM")]
    disassemble: bool,

    #[argh(switch, short = 'p')]
    #[argh(description = "pause at start")]
    pause: bool,

    #[argh(positional)]
    #[argh(description = "ROM path")]
    rom: String,
}

fn main() {
    let args: Args = argh::from_env();

    let mut cart = match cartridge::Cartridge::from_path(args.rom) {
        Ok(cart) => cart,
        Err(e) => panic!("I/O error: {}", e),
    };

    if args.disassemble {
        // Disassemble only
        let mut d = disasm::Disasm {
            mem: &mut cart,
            pc: 0,
        };
        while d.pc <= 0x7fff {
            let pc = d.pc;
            println!("${:04X}\t{}", pc, d.disasm());
        }
        return;
    }

    println!("Name: {}", cart.title);
    println!("Type: {}", cart.cartridge_type);

    let mut gameboy = gameboy::GameBoy::new(cart);

    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut video_out = VideoOut::new(&event_loop, 4);
    video_out.set_title("Rustboy");

    let mut state = if args.pause {
        State::Paused
    } else {
        State::Running
    };
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

    event_loop
        .run(move |event, elwt| {
            if let Event::WindowEvent {
                event: WindowEvent::RedrawRequested,
                ..
            } = event
            {
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
                        event:
                            KeyEvent {
                                physical_key: PhysicalKey::Code(keycode),
                                state: keystate,
                                ..
                            },
                        ..
                    },
                ..
            } = event
            {
                if keystate == ElementState::Pressed && keycode == KeyCode::Escape {
                    state = State::Paused;
                }

                if let Some(button) = keymap(keycode) {
                    gameboy.set_button(button, keystate == ElementState::Pressed);
                }
            }

            if let Event::AboutToWait = event {
                if state == State::Done {
                    elwt.exit();
                    return;
                }

                // Debugger
                if state == State::Paused || state == State::Step {
                    match debugger.prompt(gameboy.cpu(), gameboy.mem()) {
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
                if state == State::WaitForSync && frame_start.elapsed() >= frame_duration {
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
        })
        .unwrap();
}
