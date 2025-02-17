#![allow(clippy::needless_range_loop)]

#[macro_use]
extern crate log;

use cartridge::Cartridge;
use debug::Debugger;
use gameboy::GameBoy;
use pixels::{
    wgpu::{BlendState, TextureFormat},
    Pixels, PixelsBuilder, SurfaceTexture,
};
use std::{
    sync::Arc,
    time::{Duration, Instant},
};
use winit::{
    application::ApplicationHandler,
    dpi::LogicalSize,
    event::{ElementState, KeyEvent, WindowEvent},
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    keyboard::{KeyCode, PhysicalKey},
    window::{Window, WindowId},
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

const SCALE: u32 = 4;
const WIDTH: u32 = video::SCREEN_WIDTH as u32 * SCALE;
const HEIGHT: u32 = video::SCREEN_HEIGHT as u32 * SCALE;

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

struct App<'a> {
    window: Option<Arc<Window>>,
    pixels: Option<Pixels<'static>>,
    gameboy: GameBoy<'a>,
    debugger: Debugger,
    state: State,
    frame_duration: Duration,
    frame_start: Instant,
    last_fps_update: Instant,
    frames: usize,
}

impl<'a> App<'a> {
    pub fn new(cart: Cartridge) -> App<'a> {
        let now = Instant::now();

        let frame_duration = Duration::from_secs_f64(
            video::SCREEN_REFRESH_CYCLES as f64 / cpu::CYCLES_PER_SEC as f64,
        );

        println!(
            "ft: {:?} / fps: {:.02}",
            frame_duration,
            1.0 / frame_duration.as_secs_f64()
        );

        App {
            window: None,
            pixels: None,
            gameboy: GameBoy::new(cart),
            debugger: Debugger::new(),
            state: State::Paused,
            frame_duration,
            frame_start: now,
            last_fps_update: now,
            frames: 0,
        }
    }
}

impl ApplicationHandler for App<'_> {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let size = LogicalSize::new(WIDTH, HEIGHT);

        let window = event_loop
            .create_window(
                Window::default_attributes()
                    .with_inner_size(size)
                    .with_resizable(false)
                    .with_title("Rustboy"),
            )
            .unwrap();
        let window = Arc::new(window);
        self.window = Some(window.clone());

        let size = window.inner_size();
        let texture = SurfaceTexture::new(size.width, size.height, window.clone());

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

        self.pixels = Some(pixels);
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        if self.state == State::Done {
            event_loop.exit();
            return;
        }

        // Debugger
        if self.state == State::Paused || self.state == State::Step {
            match self.debugger.prompt(self.gameboy.cpu(), self.gameboy.mem()) {
                debug::DebuggerCommand::Quit => self.state = State::Done,
                debug::DebuggerCommand::Run => self.state = State::Running,
                debug::DebuggerCommand::Step => self.state = State::Step,
            }
        }

        // Emulation loop
        if self.state == State::Running {
            let mut new_frame;
            loop {
                new_frame = self.gameboy.step();

                if self.debugger.should_break(self.gameboy.cpu()) {
                    self.state = State::Paused;
                    break;
                }

                if new_frame || self.state == State::Step {
                    break;
                }
            }

            if new_frame {
                self.pixels
                    .as_mut()
                    .unwrap()
                    .frame_mut()
                    .copy_from_slice(self.gameboy.pixels());
                self.pixels.as_ref().unwrap().render().unwrap();

                self.state = State::WaitForSync;
            }
        }

        // Sync frame rate
        if self.state == State::WaitForSync && self.frame_start.elapsed() >= self.frame_duration {
            self.state = State::Running;

            self.frame_start = Instant::now();

            self.frames += 1;

            if self.last_fps_update.elapsed() > Duration::from_secs(1) {
                let fps = self.frames as f64 / self.last_fps_update.elapsed().as_secs_f64();
                self.window
                    .as_ref()
                    .unwrap()
                    .set_title(format!("Rustboy - {:.02} fps", fps).as_ref());
                self.last_fps_update = self.frame_start;
                self.frames = 0;
            }
        }
    }

    fn window_event(
        &mut self,
        _event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        match event {
            WindowEvent::RedrawRequested => {
                self.pixels.as_ref().unwrap().render().unwrap();
            }
            WindowEvent::CloseRequested => {
                self.state = State::Done;
            }
            WindowEvent::KeyboardInput {
                event:
                    KeyEvent {
                        physical_key: PhysicalKey::Code(keycode),
                        state: keystate,
                        ..
                    },
                ..
            } => {
                if keystate == ElementState::Pressed && keycode == KeyCode::Escape {
                    self.state = State::Paused;
                }

                if let Some(button) = keymap(keycode) {
                    self.gameboy
                        .set_button(button, keystate == ElementState::Pressed);
                }
            }
            _ => {}
        }
    }
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

    let mut app = App::new(cart);
    app.state = if args.pause {
        State::Paused
    } else {
        State::Running
    };

    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    event_loop.run_app(&mut app).unwrap()
}
