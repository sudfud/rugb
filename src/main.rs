mod emulator;

extern crate sdl2;

use std::path::Path;
use std::time::{Duration, Instant};

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::TextureAccess;

use emulator::{ActionButton, DirectionButton};
use emulator::{Emulator, EmulatorError};

const FRAME_TICKS: u32 = 70224;
const FRAME_TIME: Duration = Duration::from_micros(16750);

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

#[derive(Debug)]
enum RugbError {
    Emulator(EmulatorError),
    Sdl(String),
}

impl std::fmt::Display for RugbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error: {}",
            match self {
                Self::Emulator(e) => e.to_string(),
                Self::Sdl(s) => s.clone(),
            }
        )
    }
}

impl std::error::Error for RugbError {}

fn main() -> Result<(), RugbError> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 {
        let mut emulator = Emulator::new(Path::new(&args[1])).map_err(RugbError::Emulator)?;

        // SDL Setup
        let sdl_context = sdl2::init().map_err(RugbError::Sdl)?;
        let video_subsystem = sdl_context.video().map_err(RugbError::Sdl)?;

        let mut window = video_subsystem
            .window("demo", SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
            .position_centered()
            .resizable()
            .build()
            .map_err(|e| RugbError::Sdl(e.to_string()))?;

        window
            .set_minimum_size(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
            .map_err(|e| RugbError::Sdl(e.to_string()))?;

        let mut canvas = window
            .into_canvas()
            .build()
            .map_err(|e| RugbError::Sdl(e.to_string()))?;

        canvas.set_draw_color(Color::BLACK);
        canvas
            .set_logical_size(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
            .map_err(|e| RugbError::Sdl(e.to_string()))?;

        let texture_creator = canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture(
                Some(PixelFormatEnum::RGB24),
                TextureAccess::Streaming,
                SCREEN_WIDTH as u32,
                SCREEN_HEIGHT as u32,
            )
            .map_err(|e| RugbError::Sdl(e.to_string()))?;

        let mut current_time = Instant::now();
        let mut event_pump = sdl_context.event_pump().map_err(RugbError::Sdl)?;
        let mut tick_count = 0;

        'running: loop {
            tick_count += emulator.step().map_err(RugbError::Emulator)?;

            if tick_count >= FRAME_TICKS {
                tick_count -= FRAME_TICKS;

                canvas.clear();

                texture
                    .with_lock(None, |pixels, _pitch| {
                        pixels.copy_from_slice(emulator.frame_buffer().as_slice());
                    })
                    .map_err(RugbError::Sdl)?;

                canvas.copy(&texture, None, None).map_err(RugbError::Sdl)?;

                canvas.present();

                if Instant::now() - current_time < FRAME_TIME {
                    let sleep_time = FRAME_TIME - (Instant::now() - current_time);
                    std::thread::sleep(sleep_time);
                }

                current_time = Instant::now();

                for event in event_pump.poll_iter() {
                    match event {
                        Event::KeyDown { keycode, .. } => {
                            update_key_press(&mut emulator, keycode, true)
                        }
                        Event::KeyUp { keycode, .. } => {
                            update_key_press(&mut emulator, keycode, false)
                        }
                        Event::Quit { .. } => break 'running,
                        _ => {}
                    }
                }
            }
        }
    } else {
        println!("Usage: rugb [file path]");
    }

    Ok(())
}

fn update_key_press(emulator: &mut Emulator, keycode: Option<Keycode>, pressed: bool) {
    match keycode {
        Some(Keycode::W) => emulator.set_direction_button(DirectionButton::Up, pressed),
        Some(Keycode::A) => emulator.set_direction_button(DirectionButton::Left, pressed),
        Some(Keycode::S) => emulator.set_direction_button(DirectionButton::Down, pressed),
        Some(Keycode::D) => emulator.set_direction_button(DirectionButton::Right, pressed),

        Some(Keycode::Z) => emulator.set_action_button(ActionButton::A, pressed),
        Some(Keycode::X) => emulator.set_action_button(ActionButton::B, pressed),
        Some(Keycode::Space) => emulator.set_action_button(ActionButton::Select, pressed),
        Some(Keycode::Return) => emulator.set_action_button(ActionButton::Start, pressed),
        _ => {}
    }
}
