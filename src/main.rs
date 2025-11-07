mod emulator;

extern crate sdl2;

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::render::TextureAccess;

use std::time::Duration;

use emulator::{Emulator, EmulatorError};

const FRAME_TIME: u32 = 70224;

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

type FrameBuffer = Rc<RefCell<[u8; SCREEN_WIDTH * SCREEN_HEIGHT * 3]>>;

#[derive(Debug)]
enum RugbError {
    Emulator(EmulatorError),
    SDL(String),
}

impl std::fmt::Display for RugbError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error: {}",
            match self {
                Self::Emulator(e) => e.to_string(),
                Self::SDL(s) => s.clone(),
            }
        )
    }
}

impl std::error::Error for RugbError {}

fn main() -> Result<(), RugbError> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 2 {
        let lcd = Rc::new(RefCell::new([0; SCREEN_WIDTH * SCREEN_HEIGHT * 3]));
        let mut emulator = Emulator::new(Path::new(&args[1]), Rc::clone(&lcd)).map_err(RugbError::Emulator)?;

        // SDL Setup
        let sdl_context = sdl2::init().map_err(RugbError::SDL)?;
        let video_subsystem = sdl_context.video().map_err(RugbError::SDL)?;

        let window = video_subsystem
            .window("demo", SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
            .position_centered()
            .build()
            .map_err(|e| RugbError::SDL(e.to_string()))?;

        let mut canvas = window
            .into_canvas()
            .build()
            .map_err(|e| RugbError::SDL(e.to_string()))?;

        let texture_creator = canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture(
                Some(PixelFormatEnum::RGB24),
                TextureAccess::Streaming,
                SCREEN_WIDTH as u32,
                SCREEN_HEIGHT as u32
            )
            .map_err(|e| RugbError::SDL(e.to_string()))?;

        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.present();

        let mut event_pump = sdl_context.event_pump().map_err(RugbError::SDL)?;
        let mut tick_count = 0;

        'running: loop {
            tick_count += emulator.step().map_err(RugbError::Emulator)?;

            if tick_count >= FRAME_TIME {
                tick_count -= FRAME_TIME;

                canvas.clear();

                texture.with_lock(None, |pixels, pitch| {
                    pixels.copy_from_slice(lcd.borrow().as_slice());
                }).map_err(RugbError::SDL)?;

                canvas.copy(&texture, None, None).map_err(RugbError::SDL)?;

                canvas.present();
            }

            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. } => break 'running,
                    _ => {}
                }
            }
        }
    } else {
        println!("Usage: rugb [file path]");
    }

    Ok(())
}
