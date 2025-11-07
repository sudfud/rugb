mod emulator;

extern crate sdl2;

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

use std::time::Duration;

use emulator::{Emulator, EmulatorError};

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

type LCD = Rc<RefCell<[[Color; SCREEN_WIDTH]; SCREEN_HEIGHT]>>;

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
        let lcd = Rc::new(RefCell::new([[Color::BLACK; SCREEN_WIDTH]; SCREEN_HEIGHT]));
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

        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.present();

        let mut event_pump = sdl_context.event_pump().map_err(RugbError::SDL)?;

        'running: loop {
            emulator.step().map_err(RugbError::Emulator)?;

            canvas.clear();

            for row in 0..SCREEN_HEIGHT {
                for column in 0..SCREEN_WIDTH {
                    canvas.set_draw_color(lcd.borrow()[row][column]);
                    canvas.draw_point((column as i32, row as i32));
                }
            }

            canvas.present();

            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. } => break 'running,
                    _ => {}
                }
            }
            // std::thread::sleep(Duration::new(0, 1_000_000_000 / 60));
        }
    } else {
        println!("Usage: rugb [file path]");
    }

    Ok(())
}
