mod emulator;

extern crate sdl2;

use std::{fmt, path::Path};

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;

use std::time::Duration;

use emulator::{Emulator, EmulatorError};

const SCREEN_WIDTH: u32 = 160;
const SCREEN_HEIGHT: u32 = 144;

#[derive(Debug)]
enum RugbError {
    Emulator(EmulatorError),
    SDL(String),
}

impl fmt::Display for RugbError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        let mut emulator = Emulator::try_from(Path::new(&args[1])).map_err(RugbError::Emulator)?;
        emulator.run().map_err(RugbError::Emulator)?;

        // SDL Setup
        // let sdl_context = sdl2::init().map_err(RugbError::SDL)?;
        // let video_subsystem = sdl_context.video().map_err(RugbError::SDL)?;

        // let window = video_subsystem
        //     .window("demo", SCREEN_WIDTH, SCREEN_HEIGHT)
        //     .position_centered()
        //     .build()
        //     .map_err(|e| RugbError::SDL(e.to_string()))?;

        // let mut canvas = window
        //     .into_canvas()
        //     .build()
        //     .map_err(|e| RugbError::SDL(e.to_string()))?;

        // canvas.set_draw_color(Color::BLACK);
        // canvas.clear();
        // canvas.present();

        // let mut event_pump = sdl_context.event_pump().map_err(RugbError::SDL)?;
        // let mut r = 0;

        // 'running: loop {
        //     canvas.clear();

        //     let mut b = 0;

        //     for row in 0..SCREEN_HEIGHT {
        //         let mut g = 0;

        //         for column in 0..SCREEN_WIDTH {
        //             canvas.set_draw_color(Color::RGB(r, g, b));
        //             canvas.draw_point((column as i32, row as i32));
        //             g += 1;
        //         }

        //         b += 1;
        //     }

        //     for event in event_pump.poll_iter() {
        //         match event {
        //             Event::Quit { .. } => break 'running,
        //             _ => {}
        //         }
        //     }

        //     r = (r + 1) % 255;

        //     canvas.present();
        //     std::thread::sleep(Duration::new(0, 1_000_000_000 / 60));
        // }
    } else {
        println!("Usage: rugb [file path]");
    }

    Ok(())
}
