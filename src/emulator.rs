mod cartridge;
mod cpu;

use std::cell::RefCell;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::rc::Rc;

use crate::FrameBuffer;

use cartridge::{Cartridge, CartridgeError};
use cpu::{CPU, CPUError};

#[derive(Debug)]
pub(super) enum EmulatorError {
    Cartridge(CartridgeError),
    CPU(CPUError)
}

impl std::fmt::Display for EmulatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Cartridge(e) => e.to_string(),
                Self::CPU(e) => e.to_string(),
            }
        )
    }
}

pub(super) struct Emulator {
    cpu: CPU,
}

impl Emulator {
    pub(super) fn new(cart_path: &Path, frame_buffer: FrameBuffer) -> Result<Self, EmulatorError> {
        let cartridge = Cartridge::try_from(cart_path).map_err(EmulatorError::Cartridge)?;

        Ok(Self {
            cpu: CPU::new(cartridge, frame_buffer),
        })
    }

    pub(super) fn pc(&self) -> u16 {
        self.cpu.pc()
    }

    pub(super) fn step(&mut self) -> Result<u32, EmulatorError> {
        let ticks = self.cpu.execute().map_err(EmulatorError::CPU)?;

        if self.cpu.read(0xFF02) == 0x81 {
            let c = self.cpu.read(0xFF01) as char;
            print!("{}", c);
            self.cpu.write(0xFF02, 0x00);
        }

        Ok(ticks)
    }
}
