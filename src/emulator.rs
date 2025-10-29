mod cartridge;
mod cpu;

use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;

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
    pub(super) fn run(&mut self) -> Result<(), EmulatorError> {
        let mut output = String::new();

        loop {
            let _cycles = self.cpu.execute().map_err(EmulatorError::CPU)?;

            if self.cpu.read(0xFF02) == 0x81 {
                let c = self.cpu.read(0xFF01) as char;
                output.push(c);
                self.cpu.write(0xFF02, 0x00);

                if output.contains("Passed") || output.contains("Failed") {
                    break;
                }
            }
        }

        println!("{}", output);

        Ok(())
    }
}

impl TryFrom<&Path> for Emulator {
    type Error = EmulatorError;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let cartridge = Cartridge::try_from(value).map_err(EmulatorError::Cartridge)?;

        Ok(Self {
            cpu: CPU::new(cartridge),
        })
    }
}
