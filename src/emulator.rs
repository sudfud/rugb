mod bus;
mod cartridge;
mod cpu;
mod interrupts;
mod joypad;
mod ppu;
mod serial;
mod timer;

use std::path::Path;

use bus::Bus;
use cartridge::{Cartridge, CartridgeError};
use cpu::{CPU, CPUError};
use interrupts::Interrupts;
use joypad::Joypad;
use ppu::{PPU, FrameBuffer};
use serial::Serial;
use timer::Timer;

const WRAM_SIZE: usize = 0x2000;
const HRAM_SIZE: usize = 0x7F;

type WRAM = [u8; WRAM_SIZE];
type HRAM = [u8; HRAM_SIZE];

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
    cartridge: Cartridge,
    cpu: CPU,
    hram: HRAM,
    interrupts: Interrupts,
    joypad: Joypad,
    ppu: PPU,
    serial: Serial,
    timer: Timer,
    wram: WRAM
}

impl Emulator {
    pub(super) fn new(cart_path: &Path) -> Result<Self, EmulatorError> {
        let cartridge = Cartridge::try_from(cart_path).map_err(EmulatorError::Cartridge)?;

        Ok(Self {
            cartridge,
            cpu: CPU::new(),
            hram: [0; HRAM_SIZE],
            interrupts: Interrupts::new(),
            joypad: Joypad::new(),
            ppu: PPU::new(),
            serial: Serial::new(),
            timer: Timer::new(),
            wram: [0; WRAM_SIZE]
        })
    }

    pub(super) fn frame_buffer(&self) -> &FrameBuffer {
        &self.ppu.frame_buffer()
    }

    pub(super) fn step(&mut self) -> Result<u32, EmulatorError> {
        let bus = Bus {
            cartridge: &mut self.cartridge,
            hram: &mut self.hram,
            interrupts: &mut self.interrupts,
            joypad: &mut self.joypad,
            ppu: &mut self.ppu,
            serial: &mut self.serial,
            timer: &mut self.timer,
            wram: &mut self.wram
        };

        let ticks = self.cpu.execute(bus).map_err(EmulatorError::CPU)?;

        Ok(ticks)
    }
}
