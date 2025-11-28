mod apu;
mod bus;
mod cartridge;
mod cpu;
mod dma;
mod interrupts;
mod joypad;
mod ppu;
mod serial;
mod timer;

pub(super) use joypad::{ActionButton, DirectionButton};

use std::path::Path;

use bus::Bus;
use cartridge::{Cartridge, CartridgeError};
use cpu::{Cpu, CpuError};
use dma::Dma;
use interrupts::Interrupts;
use joypad::Joypad;
use ppu::{FrameBuffer, Ppu};
use serial::Serial;
use timer::Timer;

const HRAM_SIZE: usize = 0x7F;
const OAM_SIZE: usize = 0xA0;
const VRAM_SIZE: usize = 0x2000;
const WRAM_SIZE: usize = 0x2000;

type Hram = [u8; HRAM_SIZE];
type Oam = [u8; OAM_SIZE];
type Vram = [u8; VRAM_SIZE];
type Wram = [u8; WRAM_SIZE];

#[derive(Debug)]
pub(super) enum EmulatorError {
    Cartridge(CartridgeError),
    Cpu(CpuError),
}

impl std::fmt::Display for EmulatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Cartridge(e) => e.to_string(),
                Self::Cpu(e) => e.to_string(),
            }
        )
    }
}

pub(super) struct Emulator {
    cartridge: Cartridge,
    cpu: Cpu,
    dma: Dma,
    hram: Hram,
    interrupts: Interrupts,
    joypad: Joypad,
    oam: Oam,
    ppu: Ppu,
    serial: Serial,
    timer: Timer,
    vram: Vram,
    wram: Wram,
}

impl Emulator {
    pub(super) fn new(cart_path: &Path) -> Result<Self, EmulatorError> {
        let cartridge = Cartridge::try_from(cart_path).map_err(EmulatorError::Cartridge)?;

        Ok(Self {
            cartridge,
            cpu: Cpu::new(),
            dma: Dma::new(),
            hram: [0; HRAM_SIZE],
            interrupts: Interrupts::new(),
            joypad: Joypad::new(),
            oam: [0; OAM_SIZE],
            ppu: Ppu::new(),
            serial: Serial::new(),
            timer: Timer::new(),
            vram: [0; VRAM_SIZE],
            wram: [0; WRAM_SIZE],
        })
    }

    pub(super) fn frame_buffer(&self) -> &FrameBuffer {
        self.ppu.frame_buffer()
    }

    pub(super) fn step(&mut self) -> Result<u32, EmulatorError> {
        let bus = Bus {
            cartridge: &mut self.cartridge,
            dma: &mut self.dma,
            hram: &mut self.hram,
            interrupts: &mut self.interrupts,
            joypad: &mut self.joypad,
            oam: &mut self.oam,
            ppu: &mut self.ppu,
            serial: &mut self.serial,
            timer: &mut self.timer,
            vram: &mut self.vram,
            wram: &mut self.wram,
        };

        let ticks = self.cpu.execute(bus).map_err(EmulatorError::Cpu)?;

        Ok(ticks)
    }

    pub(super) fn set_action_button(&mut self, button: ActionButton, pressed: bool) {
        self.joypad.set_action_button(button, pressed);
    }

    pub(super) fn set_direction_button(&mut self, button: DirectionButton, pressed: bool) {
        self.joypad.set_direction_button(button, pressed);
    }
}
