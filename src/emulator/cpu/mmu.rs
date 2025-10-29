mod gpu;
mod joypad;
mod serial;
mod timer;

use crate::emulator::cartridge::Cartridge;

use gpu::GPU;
use joypad::Joypad;
use serial::Serial;
use timer::Timer;

const WRAM_SIZE: usize = 0x2000;
const HRAM_SIZE: usize = 0x7F;

const ROM_START: u16 = 0x0000;
const VRAM_START: u16 = 0x8000;
const RAM_START: u16 = 0xA000;
const WRAM_START: u16 = 0xC000;
const ECHO_START: u16 = 0xE000;
const OAM_START: u16 = 0xFE00;
const UNUSED_START: u16 = 0xFEA0;
const IO_START: u16 = 0xFF00;
const HRAM_START: u16 = 0xFF80;

const REG_SB: u16 = 0xFF01;
const REG_SC: u16 = 0xFF02;

const REG_IE: u16 = 0xFFFF;

pub(super) struct MMU {
    cartridge: Cartridge,
    gpu: GPU,
    wram: [u8; WRAM_SIZE],
    hram: [u8; HRAM_SIZE],
    joypad: Joypad,
    serial: Serial,
    timer: Timer,
    interrupt_enable: u8
}

impl MMU {
    pub(super) fn new(cartridge: Cartridge) -> Self {
        Self {
            cartridge,
            gpu: GPU::new(),
            wram: [0; WRAM_SIZE],
            hram: [0; HRAM_SIZE],
            joypad: Joypad::new(),
            serial: Serial::new(),
            timer: Timer::new(),
            interrupt_enable: 0x00
        }
    }

    pub(super) fn read(&self, address: u16) -> u8 {
        match address {
            ROM_START..VRAM_START => self.cartridge.read_rom(address),
            VRAM_START..RAM_START => self.gpu.read_vram(address - VRAM_START),
            RAM_START..WRAM_START => self.cartridge.read_ram(address - RAM_START),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize],
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize],
            OAM_START..UNUSED_START => self.gpu.read_oam(address - OAM_START),

            REG_SB => self.serial.data(),
            REG_SC => self.serial.control(),

            HRAM_START..REG_IE => self.hram[(address - HRAM_START) as usize],
            REG_IE => self.interrupt_enable,

            _ => 0xFF
        }
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        match address {
            ROM_START..VRAM_START => self.cartridge.write_rom(address, value),
            VRAM_START..RAM_START => self.gpu.write_vram(address - VRAM_START, value),
            RAM_START..WRAM_START => self.cartridge.write_ram(address - RAM_START, value),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize] = value,
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize] = value,
            OAM_START..UNUSED_START => self.gpu.write_oam(address - OAM_START, value),

            REG_SB => self.serial.set_data(value),
            REG_SC => self.serial.set_control(value),

            HRAM_START..REG_IE => self.hram[(address - HRAM_START) as usize] = value,
            REG_IE => self.interrupt_enable = value,

            _ => {}
        }
    }
}