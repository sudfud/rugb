mod ppu;
mod joypad;
mod serial;
mod timer;

use crate::emulator::cartridge::Cartridge;

use ppu::PPU;
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

const REG_DIV: u16 = 0xFF04;
const REG_TIMA: u16 = 0xFF05;
const REG_TMA: u16 = 0xFF06;
const REG_TAC: u16 = 0xFF07;

const REG_IF: u16 = 0xFF0F;

const REG_LCDC: u16 = 0xFF40;
const REG_STAT: u16 = 0xFF41;
const REG_SCY: u16 = 0xFF42;
const REG_SCX: u16 = 0xFF43;
const REG_LY: u16 = 0xFF44;
const REG_LYC: u16 = 0xFF45;
const REG_DMA: u16 = 0xFF46;
const REG_BGP: u16 = 0xFF47;
const REG_OBP0: u16 = 0xFF48;
const REG_OBP1: u16 = 0xFF49;
const REG_WY: u16 = 0xFF4A;
const REG_WX: u16 = 0xFF4B;

const REG_IE: u16 = 0xFFFF;

#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum Interrupt {
    VBlank = 0b0000_0001,
    LCD = 0b0000_0010,
    Timer = 0b0000_0100,
    Serial = 0b0000_1000,
    Joypad = 0b0001_0000
}

pub(super) struct MMU {
    cartridge: Cartridge,
    ppu: PPU,
    wram: [u8; WRAM_SIZE],
    hram: [u8; HRAM_SIZE],
    joypad: Joypad,
    serial: Serial,
    pub timer: Timer,
    interrupt_enable: u8,
    interrupt_flag: u8
}

impl MMU {
    pub(super) fn new(cartridge: Cartridge) -> Self {
        Self {
            cartridge,
            ppu: PPU::new(),
            wram: [0; WRAM_SIZE],
            hram: [0; HRAM_SIZE],
            joypad: Joypad::new(),
            serial: Serial::new(),
            timer: Timer::new(),
            interrupt_enable: 0x00,
            interrupt_flag: 0x00
        }
    }

    pub(super) fn read_cycle(&mut self, address: u16) -> u8 {
        let byte = self.read(address);
        self.cycle(4);
        byte
    }

    pub(super) fn read(&self, address: u16) -> u8 {
        match address {
            ROM_START..VRAM_START => self.cartridge.read_rom(address),
            VRAM_START..RAM_START => self.ppu.read_vram(address - VRAM_START),
            RAM_START..WRAM_START => self.cartridge.read_ram(address - RAM_START),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize],
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize],
            OAM_START..UNUSED_START => self.ppu.read_oam(address - OAM_START),

            REG_SB => self.serial.data(),
            REG_SC => self.serial.control(),

            REG_DIV => self.timer.divider(),
            REG_TIMA => self.timer.counter(),
            REG_TMA => self.timer.modulo(),
            REG_TAC => self.timer.control(),

            REG_IF => self.interrupt_flag | 0xE0,

            REG_LCDC => self.ppu.lcd_control(),
            REG_STAT => self.ppu.lcd_status(),
            REG_SCY => self.ppu.viewport_y(),
            REG_SCX => self.ppu.viewport_x(),
            REG_LY => self.ppu.lcd_y(),
            REG_LYC => self.ppu.ly_compare(),
            REG_DMA => self.ppu.dma_start(),
            REG_BGP => self.ppu.bg_palette(),
            REG_OBP0 => self.ppu.obj_palette_0(),
            REG_OBP1 => self.ppu.obj_palette_1(),
            REG_WY => self.ppu.window_y(),
            REG_WX => self.ppu.window_x(),

            HRAM_START..REG_IE => self.hram[(address - HRAM_START) as usize],
            REG_IE => self.interrupt_enable,

            _ => 0xFF
        }
    }

    pub(super) fn write_cycle(&mut self, address: u16, value: u8) {
        self.write(address, value);
        self.cycle(4);
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        match address {
            ROM_START..VRAM_START => self.cartridge.write_rom(address, value),
            VRAM_START..RAM_START => self.ppu.write_vram(address - VRAM_START, value),
            RAM_START..WRAM_START => self.cartridge.write_ram(address - RAM_START, value),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize] = value,
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize] = value,
            OAM_START..UNUSED_START => self.ppu.write_oam(address - OAM_START, value),

            REG_SB => self.serial.set_data(value),
            REG_SC => self.serial.set_control(value),

            REG_DIV => self.timer.set_divider(),
            REG_TIMA => self.timer.set_counter(value),
            REG_TMA => self.timer.set_modulo(value),
            REG_TAC => self.timer.set_control(value),

            REG_IF => self.interrupt_flag = value,

            REG_LCDC => self.ppu.set_lcd_control(value),
            REG_STAT => self.ppu.set_lcd_status(value),
            REG_SCY => self.ppu.set_viewport_y(value),
            REG_SCX => self.ppu.set_viewport_x(value),
            REG_LYC => self.ppu.set_ly_compare(value),
            REG_DMA => self.ppu.set_dma_start(value),
            REG_BGP => self.ppu.set_bg_palette(value),
            REG_OBP0 => self.ppu.set_obj_palette_0(value),
            REG_OBP1 => self.ppu.set_obj_palette_1(value),
            REG_WY => self.ppu.set_window_y(value),
            REG_WX => self.ppu.set_window_x(value),

            HRAM_START..REG_IE => self.hram[(address - HRAM_START) as usize] = value,
            REG_IE => self.interrupt_enable = value,

            _ => {}
        }
    }

    pub(super) fn cycle(&mut self, ticks: u32) {
        self.timer.cycle(ticks);

        if self.timer.interrupt() {
            self.interrupt_flag |= 0x04;
            self.timer.set_interrupt(false);
        }
    }

    pub(super) fn interrupt_enabled(&self) -> bool {
        (self.interrupt_enable & 0x1F) & (self.interrupt_flag & 0x1F) > 0x00
    }

    pub(super) fn interrupt_flag(&self, interrupt: Interrupt) -> bool {
        let interrupt = interrupt as u8;

        (self.interrupt_enable & interrupt) & (self.interrupt_flag & interrupt) > 0x00
    }

    pub(super) fn unflag_interrupt(&mut self, interrupt: Interrupt) {
        self.interrupt_flag &= !(interrupt as u8)
    }
}