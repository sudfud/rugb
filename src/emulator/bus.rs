use super::{Cartridge, HRAM, Interrupts, Joypad, PPU, Serial, Timer, WRAM};
use super::interrupts::InterruptType;

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

pub(super) struct Bus<'a> {
    pub(super) cartridge: &'a mut Cartridge,
    pub(super) hram: &'a mut HRAM,
    pub(super) interrupts: &'a mut Interrupts,
    pub(super) joypad: &'a mut Joypad,
    pub(super) ppu: &'a mut PPU,
    pub(super) serial: &'a mut Serial,
    pub(super) timer: &'a mut Timer,
    pub(super) wram: &'a mut WRAM
}

impl <'a> Bus<'a> {
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

            REG_IF => self.interrupts.flags(),

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
            REG_IE => self.interrupts.enabled(),

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

            REG_IF => self.interrupts.set_flags(value),

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
            REG_IE => self.interrupts.set_enabled(value),

            _ => {}
        }
    }

    pub(super) fn cycle(&mut self, ticks: u32) {
        self.timer.cycle(ticks);

        if self.timer.interrupt() {
            self.interrupts.set_flag(InterruptType::Timer);
            self.timer.set_interrupt(false);
        }

        for _ in 0..ticks {
            self.ppu.tick();
        }

        if self.ppu.interrupt() {
            self.interrupts.set_flag(InterruptType::LCD);
            self.ppu.set_interrupt(false);
        }
    }
}