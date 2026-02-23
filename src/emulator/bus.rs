use super::dma::DmaState;
use super::interrupts::InterruptType;
use super::{Apu, Cartridge, Dma, Hram, Interrupts, Joypad, Oam, Ppu, Serial, Timer, Vram, Wram};

pub(super) const ROM_START: u16 = 0x0000;
pub(super) const VRAM_START: u16 = 0x8000;
pub(super) const RAM_START: u16 = 0xA000;
pub(super) const WRAM_START: u16 = 0xC000;
pub(super) const ECHO_START: u16 = 0xE000;
pub(super) const OAM_START: u16 = 0xFE00;
const UNUSED_START: u16 = 0xFEA0;
const IO_START: u16 = 0xFF00;
const HRAM_START: u16 = 0xFF80;

const REG_JOYP: u16 = 0xFF00;
const REG_SB: u16 = 0xFF01;
const REG_SC: u16 = 0xFF02;

const REG_DIV: u16 = 0xFF04;
const REG_TIMA: u16 = 0xFF05;
const REG_TMA: u16 = 0xFF06;
const REG_TAC: u16 = 0xFF07;

const REG_IF: u16 = 0xFF0F;

const REG_AUD1SWEEP: u16 = 0xFF10;
const REG_AUD1LEN: u16 = 0xFF11;
const REG_AUD1ENV: u16 = 0xFF12;
const REG_AUD1LOW: u16 = 0xFF13;
const REG_AUD1HIGH: u16 = 0xFF14;
const REG_AUD2LEN: u16 = 0xFF16;
const REG_AUD2ENV: u16 = 0xFF17;
const REG_AUD2LOW: u16 = 0xFF18;
const REG_AUD2HIGH: u16 = 0xFF19;
const REG_AUDVOL: u16 = 0xFF24;
const REG_AUDTERM: u16 = 0xFF25;
const REG_AUDENA: u16 = 0xFF26;

const WAVE_RAM_START: u16 = 0xFF30;

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
    pub(super) apu: &'a mut Apu,
    pub(super) cartridge: &'a mut Cartridge,
    pub(super) dma: &'a mut Dma,
    pub(super) hram: &'a mut Hram,
    pub(super) interrupts: &'a mut Interrupts,
    pub(super) joypad: &'a mut Joypad,
    pub(super) oam: &'a mut Oam,
    pub(super) ppu: &'a mut Ppu,
    pub(super) serial: &'a mut Serial,
    pub(super) timer: &'a mut Timer,
    pub(super) vram: &'a mut Vram,
    pub(super) wram: &'a mut Wram,
}

impl<'a> Bus<'a> {
    pub(super) fn read_cycle(&mut self, address: u16) -> u8 {
        let byte = self.read(address);
        self.cycle(4);
        byte
    }

    pub(super) fn read(&self, address: u16) -> u8 {
        if let DmaState::Transferring(byte) = self.dma.state()
            && address < IO_START
        {
            return byte;
        }

        match address {
            ROM_START..VRAM_START => self.cartridge.read_rom(address),
            VRAM_START..RAM_START => self.vram[(address - VRAM_START) as usize],
            RAM_START..WRAM_START => self.cartridge.read_ram(address - RAM_START),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize],
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize],
            OAM_START..UNUSED_START => self.oam[(address - OAM_START) as usize],

            REG_JOYP => self.joypad.read(),
            REG_SB => self.serial.data(),
            REG_SC => self.serial.control(),

            REG_DIV => self.timer.divider(),
            REG_TIMA => self.timer.counter(),
            REG_TMA => self.timer.modulo(),
            REG_TAC => self.timer.control(),

            REG_IF => self.interrupts.flags(),

            REG_AUD1SWEEP => self.apu.channel_1_sweep(),
            REG_AUD1LEN => self.apu.channel_1_duty_length(),
            REG_AUD1ENV => self.apu.channel_1_volume(),
            REG_AUD1HIGH => self.apu.channel_1_control(),

            REG_AUD2LEN => self.apu.channel_2_duty_length(),
            REG_AUD2ENV => self.apu.channel_2_volume(),
            REG_AUD2HIGH => self.apu.channel_2_control(),

            REG_AUDVOL => self.apu.master_volume(),
            REG_AUDTERM => self.apu.panning(),
            REG_AUDENA => self.apu.control(),
            WAVE_RAM_START..REG_LCDC => self.apu.wave_ram((address - WAVE_RAM_START) as usize),

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

            _ => 0xFF,
        }
    }

    pub(super) fn write_cycle(&mut self, address: u16, value: u8) {
        self.write(address, value);
        self.cycle(4);
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        if let DmaState::Transferring(_) = self.dma.state()
            && address < IO_START
        {
            return;
        }

        match address {
            ROM_START..VRAM_START => self.cartridge.write_rom(address, value),
            VRAM_START..RAM_START => self.vram[(address - VRAM_START) as usize] = value,
            RAM_START..WRAM_START => self.cartridge.write_ram(address - RAM_START, value),
            WRAM_START..ECHO_START => self.wram[(address - WRAM_START) as usize] = value,
            ECHO_START..OAM_START => self.wram[(address - ECHO_START) as usize] = value,
            OAM_START..UNUSED_START => self.oam[(address - OAM_START) as usize] = value,

            REG_JOYP => self.joypad.write(value),
            REG_SB => self.serial.set_data(value),
            REG_SC => self.serial.set_control(value),

            REG_DIV => self.timer.set_divider(),
            REG_TIMA => self.timer.set_counter(value),
            REG_TMA => self.timer.set_modulo(value),
            REG_TAC => self.timer.set_control(value),

            REG_IF => self.interrupts.set_flags(value),

            REG_AUD1SWEEP => self.apu.set_channel_1_sweep(value),
            REG_AUD1LEN => self.apu.set_channel_1_duty_length(value),
            REG_AUD1ENV => self.apu.set_channel_1_volume(value),
            REG_AUD1LOW => self.apu.set_channel_1_period_low(value),
            REG_AUD1HIGH => self.apu.set_channel_1_control(value),

            REG_AUD2LEN => self.apu.set_channel_2_duty_length(value),
            REG_AUD2ENV => self.apu.set_channel_2_volume(value),
            REG_AUD2LOW => self.apu.set_channel_2_period_low(value),
            REG_AUD2HIGH => self.apu.set_channel_2_control(value),
            
            REG_AUDVOL => self.apu.set_master_volume(value),
            REG_AUDTERM => self.apu.set_panning(value),
            REG_AUDENA => self.apu.set_control(value),
            WAVE_RAM_START..REG_LCDC => self.apu.set_wave_ram((address - WAVE_RAM_START) as usize, value),

            REG_LCDC => self.ppu.set_lcd_control(value),
            REG_STAT => self.ppu.set_lcd_status(value),
            REG_SCY => self.ppu.set_viewport_y(value),
            REG_SCX => self.ppu.set_viewport_x(value),
            REG_LYC => self.ppu.set_ly_compare(value),
            REG_DMA => {
                self.ppu.set_dma_start(value);
                self.dma.start((value as u16) << 8);
            }
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
            self.apu.tick(self.timer.divider());
            self.ppu.tick(self.vram, self.oam);
            self.dma
                .tick(self.cartridge, self.vram, self.wram, self.oam);
        }

        if self.ppu.lcd_interrupt() {
            self.interrupts.set_flag(InterruptType::Lcd);
            self.ppu.set_lcd_interrupt(false);
        }

        if self.ppu.vblank_interrupt() {
            self.interrupts.set_flag(InterruptType::VBlank);
            self.ppu.set_vblank_interrupt(false);
        }

        if self.joypad.interrupt() {
            self.interrupts.set_flag(InterruptType::Joypad);
            self.joypad.set_interrupt(false);
        }
    }
}
