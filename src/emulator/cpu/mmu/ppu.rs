use std::collections::VecDeque;

use sdl2::pixels::Color;

use crate::{FrameBuffer, SCREEN_WIDTH, SCREEN_HEIGHT};

const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;

const TILE_SIZE_PIXELS: u16 = 8;
const TILE_SIZE_BYTES: usize = 16;

const MAP0_START: usize = 0x1800;
const MAP1_START: usize = 0x1C00;

const SCANLINE_COUNT: u8 = 154;

const SCANLINE_TIME: u32 = 456;
const OAM_TIME: u32 = 80;

pub(super) struct PPU {
    vram: [u8; VRAM_SIZE],
    oam: [u8; OAM_SIZE],
    frame_buffer: FrameBuffer,
    regs: Registers,
    pixel_fetcher: PixelFetcher,
    draw_state: DrawState,
    counter: u32,
    stat_interrupt_line: bool,
    interrupt: bool,
    wy_latch: bool
}

impl PPU {
    pub(super) fn new(lcd: FrameBuffer) -> Self {
        Self {
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
            frame_buffer: lcd,
            regs: Registers::new(),
            pixel_fetcher: PixelFetcher::new(),
            draw_state: DrawState::InitialLoad(0),
            counter: 0,
            stat_interrupt_line: false,
            interrupt: false,
            wy_latch: false
        }
    }

    pub(super) fn read_vram(&self, address: u16) -> u8 {
        self.vram[address as usize]
    }

    pub(super) fn write_vram(&mut self, address: u16, value: u8) {
        self.vram[address as usize] = value;
    }

    pub(super) fn read_oam(&self, address: u16) -> u8 {
        self.oam[address as usize]
    }

    pub(super) fn write_oam(&mut self, address: u16, value: u8) {
        self.oam[address as usize] = value;
    }

    pub(super) fn lcd_control(&self) -> u8 {
        self.regs.lcd_control.0
    }

    pub(super) fn set_lcd_control(&mut self, value: u8) {
        self.regs.lcd_control.0 = value;
        self.check_wy_latch();
    }

    pub(super) fn lcd_status(&self) -> u8 {
        self.regs.lcd_status.0
    }

    pub(super) fn set_lcd_status(&mut self, value: u8) {
        self.regs.lcd_status.0 = (self.regs.lcd_status.0 & 0x07) | (value & 0xF8);
        self.update_stat_interrupt();
    }

    pub(super) fn viewport_x(&self) -> u8 {
        self.regs.scroll_x
    }

    pub(super) fn set_viewport_x(&mut self, value: u8) {
        self.regs.scroll_x = value;
    }

    pub(super) fn viewport_y(&self) -> u8 {
        self.regs.scroll_y
    }

    pub(super) fn set_viewport_y(&mut self, value: u8) {
        self.regs.scroll_y = value;
    }

    pub(super) fn lcd_y(&self) -> u8 {
        self.regs.lcd_y
    }

    pub(super) fn ly_compare(&self) -> u8 {
        self.regs.ly_compare
    }

    pub(super) fn set_ly_compare(&mut self, value: u8) {
        self.regs.ly_compare = value;
        self.regs.lcd_status.set_lyc_ly_equal(self.regs.lcd_y == self.regs.ly_compare);
        self.update_stat_interrupt();
    }

    pub(super) fn dma_start(&self) -> u8 {
        self.regs.dma_start
    }

    pub(super) fn set_dma_start(&mut self, value: u8) {
        self.regs.dma_start = value & 0xDF;
    }

    pub(super) fn bg_palette(&self) -> u8 {
        self.regs.bg_palette
    }

    pub(super) fn set_bg_palette(&mut self, value: u8) {
        self.regs.bg_palette = value;
    }

    pub(super) fn obj_palette_0(&self) -> u8 {
        self.regs.obj_palette_0
    }

    pub(super) fn set_obj_palette_0(&mut self, value: u8) {
        self.regs.obj_palette_0 = (self.regs.obj_palette_0 & 0x03) | (value & 0xFC);
    }

    pub(super) fn obj_palette_1(&self) -> u8 {
        self.regs.obj_palette_1
    }

    pub(super) fn set_obj_palette_1(&mut self, value: u8) {
        self.regs.obj_palette_1 = (self.regs.obj_palette_1 & 0x03) | (value & 0xFC);
    }

    pub(super) fn window_x(&self) -> u8 {
        self.regs.window_x
    }

    pub(super) fn set_window_x(&mut self, value: u8) {
        self.regs.window_x = value;
    }

    pub(super) fn window_y(&self) -> u8 {
        self.regs.window_y
    }

    pub(super) fn set_window_y(&mut self, value: u8) {
        self.regs.window_y = value;
        self.check_wy_latch();
    }

    pub(super) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(super) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    pub(super) fn tick(&mut self) {
        self.counter += 1;
        
        self.check_wy_latch();
        
        match self.regs.lcd_status.ppu_mode() {
            RenderMode::HBlank => if self.counter >= SCANLINE_TIME {
                self.counter -= SCANLINE_TIME;

                self.regs.lcd_y += 1;

                self.regs.lcd_status.set_lyc_ly_equal(self.regs.lcd_y == self.regs.ly_compare);

                if self.pixel_fetcher.window_mode {
                    self.pixel_fetcher.window_line_counter += 1;
                }

                self.pixel_fetcher.window_mode = false;
                
                self.regs.lcd_status.set_ppu_mode(
                    if self.regs.lcd_y as usize >= SCREEN_HEIGHT {
                        self.wy_latch = false;
                        self.pixel_fetcher.window_line_counter = 0;
                        RenderMode::VBlank
                    } else {
                        RenderMode::ScanOAM
                    }
                );

                self.update_stat_interrupt();
            },

            RenderMode::VBlank => if self.counter >= SCANLINE_TIME {
                self.counter -= SCANLINE_TIME;
                self.regs.lcd_y += 1;
                self.regs.lcd_status.set_lyc_ly_equal(self.regs.lcd_y == self.regs.ly_compare);

                if self.regs.lcd_y == 1 || self.regs.lcd_y >= SCANLINE_COUNT {
                    self.regs.lcd_y = 0;
                    self.regs.lcd_status.set_ppu_mode(RenderMode::ScanOAM);
                    self.update_stat_interrupt();
                }
            },

            // TODO: Implement OAM/Sprites
            RenderMode::ScanOAM => if self.counter >= OAM_TIME {
                self.regs.lcd_status.set_ppu_mode(RenderMode::Draw);
                self.update_stat_interrupt();
                self.draw_state = DrawState::InitialLoad(12);
            },

            RenderMode::Draw => {
                self.pixel_fetcher.tick(&self.vram, &self.regs);

                match self.draw_state {
                    DrawState::InitialLoad(time_left) => if time_left > 0 {
                        if !self.pixel_fetcher.bg_queue.is_empty() {
                            self.pixel_fetcher.bg_queue.clear();
                            self.pixel_fetcher.x_position = 0;
                        }

                        self.draw_state = DrawState::InitialLoad(time_left - 1);
                    } else {
                        self.draw_state = DrawState::DiscardPixels(self.regs.scroll_x as u32 % 8);
                    },

                    DrawState::DiscardPixels(time_left) => if time_left > 0 {
                        self.pixel_fetcher.bg_queue.pop_front();
                        self.draw_state = DrawState::DiscardPixels(time_left - 1);
                    } else {
                        self.draw_state = DrawState::ShiftPixels;
                    },

                    DrawState::ShiftPixels => {
                        self.pixel_fetcher.tick(&self.vram, &self.regs);
                        match self.pixel_fetcher.bg_queue.pop_front() {
                            Some(pixel) => {
                                let row = self.regs.lcd_y as usize;
                                let column = self.regs.lcd_x as usize;
                                let pixel_address = (row * SCREEN_WIDTH * 3) + (column * 3);
                                let (r, g, b) = if self.regs.lcd_control.bg_window_enabled() {
                                    match pixel {
                                        PixelColor::Zero => (255, 255, 255),
                                        PixelColor::One => (128, 128, 128),
                                        PixelColor::Two => (64, 64, 64),
                                        PixelColor::Three => (0, 0, 0)
                                    }
                                } else {
                                    (255, 255, 255)
                                };

                                self.frame_buffer.borrow_mut()[pixel_address] = r;
                                self.frame_buffer.borrow_mut()[pixel_address + 1] = g;
                                self.frame_buffer.borrow_mut()[pixel_address + 2] = b;

                                self.regs.lcd_x += 1;

                                if !self.pixel_fetcher.window_mode && self.regs.lcd_control.window_enabled() && self.wy_latch && self.regs.lcd_x + 7 >= self.regs.window_x {
                                    self.pixel_fetcher.window_mode = true;
                                    self.pixel_fetcher.x_position = 0;
                                    self.pixel_fetcher.counter = 0;
                                    self.pixel_fetcher.state = FetcherState::TileId;
                                    self.pixel_fetcher.bg_queue.clear();
                                }

                                if self.regs.lcd_x >= SCREEN_WIDTH as u8 {
                                    self.pixel_fetcher.x_position = 0;
                                    self.pixel_fetcher.counter = 0;
                                    self.pixel_fetcher.state = FetcherState::TileId;
                                    self.pixel_fetcher.bg_queue.clear();

                                    self.regs.lcd_x = 0;
                                    self.regs.lcd_status.set_ppu_mode(RenderMode::HBlank);

                                    self.update_stat_interrupt();
                                }
                            },
                            None => return
                        }
                    }
                }
            }
        }
    }

    fn update_stat_interrupt(&mut self) {
        let status = &self.regs.lcd_status;
        let prev = self.stat_interrupt_line;

        self.stat_interrupt_line = (status.lyc_select() && status.lyc_ly_equal())
            || (status.oam_scan_mode_select() && status.ppu_mode() == RenderMode::ScanOAM)
            || (status.vblank_mode_select() && status.ppu_mode() == RenderMode::VBlank)
            || (status.hblank_mode_select() && status.ppu_mode() == RenderMode::HBlank);

        if !prev && self.stat_interrupt_line {
            self.interrupt = true;
        }
    }

    fn check_wy_latch(&mut self) {
        if !self.wy_latch && self.regs.lcd_control.window_enabled() && self.regs.lcd_y == self.regs.window_y {
            self.wy_latch = true;
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum DrawState {
    InitialLoad(u32),
    DiscardPixels(u32),
    ShiftPixels
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum AddressMode {
    Signed,
    Unsigned
}

struct LcdControl(u8);

impl LcdControl {
    fn lcd_enabled(&self) -> bool {
        self.0 & 0x80 > 0x00
    }

    fn window_tile_map(&self) -> usize {
        if self.0 & 0x40 == 0x00 {
            MAP0_START
        }
        else {
            MAP1_START
        }
    }

    fn window_enabled(&self) -> bool {
        self.0 & 0x20 > 0 && self.bg_window_enabled()
    }

    fn address_mode(&self) -> AddressMode {
        if self.0 & 0x10 > 0 {
            AddressMode::Unsigned
        }
        else {
            AddressMode::Signed
        }
    }

    fn bg_tile_map(&self) -> usize {
        if self.0 & 0x08 == 0x00 {
            MAP0_START
        }
        else {
            MAP1_START
        }
    }

    fn sprite_size(&self) -> u16 {
        if self.0 & 0x04 == 0x00 {
            TILE_SIZE_PIXELS
        }
        else {
            TILE_SIZE_PIXELS * 2
        }
    }

    fn sprites_enabled(&self) -> bool {
        self.0 & 0x02 > 0x00
    }

    fn bg_window_enabled(&self) -> bool {
        self.0 & 0x01 > 0x00
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum RenderMode {
    HBlank = 0,
    VBlank = 1,
    ScanOAM = 2,
    Draw = 3
}

struct LcdStatus(u8);

impl LcdStatus {
    fn lyc_select(&self) -> bool {
        self.0 & 0x40 > 0x00
    }

    fn oam_scan_mode_select(&self) -> bool {
        self.0 & 0x20 > 0x00
    }

    fn vblank_mode_select(&self) -> bool {
        self.0 & 0x10 > 0x00
    }

    fn hblank_mode_select(&self) -> bool {
        self.0 & 0x08 > 0x00
    }

    fn lyc_ly_equal(&self) -> bool {
        self.0 & 0x04 > 0x00
    }

    fn set_lyc_ly_equal(&mut self, value: bool) {
        self.0 = self.0 & !0x04;

        if value {
            self.0 |= 0x04;
        }
    }

    fn ppu_mode(&self) -> RenderMode {
        match self.0 & 0x03 {
            0 => RenderMode::HBlank,
            1 => RenderMode::VBlank,
            2 => RenderMode::ScanOAM,
            _ => RenderMode::Draw
        }
    }

    fn set_ppu_mode(&mut self, mode: RenderMode) {
        self.0 &= 0xFC;
        self.0 |= mode as u8;
    }
}

struct Registers {
    lcd_control: LcdControl,
    lcd_status: LcdStatus,
    dma_start: u8,
    bg_palette: u8,
    obj_palette_0: u8,
    obj_palette_1: u8,
    scroll_x: u8,
    scroll_y: u8,
    window_x: u8,
    window_y: u8,
    lcd_y: u8,
    lcd_x: u8,
    ly_compare: u8,
}

impl Registers {
    fn new() -> Self {
        Self {
            lcd_control: LcdControl(0x91),
            lcd_status: LcdStatus(0x85),
            dma_start: 0x00,
            bg_palette: 0xFC,
            obj_palette_0: 0xFF,
            obj_palette_1: 0xFF,
            scroll_x: 0,
            scroll_y: 0,
            window_x: 0,
            window_y: 0,
            lcd_y: 0,
            lcd_x: 0,
            ly_compare: 0
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum PixelColor {
    Zero,
    One,
    Two,
    Three
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum FetcherState {
    TileId,
    TileDataLow,
    TileDataHigh,
    Push
}

struct PixelFetcher {
    window_mode: bool,
    window_line_counter: u8,
    counter: u8,
    tile_index: u8,
    tile_data_low: u8,
    tile_data_high: u8,
    state: FetcherState,
    x_position: u8,
    bg_queue: VecDeque<PixelColor>
}

impl PixelFetcher {
    fn new() -> Self {
        Self {
            window_mode: false,
            window_line_counter: 0,
            counter: 0,
            tile_index: 0,
            tile_data_low: 0,
            tile_data_high: 0,
            state: FetcherState::TileId,
            x_position: 0,
            bg_queue: VecDeque::new()
        }
    }

    fn tick(&mut self, vram: &[u8; VRAM_SIZE], regs: &Registers) {
        match self.state {
            FetcherState::TileId => {
                self.counter += 1;

                if self.counter < 2 {
                    return;
                }

                self.counter = 0;

                self.tile_index = if self.window_mode {
                    let window_address = regs.lcd_control.window_tile_map();
                    let x = self.x_position as usize;
                    let y = 32 * (self.window_line_counter as usize / 8);

                    vram[window_address + x + y]
                } else {
                    let bg_address = regs.lcd_control.bg_tile_map();
                    let x = self.x_position.wrapping_add(regs.scroll_x / 8) as usize & 0x1F;
                    let y = 32 * ((regs.lcd_y.wrapping_add(regs.scroll_y) & 0xFF) / 8) as usize;

                    vram[bg_address + ((x + y) & 0x03FF)]
                };
                
                self.state = FetcherState::TileDataLow;
            },

            FetcherState::TileDataLow => {
                self.counter += 1;

                if self.counter < 2 {
                    return;
                }

                self.counter = 0;

                self.tile_data_low = vram[self.tile_address(regs)];
                self.state = FetcherState::TileDataHigh;
            },

            FetcherState::TileDataHigh => {
                self.counter += 1;

                if self.counter < 2 {
                    return;
                }

                self.counter = 0;

                self.tile_data_high = vram[self.tile_address(regs) + 1];

                if self.bg_queue.is_empty() {
                    self.fill_bg_queue();
                    self.x_position += 1;
                    self.state = FetcherState::TileId;
                }
                else {
                    self.state = FetcherState::Push;
                }
            },

            FetcherState::Push => {
                if self.bg_queue.is_empty() {
                    self.fill_bg_queue();
                    self.x_position += 1;
                    self.state = FetcherState::TileId;
                }
            }
        }
    }

    fn tile_address(&self, regs: &Registers) -> usize {
        let address = match regs.lcd_control.address_mode() {
            AddressMode::Unsigned
                => TILE_SIZE_BYTES * self.tile_index as usize,
            AddressMode::Signed => {
                let signed_index = self.tile_index as i8 as isize;
                0x1000_usize.wrapping_add_signed(TILE_SIZE_BYTES as isize * signed_index)
            }
        };

        if self.window_mode {
            address + 2 * (self.window_line_counter as usize % 8)
        } else {
            address + 2 * (regs.lcd_y.wrapping_add(regs.scroll_y) as usize % 8)
        }
    }

    fn fill_bg_queue(&mut self) {
        for bit in (0_u8..=7).rev() {
            let mask = 1 << bit;
            let color_bit_low = (self.tile_data_low & mask) >> bit;
            let color_bit_high = (self.tile_data_high & mask) >> bit;
            let color = match (color_bit_high << 1) | color_bit_low {
                0 => PixelColor::Zero,
                1 => PixelColor::One,
                2 => PixelColor::Two,
                _ => PixelColor::Three
            };

            self.bg_queue.push_back(color);
        }
    }
}