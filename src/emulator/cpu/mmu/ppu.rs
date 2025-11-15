use std::collections::VecDeque;

use sdl2::pixels::Color;

use crate::{FrameBuffer, SCREEN_WIDTH, SCREEN_HEIGHT};

const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;

const TILE_SIZE_PIXELS: u8 = 8;
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
    bg_fetcher: BackgroundFetcher,
    sprite_fetcher: SpriteFetcher,
    draw_state: DrawState,
    counter: u32,
    draw_time: u32,
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
            bg_fetcher: BackgroundFetcher::new(),
            sprite_fetcher: SpriteFetcher::new(),
            draw_state: DrawState::InitialLoad(0),
            counter: 0,
            draw_time: 0,
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

                if self.bg_fetcher.window_mode {
                    self.bg_fetcher.window_line_counter += 1;
                }

                self.bg_fetcher.window_mode = false;
                
                self.regs.lcd_status.set_ppu_mode(
                    if self.regs.lcd_y as usize >= SCREEN_HEIGHT {
                        self.wy_latch = false;
                        self.bg_fetcher.window_line_counter = 0;
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
                self.sprite_fetcher.fill_buffer(&self.oam, &self.regs);
                self.regs.lcd_status.set_ppu_mode(RenderMode::Draw);
                self.update_stat_interrupt();
                self.draw_state = DrawState::InitialLoad(0);
            },

            RenderMode::Draw => {
                self.draw_time += 1;
                self.bg_fetcher.tick(&self.vram, &self.regs);

                match self.draw_state {
                    DrawState::InitialLoad(mut time) => {
                        time += 1;

                        if time < 12 {
                            // Discard first pixel fetch
                            if !self.bg_fetcher.bg_queue.is_empty() {
                                self.bg_fetcher.bg_queue.clear();
                                self.bg_fetcher.x_position = 0;
                            }

                            self.draw_state = DrawState::InitialLoad(time);
                        }
                        else if self.regs.scroll_x % TILE_SIZE_PIXELS != 0 {
                            self.draw_state = DrawState::DiscardPixels(self.regs.scroll_x % TILE_SIZE_PIXELS);
                        }
                        else {
                            self.draw_state = DrawState::ShiftPixels;
                        }
                    },

                    DrawState::DiscardPixels(time_left) => if time_left > 1 {
                        self.bg_fetcher.bg_queue.pop_front();
                        self.draw_state = DrawState::DiscardPixels(time_left - 1);
                    } else {
                        self.draw_state = DrawState::ShiftPixels;
                    },

                    DrawState::ShiftPixels => {
                        if !self.bg_fetcher.window_mode && self.regs.lcd_control.window_enabled() && self.wy_latch && self.regs.lcd_x + 7 >= self.regs.window_x {
                            self.bg_fetcher.window_mode = true;
                            self.bg_fetcher.reset();
                            return;
                        }

                        // self.bg_fetcher.tick(&self.vram, &self.regs);
                        match self.bg_fetcher.bg_queue.pop_front() {
                            Some(pixel) => {
                                let row = self.regs.lcd_y as usize;
                                let column = self.regs.lcd_x as usize;
                                let pixel_address = (row * SCREEN_WIDTH * 3) + (column * 3);

                                // Convert pixel color to RGB
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

                                // Place each RGB channel into the frame buffer
                                self.frame_buffer.borrow_mut()[pixel_address] = r;
                                self.frame_buffer.borrow_mut()[pixel_address + 1] = g;
                                self.frame_buffer.borrow_mut()[pixel_address + 2] = b;

                                self.regs.lcd_x += 1;

                                // Move on to HBlank
                                if self.regs.lcd_x >= SCREEN_WIDTH as u8 {
                                    self.bg_fetcher.reset();

                                    self.regs.lcd_x = 0;
                                    self.regs.lcd_status.set_ppu_mode(RenderMode::HBlank);
                                    self.draw_time = 0;

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
    InitialLoad(u8),
    DiscardPixels(u8),
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

    fn sprite_size(&self) -> u8 {
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

struct BackgroundFetcher {
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

impl BackgroundFetcher {
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

    fn reset(&mut self) {
        self.x_position = 0;
        self.counter = 0;
        self.state = FetcherState::TileId;
        self.bg_queue.clear();
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum SpritePriority {
    Front = 0,
    Back = 0b1000_0000
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Palette {
    OBP0 = 0,
    OBP1 = 0b0001_0000
}

struct Sprite {
    oam_index: u8,
    x: u8,
    y: u8,
    tile_number: u8,
    flags: u8
}

impl Sprite {
    fn new(oam_index: u8, x: u8, y: u8, tile_number: u8, flags: u8) -> Self {
        Self {
            oam_index,
            x,
            y,
            tile_number,
            flags
        }
    }

    fn palette(&self) -> Palette {
        match self.flags & 0x10 {
            0 => Palette::OBP0,
            _ => Palette::OBP1
        }
    }

    fn flip_x(&self) -> bool {
        self.flags & 0x20 > 0
    }

    fn flip_y(&self) -> bool {
        self.flags & 0x40 > 0
    }

    fn priority(&self) -> SpritePriority {
        match self.flags & 0x80 {
            0 => SpritePriority::Front,
            _ => SpritePriority::Back
        }
    }
}

struct SpritePixel {
    color: PixelColor,
    palette: Palette,
    priority: SpritePriority
}

impl SpritePixel {
    fn new(color: PixelColor, palette: Palette, priority: SpritePriority) -> Self {
        Self {
            color,
            palette,
            priority
        }
    }
}

struct SpriteFetcher {
    counter: u8,
    tile_index: u8,
    tile_data_low: u8,
    tile_data_high: u8,
    state: FetcherState,
    sprite_buffer: Vec<Sprite>,
    current_sprite: Option<Sprite>,
    sprite_queue: VecDeque<SpritePixel>
}

impl SpriteFetcher {
    fn new() -> Self {
        Self {
            counter: 0,
            tile_index: 0,
            tile_data_low: 0,
            tile_data_high: 0,
            state: FetcherState::TileId,
            sprite_buffer: Vec::with_capacity(10),
            current_sprite: None,
            sprite_queue: VecDeque::new()
        }
    }

    fn tick(&mut self, vram: &[u8; VRAM_SIZE]) {
        if let Some(ref sprite) = self.current_sprite {
            match self.state {
                FetcherState::TileId => {
                    self.counter += 1;

                    if self.counter < 2 {
                        return;
                    }

                    self.counter = 0;
                    self.tile_index = sprite.tile_number;
                    self.state = FetcherState::TileDataLow;
                },

                FetcherState::TileDataLow => {
                    self.counter += 1;

                    if self.counter < 2 {
                        return;
                    }

                    self.counter = 0;
                    self.tile_data_low = vram[self.tile_index as usize * TILE_SIZE_BYTES];
                    self.state = FetcherState::TileDataHigh;
                },

                FetcherState::TileDataHigh => {
                    self.counter += 1;

                    if self.counter < 2 {
                        return;
                    }

                    self.counter = 0;
                    self.tile_data_high = vram[(self.tile_index as usize * TILE_SIZE_BYTES) + 1];
                    self.state = FetcherState::Push;
                },

                FetcherState::Push => {
                    let start_index = if sprite.x < TILE_SIZE_PIXELS {
                        TILE_SIZE_PIXELS - sprite.x
                    } else if !self.sprite_queue.is_empty() {
                        TILE_SIZE_PIXELS - self.sprite_queue.len() as u8
                    } else {
                        0
                    };

                    for bit in (start_index..TILE_SIZE_PIXELS).rev() {
                        let mask = 1 << bit;
                        let color_bit_low = (self.tile_data_low & mask) >> bit;
                        let color_bit_high = (self.tile_data_high & mask) >> bit;
                        let color = match (color_bit_high << 1) | color_bit_low {
                            0 => PixelColor::Zero,
                            1 => PixelColor::One,
                            2 => PixelColor::Two,
                            _ => PixelColor::Three
                        };

                        let sprite_pixel = SpritePixel {
                            color,
                            palette: sprite.palette(),
                            priority: sprite.priority()
                        };

                        self.sprite_queue.push_back(sprite_pixel);
                    }
                }
            }
        }
    }

    fn fill_buffer(&mut self, oam: &[u8; OAM_SIZE], regs: &Registers) {
        self.sprite_buffer.clear();

        // Loop through each OAM entry
        for i in (0..OAM_SIZE).step_by(4) {
            let sprite_y = oam[i];
            let line_y = regs.lcd_y + 16;
            let height = regs.lcd_control.sprite_size();

            // Move on to the next sprite if the current one is out of bounds
            if line_y < sprite_y || line_y >= sprite_y + height {
                continue;
            }

            let sprite_x = oam[i + 1];

            if sprite_x <= 0 {
                continue;
            }

            // Add this sprite to the buffer
            let oam_index = i as u8 / 4;
            let tile_number = oam[i + 2];
            let flags = oam[i + 3];
            let sprite = Sprite::new(oam_index, sprite_x, sprite_y, tile_number, flags);

            self.sprite_buffer.push(sprite);

            // Stop if the buffer is full
            if self.sprite_buffer.len() == 10 {
                break;
            }
        }

        // Sort buffer by X position, or by OAM index if two sprites have the same X
        self.sprite_buffer.sort_by(|a, b| {
            if a.x != b.x {
                b.x.cmp(&a.x)
            }
            else {
                b.oam_index.cmp(&a.oam_index)
            }
        });
    }

    fn sprite_hit(&mut self, lcd_x: u8) -> bool {
        self.current_sprite = self.sprite_buffer.pop_if(|spr| spr.x <= lcd_x + 8);
        self.current_sprite.is_some()
    }
}