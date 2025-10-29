const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;

pub(super) struct GPU {
    vram: [u8; VRAM_SIZE],
    oam: [u8; OAM_SIZE],
    lcd_control: u8,
    lcd_status: u8,
    viewport_x: u8,
    viewport_y: u8,
    lcd_y: u8,
    ly_compare: u8,
    dma_start: u8,
    bg_palette: u8,
    obj_palette_0: u8,
    obj_palette_1: u8,
    window_x: u8,
    window_y: u8,
}

impl GPU {
    pub(super) fn new() -> Self {
        Self {
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE],
            lcd_control: 0x91,
            lcd_status: 0x00,
            viewport_x: 0x00,
            viewport_y: 0x00,
            lcd_y: 0x00,
            ly_compare: 0x00,
            dma_start: 0x00,
            bg_palette: 0xFC,
            obj_palette_0: 0xFF,
            obj_palette_1: 0xFF,
            window_x: 0x00,
            window_y: 0x00
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
        self.lcd_control
    }

    pub(super) fn set_lcd_control(&mut self, value: u8) {
        self.lcd_control = value;
    }

    pub(super) fn lcd_status(&self) -> u8 {
        self.lcd_status
    }

    pub(super) fn set_lcd_status(&mut self, value: u8) {
        self.lcd_status = (self.lcd_status & 0x07) | (value & 0xF8);
    }

    pub(super) fn viewport_x(&self) -> u8 {
        self.viewport_x
    }

    pub(super) fn set_viewport_x(&mut self, value: u8) {
        self.viewport_x = value;
    }

    pub(super) fn viewport_y(&self) -> u8 {
        self.viewport_y
    }

    pub(super) fn set_viewport_y(&mut self, value: u8) {
        self.viewport_y = value;
    }

    pub(super) fn lcd_y(&self) -> u8 {
        self.lcd_y
    }

    pub(super) fn ly_compare(&self) -> u8 {
        self.ly_compare
    }

    pub(super) fn set_ly_compare(&mut self, value: u8) {
        self.ly_compare = value;
    }

    pub(super) fn dma_start(&self) -> u8 {
        self.dma_start
    }

    pub(super) fn set_dma_start(&mut self, value: u8) {
        self.dma_start = value & 0xDF;
    }

    pub(super) fn bg_palette(&self) -> u8 {
        self.bg_palette
    }

    pub(super) fn set_bg_palette(&mut self, value: u8) {
        self.bg_palette = value;
    }

    pub(super) fn obj_palette_0(&self) -> u8 {
        self.obj_palette_0
    }

    pub(super) fn set_obj_palette_0(&mut self, value: u8) {
        self.obj_palette_0 = (self.obj_palette_0 & 0x03) | (value & 0xFC);
    }

    pub(super) fn obj_palette_1(&self) -> u8 {
        self.obj_palette_1
    }

    pub(super) fn set_obj_palette_1(&mut self, value: u8) {
        self.obj_palette_1 = (self.obj_palette_1 & 0x03) | (value & 0xFC);
    }

    pub(super) fn window_x(&self) -> u8 {
        self.window_x
    }

    pub(super) fn set_window_x(&mut self, value: u8) {
        self.window_x = value;
    }

    pub(super) fn window_y(&self) -> u8 {
        self.window_y
    }

    pub(super) fn set_window_y(&mut self, value: u8) {
        self.window_y = value;
    }
}