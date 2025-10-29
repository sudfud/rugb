const VRAM_SIZE: usize = 0x2000;
const OAM_SIZE: usize = 0xA0;

pub(super) struct GPU {
    vram: [u8; VRAM_SIZE],
    oam: [u8; OAM_SIZE]
}

impl GPU {
    pub(super) fn new() -> Self {
        Self {
            vram: [0; VRAM_SIZE],
            oam: [0; OAM_SIZE]
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
}