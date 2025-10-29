pub(super) struct Joypad {
    data: u8
}

impl Joypad {
    pub(super) fn new() -> Self {
        Self {
            data: 0xFF
        }
    }

    pub(super) fn read(&self) -> u8 {
        self.data
    }

    pub(super) fn write(&mut self, value: u8) {
        self.data = (self.data & 0xCF) | (value & 0x30);
    }
}