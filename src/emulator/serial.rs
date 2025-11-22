pub(super) struct Serial {
    data: u8,
    control: u8,
}

impl Serial {
    pub(super) fn new() -> Self {
        Self {
            data: 0x00,
            control: 0x00,
        }
    }

    pub(super) fn data(&self) -> u8 {
        self.data
    }

    pub(super) fn set_data(&mut self, value: u8) {
        self.data = value;
    }

    pub(super) fn control(&self) -> u8 {
        self.control
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control = value;
    }
}
