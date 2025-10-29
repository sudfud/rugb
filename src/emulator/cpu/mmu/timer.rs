pub(super) struct Timer {
    divider: u8,
    counter: u8,
    modulo: u8,
    control: u8
}

impl Timer {
    pub(super) fn new() -> Self {
        Self {
            divider: 0x00,
            counter: 0x00,
            modulo: 0x00,
            control: 0x00,
        }
    }

    pub(super) fn divider(&self) -> u8 {
        self.divider
    }

    pub(super) fn set_divider(&mut self, value: u8) {
        self.divider = value;
    }

    pub(super) fn counter(&self) -> u8 {
        self.counter
    }

    pub(super) fn set_counter(&mut self, value: u8) {
        self.counter = value;
    }

    pub(super) fn modulo(&self) -> u8 {
        self.modulo
    }

    pub(super) fn set_modulo(&mut self, value: u8) {
        self.modulo = value
    }

    pub(super) fn control(&self) -> u8 {
        self.control
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control = value
    }
}