pub struct Timer {
    divider: u8,
    counter: u8,
    modulo: u8,
    control: u8,
    enabled: bool,
    step_count: u32,
    system_clock: u32,
    pub timer_clock: u32,
    interrupt: bool
}

impl Timer {
    pub(super) fn new() -> Self {
        Self {
            divider: 0x00,
            counter: 0x00,
            modulo: 0x00,
            control: 0x00,
            enabled: false,
            step_count: 1024,
            system_clock: 0x00,
            timer_clock: 0x00,
            interrupt: false
        }
    }

    pub(super) fn divider(&self) -> u8 {
        self.divider
    }

    pub(super) fn set_divider(&mut self) {
        self.divider = 0x00;
        self.system_clock = 0x00;
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
        self.control = value & 0x07;

        self.enabled = self.control & 0x04 > 0x00;
        self.step_count = match self.control & 0x03 {
            0 => 1024,
            1 => 16,
            2 => 64,
            _ => 256
        };
    }

    pub(super) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(super) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    pub(super) fn cycle(&mut self, ticks: u32) {
        self.system_clock += ticks;

        while self.system_clock >= 256 {
            self.divider = self.divider.wrapping_add(1);
            self.system_clock -= 256;
        }

        if self.enabled {
            self.timer_clock += ticks;
            while self.timer_clock >= self.step_count {
                self.counter = self.counter.wrapping_add(1);

                if self.counter == 0 {
                    self.counter = self.modulo;
                    self.interrupt = true;
                }

                self.timer_clock -= self.step_count;
            }
        }
    }
}