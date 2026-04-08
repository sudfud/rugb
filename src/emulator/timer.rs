pub struct Timer {
    divider: u16,
    counter: u8,
    modulo: u8,
    control: TimerControl,
    overflow: bool,
    ignore_tima_write: bool,
    interrupt: bool,
    tick_count: u32,
    falling_edge_detector: FallingEdgeDetector
}

impl Timer {
    pub(super) fn new() -> Self {
        Self {
            divider: 0xAB,
            counter: 0x00,
            modulo: 0x00,
            control: TimerControl(0xF8),
            overflow: false,
            ignore_tima_write: false,
            interrupt: false,
            tick_count: 0,
            falling_edge_detector: FallingEdgeDetector::new()
        }
    }

    pub(super) fn divider(&self) -> u8 {
        (self.divider >> 6) as u8
    }

    pub(super) fn set_divider(&mut self) {
        self.divider = 0x00;
    }

    pub(super) fn counter(&self) -> u8 {
        self.counter
    }

    pub(super) fn set_counter(&mut self, value: u8) {
        // Don't handle overflow if TIMA is written to during the M-cycle after it occured
        if self.overflow {
            self.overflow = false;
            self.interrupt = false;
        }

        if !self.ignore_tima_write {
            self.counter = value;
        }
    }

    pub(super) fn modulo(&self) -> u8 {
        self.modulo
    }

    pub(super) fn set_modulo(&mut self, value: u8) {
        self.modulo = value;

        if self.ignore_tima_write {
            self.counter = value;
        }
    }

    pub(super) fn control(&self) -> u8 {
        self.control.0
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control.0 = value | 0xF8;
    }

    pub(super) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(super) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    pub(super) fn tick_count(&self) -> u32 {
        self.tick_count
    }

    pub(super) fn reset_tick_count(&mut self) {
        self.tick_count = 0;
    }

    pub(super) fn cycle(&mut self, t_cycles: u32) {
        self.tick_count += t_cycles;

        let m_cycles = t_cycles / 4;

        for _ in 0..m_cycles {
            self.divider = self.divider.wrapping_add(1);
            
            if self.overflow {
                self.counter = self.modulo;
                self.overflow = false;
                self.ignore_tima_write = true;
            } else if self.ignore_tima_write {
                self.ignore_tima_write = false;
            }

            let step = match self.control.frequency() {
                0 => self.divider & 0x80,
                1 => self.divider & 0x02,
                2 => self.divider & 0x08,
                _ => self.divider & 0x20   
            };

            if self.falling_edge_detector.detect(step > 0 && self.control.enabled()) {
                self.counter = self.counter.wrapping_add(1);

                // Handle overflow during the next M-cycle
                if self.counter == 0 {
                    self.overflow = true;
                    self.interrupt = true;
                }
            }
        }
    }
}

struct TimerControl(u8);

impl TimerControl {
    fn enabled(&self) -> bool {
        self.0 & 0x04 > 0
    }

    fn frequency(&self) -> u8 {
        self.0 & 0x03
    }
}

struct FallingEdgeDetector {
    prev: bool
}

impl FallingEdgeDetector {
    fn new() -> Self {
        Self {
            prev: false
        }
    }

    fn detect(&mut self, next: bool) -> bool {
        let result = self.prev && !next;
        self.prev = next;
        result
    }
}