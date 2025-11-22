#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum InterruptType {
    VBlank = 0b0000_0001,
    LCD = 0b0000_0010,
    Timer = 0b0000_0100,
    Serial = 0b0000_1000,
    Joypad = 0b0001_0000,
}

pub(super) struct Interrupts {
    interrupt_enable: u8,
    interrupt_flag: u8,
}

impl Interrupts {
    pub(super) fn new() -> Self {
        Self {
            interrupt_enable: 0x00,
            interrupt_flag: 0x00,
        }
    }

    pub(super) fn enabled(&self) -> u8 {
        self.interrupt_enable
    }

    pub(super) fn set_enabled(&mut self, enabled: u8) {
        self.interrupt_enable = enabled;
    }

    pub(super) fn flags(&self) -> u8 {
        self.interrupt_flag | 0xE0
    }

    pub(super) fn set_flags(&mut self, flags: u8) {
        self.interrupt_flag = flags;
    }

    pub(super) fn set_flag(&mut self, interrupt_type: InterruptType) {
        self.interrupt_flag |= interrupt_type as u8;
    }

    pub(super) fn interrupt_enabled(&self) -> bool {
        (self.interrupt_enable & 0x1F) & (self.interrupt_flag & 0x1F) > 0x00
    }

    pub(super) fn interrupt_flag(&self, interrupt: InterruptType) -> bool {
        let interrupt = interrupt as u8;

        (self.interrupt_enable & interrupt) & (self.interrupt_flag & interrupt) > 0x00
    }

    pub(super) fn unflag_interrupt(&mut self, interrupt: InterruptType) {
        self.interrupt_flag &= !(interrupt as u8)
    }
}
