#[repr(u8)]
pub(crate) enum ActionButton {
    A = 0,
    B = 1,
    Select = 2,
    Start = 3
}

#[repr(u8)]
pub(crate) enum DirectionButton {
    Right = 0,
    Left = 1,
    Up = 2,
    Down = 3
}

pub(super) struct Joypad {
    data: u8,
    action: u8,
    direction: u8,
    interrupt: bool
}

impl Joypad {
    pub(super) fn new() -> Self {
        Self {
            data: 0xFF,
            action: 0x0F,
            direction: 0x0F,
            interrupt: false
        }
    }

    pub(super) fn read(&self) -> u8 {
        self.data
    }

    pub(super) fn write(&mut self, value: u8) {
        self.data = (self.data & 0xCF) | (value & 0x30);
        self.update();
    }

    pub(super) fn interrupt(&self) -> bool {
        self.interrupt
    }

    pub(super) fn set_interrupt(&mut self, value: bool) {
        self.interrupt = value;
    }

    pub(super) fn set_action_button(&mut self, button: ActionButton, pressed: bool) {
        if pressed {
            self.action &= !(1 << button as u8);
        } else {
            self.action |= 1 << button as u8;
        }

        self.update();
    }

    pub(super) fn set_direction_button(&mut self, button: DirectionButton, pressed: bool) {
        if pressed {
            self.direction &= !(1 << button as u8);
        } else {
            self.direction |= 1 << button as u8;
        }

        self.update();
    }

    fn update(&mut self) {
        let old_data = self.data & 0x0F;
        let mut new_data = 0x0F;

        if self.data & 0x10 == 0 {
            new_data &= self.direction;
        }

        if self.data & 0x20 == 0 {
            new_data &= self.action;
        }

        if old_data == 0x0F && new_data != 0x0F {
            self.interrupt = true;
        }

        self.data = (self.data & 0xF0) | new_data;
    }
}