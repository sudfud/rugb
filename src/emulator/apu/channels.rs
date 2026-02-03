
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ChannelType {
    Pulse1,
    Pulse2,
    Wave,
    Noise
}

pub(super) struct PulseChannel {
    dac_enabled: bool,

    duty_length_register: DutyLengthRegister,
    volume_register: ChannelVolumeRegister,
    control_register: ChannelControlRegister,

    envelope_counter: u8,
    current_volume: u8,
    envelope_direction: EnvelopeDirection,
    sweep_pace: u8,

    period_timer: u8,
    period_low: u8,
    current_period: u16,
    period_counter: u16,

    length_enabled: bool,
    length_counter: u8,

    duty_step: u8
}

impl PulseChannel {
    pub(super) fn new() -> Self {
        Self {
            dac_enabled: false,

            duty_length_register: DutyLengthRegister(0x3F),
            volume_register: ChannelVolumeRegister(0x00),
            control_register: ChannelControlRegister(0xBF),

            envelope_counter: 0,
            current_volume: 0,
            envelope_direction: EnvelopeDirection::Decreasing,
            sweep_pace: 0,

            period_timer: 0,
            period_low: 0xFF,
            current_period: 0,
            period_counter: 0,

            length_enabled: false,
            length_counter: 0,

            duty_step: 0
        }
    }

    pub(super) fn dac_enabled(&self) -> bool {
        self.dac_enabled
    }

    pub(super) fn duty_length(&self) -> u8 {
        self.duty_length_register.0 | 0x3F
    }

    pub(super) fn set_duty_length(&mut self, value: u8) {
        self.duty_length_register.0 = value;
    }

    pub(super) fn volume(&self) -> u8 {
        self.volume_register.0
    }

    pub(super) fn set_volume(&mut self, value: u8) {
        self.volume_register.0 = value;
        self.dac_enabled = value & 0xF8 != 0;
    }

    pub(super) fn set_period_low(&mut self, value: u8) {
        self.period_low = value;
    }

    pub(super) fn control(&self) -> u8 {
        self.control_register.0 & 0xBF
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control_register.0 = value;

        if !self.length_enabled && self.control_register.length_enabled() {
            self.length_enabled = true;
            self.length_counter = self.duty_length_register.initial_length_timer();
        } else if self.length_enabled && !self.control_register.length_enabled() {
            self.length_enabled = false;
        }
    }

    pub(super) fn can_trigger(&self) -> bool {
        self.control_register.trigger() && self.dac_enabled
    }

    pub(super) fn tick_period_divider(&mut self) {
        self.period_timer = self.period_timer.wrapping_add(1);

        if self.period_timer % 4 != 0 {
            return;
        }

        self.period_counter += 1;

        if self.period_counter > 0x7FF {
            let (p_low, p_high) = (self.period_low as u16, self.control_register.period_high() as u16);
            self.current_period = (p_high << 8) | p_low;
            self.period_counter = self.current_period;
            self.duty_step = (self.duty_step + 1) % 8;
        }
    }

    pub(super) fn tick_envelope(&mut self) {
        // Envelope is disabled if sweep pace is 0
        if self.sweep_pace == 0 {
            return;
        }

        self.envelope_counter += 1;

        if self.envelope_counter < self.sweep_pace {
            return;
        }

        self.current_volume = match self.envelope_direction {
            EnvelopeDirection::Decreasing => self.current_volume.saturating_sub(1),
            EnvelopeDirection::Increasing => u8::max(self.current_volume + 1, 15)
        };
    }

    pub(super) fn tick_length_timer(&mut self) {
        if !self.length_enabled {
            return;
        }

        self.length_counter += 1;
    }

    pub(super) fn length_timer_expired(&self) -> bool {
        self.length_enabled && self.length_counter >= 64
    }

    pub(super) fn trigger(&mut self) {
        self.current_volume = self.volume_register.initial_volume();
        self.envelope_direction = self.volume_register.envelope_direction();
        self.sweep_pace = self.volume_register.sweep_pace();
        self.envelope_counter = 0;
        self.period_timer = 0;

        let (p_low, p_high) = (self.period_low as u16, self.control_register.period_high() as u16);
        self.current_period = (p_high << 8) | p_low;
        self.period_counter = self.current_period;
    }

    pub(super) fn clear(&mut self) {
        self.duty_length_register.0 = 0;
        self.volume_register.0 = 0;
        self.control_register.0 = 0;
        self.dac_enabled = false;
        self.sweep_pace = 0;
    }
}

struct DutyLengthRegister(u8);

impl DutyLengthRegister {
    fn duty_cycle(&self) -> &[u8; 8] {
        const ONE_EIGHTH: [u8; 8] = [1, 1, 1, 1, 1, 1, 1, 0];
        const ONE_FOURTH: [u8; 8] = [0, 1, 1, 1, 1, 1, 1, 0];
        const ONE_HALF: [u8; 8] = [0, 1, 1, 1, 1, 0, 0, 0];
        const THREE_FOURTHS: [u8; 8] = [1, 0, 0, 0, 0, 0, 0, 1];

        match self.0 >> 6 {
            0 => &ONE_EIGHTH,
            1 => &ONE_FOURTH,
            2 => &ONE_HALF,
            _ => &THREE_FOURTHS
        }
    }

    fn initial_length_timer(&self) -> u8 {
        self.0 & 0x3F
    }
}

#[derive(Clone, Copy)]
enum EnvelopeDirection {
    Decreasing,
    Increasing
}

struct ChannelVolumeRegister(u8);

impl ChannelVolumeRegister {
    fn initial_volume(&self) -> u8 {
        self.0 >> 4
    }

    fn envelope_direction(&self) -> EnvelopeDirection {
        match (self.0 & 0x08) >> 3 {
            0 => EnvelopeDirection::Decreasing,
            _ => EnvelopeDirection::Increasing
        }
    }

    fn sweep_pace(&self) -> u8 {
        self.0 & 0x07
    }
}

struct ChannelControlRegister(u8);

impl ChannelControlRegister {
    fn trigger(&self) -> bool {
        self.0 & 0x80 > 0
    }

    fn length_enabled(&self) -> bool {
        self.0 & 0x40 > 0
    }

    fn period_high(&self) -> u8 {
        self.0 & 0x07
    }
}