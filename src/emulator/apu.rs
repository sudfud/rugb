use std::collections::VecDeque;

const FREQ_MAX: u16 = 2048;

pub(super) struct Apu {
    prev_div: u8,
    div_counter: u64,
    master_control: MasterControl,
    panning: Panning,
    master_volume: MasterVolume,
    channel_2: PulseChannel,
    samples: VecDeque<i16>
}

impl Apu {
    pub(super) fn new() -> Self {
        Self {
            prev_div: 0,
            div_counter: 0,
            master_control: MasterControl(0xF1),
            panning: Panning(0xF3),
            master_volume: MasterVolume(0x77),
            channel_2: PulseChannel {
                dac_enabled: false,
                length_control: LengthControl(0x3F),
                envelope_control: Envelope(0x00),
                period_low: 0xFF,
                channel_control: ChannelControl(0xBF),
                length_counter: 0x3F,
                period_counter: 4 * (FREQ_MAX - 0x07FF),
                envelope_counter: 0,
                current_volume: 0,
                current_direction: Direction::Decreasing,
                period: 0x07FF,
                phase: 0
            },
            samples: VecDeque::new()
        }
    }

    pub(super) fn tick(&mut self, div: u8) {
        if !self.master_control.audio_enabled() {
            self.samples.push_back(0);
            return;
        }

        if self.prev_div & 0x10 > 0 && div & 0x10 == 0 {
            self.div_counter += 1;

            if self.channel_2.channel_control.length_enabled() && self.div_counter % 2 == 0 {
                self.channel_2.tick_length_timer();

                if self.channel_2.length_counter == 0 {
                    self.master_control.set_channel_on(Channel::Pulse2, false);
                }
            }

            if self.div_counter % 8 == 0 {
                self.channel_2.tick_phase_timer();
            }
        }

        self.prev_div = div;

        let digital_sample = self.channel_2.sample();
    }

    pub(super) fn master_control(&self) -> u8 {
        self.master_control.0
    }

    pub(super) fn set_master_control(&mut self, value: u8) {
        self.master_control.0 |= value & 0x80;
    }

    pub(super) fn panning(&self) -> u8 {
        self.panning.0
    }

    pub(super) fn set_panning(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.panning.0 = value;
        }
    }

    pub(super) fn master_volume(&self) -> u8 {
        self.master_volume.0
    }

    pub(super) fn set_master_volume(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.master_volume.0 = value;
        }
    }

    pub(super) fn channel_2_length_control(&self) -> u8 {
        self.channel_2.length_control.0 & 0xC0
    }

    pub(super) fn set_channel_2_length_control(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.channel_2.length_control.0 = value;
        }
    }

    pub(super) fn channel_2_envelope(&self) -> u8 {
        self.channel_2.envelope_control.0
    }

    pub(super) fn set_channel_2_envelope(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.channel_2.envelope_control.0 = value;
            self.channel_2.dac_enabled = self.channel_2.envelope_control.0 & 0xF8 > 0;

            if !self.channel_2.dac_enabled {
                self.master_control.set_channel_on(Channel::Pulse2, false);
            }
        }
    }

    pub(super) fn channel_2_period_low(&self) -> u8 {
        self.channel_2.period_low as u8
    }

    pub(super) fn set_channel_2_period_low(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.channel_2.period_low = value as u16;
        }
    }

    pub(super) fn channel_2_control(&self) -> u8 {
        self.channel_2.channel_control.0 & 0x78
    }

    pub(super) fn set_channel_2_control(&mut self, value: u8) {
        if self.master_control.audio_enabled() {
            self.channel_2.channel_control.0 = value;

            if !self.master_control.channel_on(Channel::Pulse2) && self.channel_2.dac_enabled && self.channel_2.channel_control.triggered() {
                self.master_control.set_channel_on(Channel::Pulse2, true);
                self.channel_2.trigger();
            }
        }
    }
}

enum Channel {
    Pulse1 = 0,
    Pulse2 = 1,
    Wave = 2,
    Noise = 3
}

struct MasterControl(u8);

impl MasterControl {
    fn audio_enabled(&self) -> bool {
        self.0 & 0x80 > 0
    }

    fn set_audio_enabled(&mut self, enabled: bool) {
        if enabled {
            self.0 |= 0x80;
        } else {
            self.0 &= !0x80;
        }
    }

    fn channel_on(&self, channel: Channel) -> bool {
        self.0 & (1 << channel as u8) > 0
    }

    fn set_channel_on(&mut self, channel: Channel, on: bool) {
        if on {
            self.0 |= 1 << channel as u8;
        } else {
            self.0 &= !(1 << channel as u8);
        }
    }
}

struct Panning(u8);

impl Panning {
    fn left_enabled(&self, channel: Channel) -> bool {
        self.0 & 1 << (4 + channel as u8) > 0
    }

    fn set_left_enabled(&mut self, channel: Channel, enabled: bool) {
        if enabled {
            self.0 |= 1 << (4 + channel as u8);
        } else {
            self.0 &= !(1 << (4 + channel as u8));
        }
    }

    fn right_enabled(&self, channel: Channel) -> bool {
        self.0 & 1 << channel as u8 > 0
    }

    fn set_right_enabled(&mut self, channel: Channel, enabled: bool) {
        if enabled {
            self.0 |= 1 << channel as u8;
        } else {
            self.0 &= !(1 << channel as u8);
        }
    }
}

struct MasterVolume(u8);

impl MasterVolume {
    fn left(&self) -> u8 {
        (self.0 & 0x70) >> 4
    }

    fn right(&self) -> u8 {
        self.0 & 0x07
    }

    fn vin_left(&self) -> bool {
        self.0 & 0x80 > 0
    }

    fn vin_right(&self) -> bool {
        self.0 & 0x08 > 0
    }
}

#[derive(Clone, Copy, Default)]
enum DutyCycle {
    #[default]
    Eighth,
    Fourth,
    Half,
    ThreeFourths
}

impl DutyCycle {
    fn waveform_step(self, phase: u8) -> u8 {
        const EIGHTH_WAVEFORM: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 1];
        const FOURTH_WAVEFORM: [u8; 8] = [1, 0, 0, 0, 0, 0, 0, 1];
        const HALF_WAVEFORM: [u8; 8] = [1, 0, 0, 0, 0, 1, 1, 1];
        const THREE_FOURTH_WAVEFORM: [u8; 8] = [0, 1, 1, 1, 1, 1, 1, 0];

        let waveform = match self {
            Self::Eighth => EIGHTH_WAVEFORM,
            Self::Fourth => FOURTH_WAVEFORM,
            Self::Half => HALF_WAVEFORM,
            Self::ThreeFourths => THREE_FOURTH_WAVEFORM
        };

        waveform[phase as usize]
    }
}

struct LengthControl(u8);

impl LengthControl {
    fn duty_cycle(&self) -> DutyCycle {
        match self.0 >> 6 {
            0 => DutyCycle::Eighth,
            1 => DutyCycle::Fourth,
            2 => DutyCycle::Half,
            _ => DutyCycle::ThreeFourths
        }
    }

    fn length(&self) -> u8 {
        self.0 & 0x3F 
    }
}

#[derive(Clone, Copy, Default)]
enum Direction {
    #[default]
    Decreasing,
    Increasing
}

struct Envelope(u8);

impl Envelope {
    fn start_volume(&self) -> u8 {
        self.0 >> 4
    }

    fn direction(&self) -> Direction {
        match (self.0 & 0x08) >> 3 {
            0 => Direction::Decreasing,
            _ => Direction::Increasing
        }
    }

    fn pace(&self) -> u8 {
        self.0 & 0x07
    }
}

struct ChannelControl(u8);

impl ChannelControl {
    fn triggered(&self) -> bool {
        self.0 & 0x80 > 0
    }

    fn length_enabled(&self) -> bool {
        self.0 & 0x40 > 0
    }

    fn period_high(&self) -> u16 {
        (self.0 & 0x07) as u16
    }
}

struct PulseChannel {
    dac_enabled: bool,
    length_control: LengthControl,
    envelope_control: Envelope,
    channel_control: ChannelControl,
    period_low: u16,

    period: u16,
    length_counter: u8,
    period_counter: u16,
    envelope_counter: u8,
    current_volume: u8,
    current_direction: Direction,
    phase: u8
}

impl PulseChannel {
    fn tick_length_timer(&mut self) {
        self.length_counter -= 1;
    }

    fn tick_phase_timer(&mut self) {
        self.period_counter -= 1;

        if self.period_counter == 0 {
            self.phase = (self.phase + 1) % 8;
            self.period = (self.channel_control.period_high() << 8) | self.period_low;
            self.period_counter = 4 * (FREQ_MAX - self.period);
        }
    }

    fn trigger(&mut self) {
        if self.length_counter == 0 {
            self.length_counter = 64 - self.length_control.length();
        }

        self.period = (self.channel_control.period_high() << 8) | self.period_low;
        self.period_counter = 4 * (FREQ_MAX - self.period);
        self.envelope_counter = self.envelope_control.pace();
        self.current_volume = self.envelope_control.start_volume();
        self.current_direction = self.envelope_control.direction();
    }

    fn sample(&self) -> u8 {
        let waveform_step = self.length_control.duty_cycle().waveform_step(self.phase);
        waveform_step * self.current_volume
    }
}