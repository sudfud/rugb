mod channels;

use channels::{ChannelType, PulseChannel};

pub(super) struct Apu {
    enabled: bool,
    div: u8,
    div_apu_counter: u8,

    // control: AudioMasterControl,
    panning: Panning,
    master_volume: AudioMasterVolume,

    channel_2: PulseChannel,

    wave_ram: [u8; 16]
}

impl Apu {
    pub(super) fn new() -> Self {
        Self {
            enabled: true,
            div: 0, // Previous value of timer's DIV register
            div_apu_counter: 0,

            // control: AudioMasterControl(0xF1),
            panning: Panning(0xF3),
            master_volume: AudioMasterVolume(0x77),

            channel_2: PulseChannel::new(false),

            wave_ram: [0x00; 16]
        }
    }

    pub(super) fn control(&self) -> u8 {
        let mut data = 0x70;

        if self.enabled {
            data |= 0x80;
        }

        if self.channel_2.enabled() {
            data |= 0x02;
        }

        data
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.enabled = value & 0x80 > 0;

        if !self.enabled {
            self.panning.0 = 0x00;
            self.master_volume.0 = 0x00;
            self.channel_2.set_enabled(false);
        }
    }

    pub(super) fn panning(&self) -> u8 {
        self.panning.0
    }

    pub(super) fn set_panning(&mut self, value: u8) {
        if self.enabled {
            self.panning.0 = value;
        }
    }

    pub(super) fn master_volume(&self) -> u8 {
        self.master_volume.0
    }

    pub(super) fn set_master_volume(&mut self, value: u8) {
        if self.enabled {
            self.master_volume.0 = value;
        }
    }

    pub(super) fn channel_2_duty_length(&self) -> u8 {
        self.channel_2.duty_length()
    }

    pub(super) fn set_channel_2_duty_length(&mut self, value: u8) {
        if self.enabled {
            self.channel_2.set_duty_length(value);
        }
    }

    pub(super) fn channel_2_volume(&self) -> u8 {
        self.channel_2.volume()
    }

    pub(super) fn set_channel_2_volume(&mut self, value: u8) {
        self.channel_2.set_volume(value);

        if !self.channel_2.dac_enabled() {
            self.channel_2.set_enabled(false);
        }
    }

    pub(super) fn set_channel_2_period_low(&mut self, value: u8) {
        if self.enabled {
            self.channel_2.set_period_low(value);
        }
    }

    pub(super) fn channel_2_control(&self) -> u8 {
        self.channel_2.control()
    }

    pub(super) fn set_channel_2_control(&mut self, value: u8) {
        if !self.enabled {
            return;
        }

        self.channel_2.set_control(value);
    }

    pub(super) fn wave_ram(&self, index: usize) -> u8 {
        self.wave_ram[index]
    }

    pub(super) fn set_wave_ram(&mut self, index: usize, value: u8) {
        self.wave_ram[index] = value;
    }

    pub(super) fn tick(&mut self, div: u8) {
        if !self.enabled {
            return;
        }

        if self.div & 0x10 > 0 && div & 0x10 == 0 {
            self.div_apu_counter = self.div_apu_counter.wrapping_add(1);

            if self.div_apu_counter % 2 == 0 {
                // Tick channel sound lengths
                self.channel_2.tick_length_timer();
            }

            if self.div_apu_counter % 4 == 0 {
                // Tick CH1 frequency sweep
            }

            if self.div_apu_counter % 8 == 0 {
                // Tick channel envelopes
                self.channel_2.tick_envelope();
            }
        }

        self.div = div;
        self.channel_2.tick_period_divider();
    }
}

const AUDIO_ENABLE_FLAG: u8 = 0x80;
const VIN_LEFT_FLAG: u8 = 0x80;
const VIN_RIGHT_FLAG: u8 = 0x08;
const LEFT_VOLUME_FLAG: u8 = 0x70;
const RIGHT_VOLUME_FLAG: u8 = 0x07;

struct AudioMasterControl(u8);

impl AudioMasterControl {
    fn is_powered_on(&self) -> bool {
        self.0 & 0x80 > 0
    }

    fn is_channel_on(&self, channel: ChannelType) -> bool {
        self.0 & (0x01 << channel as u8) > 0
    }

    fn set_channel_on(&mut self, channel: ChannelType, on: bool) {
        let channel_flag = 0x01 << channel as u8;

        if on {
            self.0 |= channel_flag;
        } else {
            self.0 &= !channel_flag;
        }
    }
}

struct Panning(u8);

impl Panning {
    fn left(&self, channel: ChannelType) -> bool {
        self.0 & (0x10 << channel as u8) > 0
    }

    fn right(&self, channel: ChannelType) -> bool {
        self.0 & (0x01 << channel as u8) > 0
    }
}

struct AudioMasterVolume(u8);

impl AudioMasterVolume {
    fn vin_left(&self) -> bool {
        self.0 & VIN_LEFT_FLAG > 0
    }

    fn vin_right(&self) -> bool {
        self.0 & VIN_RIGHT_FLAG > 0
    }

    fn left(&self) -> u8 {
        (self.0 & LEFT_VOLUME_FLAG) >> 4
    }

    fn right(&self) -> u8 {
        self.0 & RIGHT_VOLUME_FLAG
    }
}