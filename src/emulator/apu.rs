mod channels;

use channels::{ChannelType, PulseChannel, WaveChannel};

pub(super) struct Apu {
    enabled: bool,
    div: u8,
    div_apu_counter: u8,

    panning: Panning,
    master_volume: AudioMasterVolume,

    period_counter: u32,

    channel_1: PulseChannel,
    channel_2: PulseChannel,
    channel_3: WaveChannel,

    samples: Vec<f32>
}

impl Apu {
    pub(super) fn new() -> Self {
        Self {
            enabled: true,
            div: 0, // Previous value of timer's DIV register
            div_apu_counter: 0,

            panning: Panning(0xF3),
            master_volume: AudioMasterVolume(0x77),

            period_counter: 0,

            channel_1: PulseChannel::new(true, true, 0xBF, 0xF3),
            channel_2: PulseChannel::new(false, false, 0x3F, 0x00),
            channel_3: WaveChannel::new(),

            samples: Vec::new()
        }
    }

    pub(super) fn enabled(&self) -> bool {
        self.enabled
    }

    pub(super) fn control(&self) -> u8 {
        let mut data = 0x70;

        if self.enabled {
            data |= 0x80;
        }

        if self.channel_1.enabled() {
            data |= 0x01;
        }

        if self.channel_2.enabled() {
            data |= 0x02;
        }

        if self.channel_3.enabled() {
            data |= 0x04;
        }

        data
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.enabled = value & 0x80 > 0;

        if !self.enabled {
            self.panning.0 = 0x00;
            self.master_volume.0 = 0x00;
            self.channel_1.set_enabled(false);
            self.channel_2.set_enabled(false);
            self.channel_3.set_enabled(false);
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

    pub(super) fn channel_1_sweep(&self) -> u8 {
        self.channel_1.sweep()
    }

    pub(super) fn set_channel_1_sweep(&mut self, value: u8) {
        if self.enabled {
            self.channel_1.set_sweep(value);
        }
    }

    pub(super) fn channel_1_duty_length(&self) -> u8 {
        self.channel_1.duty_length()
    }

    pub(super) fn set_channel_1_duty_length(&mut self, value: u8) {
        if self.enabled {
            self.channel_1.set_duty_length(value);
        }
    }

    pub(super) fn channel_1_volume(&self) -> u8 {
        self.channel_1.volume()
    }

    pub(super) fn set_channel_1_volume(&mut self, value: u8) {
        self.channel_1.set_volume(value);

        if !self.channel_1.dac_enabled() {
            self.channel_1.set_enabled(false);
        }
    }

    pub(super) fn set_channel_1_period_low(&mut self, value: u8) {
        if self.enabled {
            self.channel_1.set_period_low(value);
        }
    }

    pub(super) fn channel_1_control(&self) -> u8 {
        self.channel_1.control()
    }

    pub(super) fn set_channel_1_control(&mut self, value: u8) {
        if self.enabled {
            self.channel_1.set_control(value);
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
        if self.enabled {
            self.channel_2.set_control(value);
        }
    }

    pub(super) fn channel_3_dac_enabled(&self) -> u8 {
        self.channel_3.dac_enabled()
    }

    pub(super) fn set_channel_3_dac_enabled(&mut self, value: u8) {
        if self.enabled {
            self.channel_3.set_dac_enabled(value);
        }
    }

    pub(super) fn set_channel_3_length_timer(&mut self, value: u8) {
        if self.enabled {
            self.channel_3.set_length_timer(value);
        }
    }

    pub(super) fn channel_3_output_level(&self) -> u8 {
        self.channel_3.output_level()
    }

    pub(super) fn set_channel_3_output_level(&mut self, value: u8) {
        if self.enabled {
            self.channel_3.set_output_level(value);
        }
    }

    pub(super) fn set_channel_3_period_low(&mut self, value: u8) {
        if self.enabled {
            self.channel_3.set_period_low(value);
        }
    }

    pub(super) fn channel_3_control(&self) -> u8 {
        self.channel_3.control()
    }

    pub(super) fn set_channel_3_control(&mut self, value: u8) {
        if self.enabled {
            self.channel_3.set_control(value);
        }
    }

    pub(super) fn wave_ram(&self, index: u16) -> u8 {
        self.channel_3.wave_ram(index)
    }

    pub(super) fn set_wave_ram(&mut self, index: u16, value: u8) {
        if self.enabled {
            self.channel_3.set_wave_ram(index, value);
        }
    }

    pub(super) fn tick(&mut self, div: u8) {
        if !self.enabled {
            self.samples.push(0.0);
            return;
        }

        if self.div & 0x10 > 0 && div & 0x10 == 0 {
            self.div_apu_counter = self.div_apu_counter.wrapping_add(1);

            if self.div_apu_counter % 2 == 0 {
                // Tick channel sound lengths
                self.channel_1.tick_length_timer();
                self.channel_2.tick_length_timer();
                self.channel_3.tick_length_timer();
            }

            if self.div_apu_counter % 4 == 0 {
                // Tick CH1 frequency sweep
                self.channel_1.tick_sweep();
            }

            if self.div_apu_counter % 8 == 0 {
                // Tick channel envelopes
                self.channel_1.tick_envelope();
                self.channel_2.tick_envelope();
            }
        }

        self.div = div;

        self.period_counter += 1;

        if self.period_counter % 2 == 0 {
            self.channel_3.tick_period_divider();
            self.channel_3.update_buffer(self.period_counter);
        }

        if self.period_counter % 4 == 0 {
            self.channel_1.tick_period_divider();
            self.channel_2.tick_period_divider();
            self.channel_1.update_buffer(self.period_counter);
            self.channel_2.update_buffer(self.period_counter);
        }

        if self.period_counter >= 256 {
            self.channel_1.end_frame(self.period_counter);
            self.channel_2.end_frame(self.period_counter);
            self.channel_3.end_frame(self.period_counter);
            self.period_counter = 0;
        }
    }

    pub(super) fn samples_available(&self) -> u32 {
        self.channel_2.samples_available()
    }

    pub(super) fn collect_samples(&mut self, count: usize) -> Vec<i16> {
        let ch1_samples = self.channel_1.collect_samples(count);
        let ch2_samples = self.channel_2.collect_samples(count);
        let ch3_samples = self.channel_3.collect_samples(count);

        let mut mixed_samples: Vec<i16> = Vec::new();

        for i in 0..ch1_samples.len() {
            mixed_samples.push((ch1_samples[i] + ch2_samples[i] + ch3_samples[i]) / 3);
        }

        mixed_samples
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