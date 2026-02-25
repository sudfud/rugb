use blip_buf::BlipBuf;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum ChannelType {
    Pulse1,
    Pulse2,
    Wave,
    Noise
}

pub(super) struct PulseChannel {
    enabled: bool,
    dac_enabled: bool,

    duty_length_register: DutyLengthRegister,
    volume_register: ChannelVolumeRegister,
    control_register: ChannelControlRegister,
    sweep_control: Option<SweepControl>,

    envelope_counter: u8,
    current_volume: u8,
    envelope_direction: Direction,
    sweep_pace: u8,

    // period_timer: u32,
    period_low: u8,
    current_period: u16,
    period_counter: u16,

    length_enabled: bool,
    length_counter: u8,

    duty_step: usize,

    amplitude: i32,
    blip: BlipBuf
}

impl PulseChannel {
    pub(super) fn new(enabled: bool, with_sweep: bool, duty_length_reg: u8, volume_reg: u8) -> Self {
        let mut blip = BlipBuf::new(4000);
        blip.set_rates((1 << 22) as f64, 48000.0);

        let sweep_control = if with_sweep {
            Some(SweepControl::new())
        } else {
            None
        };

        Self {
            enabled,
            dac_enabled: false,

            duty_length_register: DutyLengthRegister(duty_length_reg),
            volume_register: ChannelVolumeRegister(volume_reg),
            control_register: ChannelControlRegister(0xBF),
            sweep_control,

            envelope_counter: 0,
            current_volume: 0,
            envelope_direction: Direction::Decreasing,
            sweep_pace: 0,

            period_low: 0xFF,
            current_period: 0,
            period_counter: 0,

            length_enabled: false,
            length_counter: 0,

            duty_step: 0,

            amplitude: 0,
            blip
        }
    }

    pub(super) fn enabled(&self) -> bool {
        self.enabled
    }

    pub(super) fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    pub(super) fn dac_enabled(&self) -> bool {
        self.dac_enabled
    }

    pub(super) fn sweep(&self) -> u8 {
        match self.sweep_control {
            Some(ref sweep_control) => sweep_control.register.0,
            None => 0xFF
        }
    }

    pub(super) fn set_sweep(&mut self, value: u8) {
        if let Some(ref mut sweep_control) = self.sweep_control {
            sweep_control.register.0 = 0x80 | value;
        }
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
        self.control_register.0 | 0xBF
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control_register.0 = value;

        if !self.length_enabled && self.control_register.length_enabled() {
            self.length_enabled = true;
            self.length_counter = self.duty_length_register.initial_length_timer();
        } else if self.length_enabled && !self.control_register.length_enabled() {
            self.length_enabled = false;
        }
        
        if self.control_register.trigger() && self.dac_enabled {
            self.enabled = true;
            self.trigger();
        }
    }

    pub(super) fn tick_period_divider(&mut self) {
        if !self.enabled {
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
        if !(self.enabled && self.sweep_pace > 0) {
            return;
        }

        self.envelope_counter += 1;

        if self.envelope_counter < self.sweep_pace {
            return;
        }

        self.envelope_counter = 0;
        self.current_volume = match self.envelope_direction {
            Direction::Decreasing => self.current_volume.saturating_sub(1),
            Direction::Increasing => u8::max(self.current_volume + 1, 15)
        };
    }

    pub(super) fn tick_length_timer(&mut self) {
        if !(self.enabled && self.length_enabled) {
            return;
        }

        self.length_counter += 1;

        if self.length_counter < 64 {
            return;
        }

        self.enabled = false;
    }

    pub(super) fn tick_sweep(&mut self) {
        match self.sweep_control {
            Some(ref mut sweep_control) => {
                if !sweep_control.enabled || sweep_control.current_pace == 0 {
                    return;
                }

                sweep_control.sweep_counter += 1;
                
                if sweep_control.sweep_counter < sweep_control.current_pace {
                    return;
                }

                sweep_control.sweep_counter = 0;

                let freq = sweep_control.calculate_frequency();

                if freq > 0x7FF {
                    self.enabled = false;
                    return;
                }

                sweep_control.freq_shadow = freq;

                self.period_low = freq as u8;
                self.control_register.set_period_high((freq >> 8) as u8);

                // Repeat frequency calculation/overflow check, but don't update frequency
                let freq = sweep_control.calculate_frequency();

                if freq > 0x7FF {
                    self.enabled = false;
                    return;
                }

                sweep_control.current_pace = sweep_control.register.pace();
            },
            None => return
        }
    }

    pub(super) fn update_buffer(&mut self, clock_time: u32) {
        let wave_step = self.duty_length_register.duty_cycle()[self.duty_step];
        let amplitude = (wave_step as i32 * 2 - 1) * self.current_volume as i32 * 100;

        self.blip.add_delta(clock_time, amplitude - self.amplitude);
        self.amplitude = amplitude;
    }

    pub(super) fn end_frame(&mut self, clock_duration: u32) {
        self.blip.end_frame(clock_duration);
    }

    pub(super) fn clear(&mut self) {
        self.duty_length_register.0 = 0;
        self.volume_register.0 = 0;
        self.control_register.0 = 0;
        self.dac_enabled = false;
        self.sweep_pace = 0;
    }

    pub(super) fn samples_available(&self) -> u32 {
        self.blip.samples_avail()
    }

    pub(super) fn collect_samples(&mut self, count: usize) -> Vec<i16> {
        let mut samples = vec![0; count];
        self.blip.read_samples(samples.as_mut_slice(), false);
        samples
    }

    pub(super) fn reset(&mut self) {
        self.duty_length_register.0 = 0;
        self.volume_register.0 = 0;
        self.period_low = 0;
        self.control_register.0 = 0x38;

        if let Some(ref mut sweep_control) = self.sweep_control {
            sweep_control.register.0 = 0x80;
        }
    }

    fn trigger(&mut self) {
        self.current_volume = self.volume_register.initial_volume();
        self.envelope_direction = self.volume_register.envelope_direction();
        self.sweep_pace = self.volume_register.sweep_pace();
        self.envelope_counter = 0;

        let (p_low, p_high) = (self.period_low as u16, self.control_register.period_high() as u16);
        self.current_period = (p_high << 8) | p_low;
        self.period_counter = self.current_period;

        if let Some(ref mut sweep_control) = self.sweep_control {
            sweep_control.freq_shadow = self.current_period;
            sweep_control.sweep_counter = 0;
            sweep_control.current_pace = sweep_control.register.pace();
            sweep_control.enabled = sweep_control.register.pace() > 0 || sweep_control.register.step() > 0;

            if sweep_control.register.step() == 0 {
                return;
            }

            let freq = sweep_control.calculate_frequency();

            if freq > 0x7FF {
                self.enabled = false;
            }
        }
    }
}

pub(super) struct WaveChannel {
    enabled: bool,
    dac_enabled: bool,

    blip: BlipBuf,

    wave_ram: [u8; 16],
    sample_index: usize,

    length_enabled: bool,
    initial_length_timer: u16,
    length_counter: u16,

    output_level: WaveOutputLevel,
    current_output_level: WaveOutputLevel,

    period_low: u8,
    current_period: u16,
    period_counter: u16,

    control_register: ChannelControlRegister,

    amplitude: i32
}

impl WaveChannel {
    pub(super) fn new() -> Self {
        let mut blip = BlipBuf::new(4000);
        blip.set_rates((1 << 22) as f64, 48000.0);

        Self {
            enabled: false,
            dac_enabled: false,

            blip,

            wave_ram: [0x00; 16],
            sample_index: 1,

            length_enabled: false,
            initial_length_timer: 0xFF,
            length_counter: 0xFF,

            output_level: WaveOutputLevel::Mute,
            current_output_level: WaveOutputLevel::Mute,

            period_low: 0xFF,
            current_period: 0x7FF,
            period_counter: 0x800,

            control_register: ChannelControlRegister(0xBF),

            amplitude: 0
        }
    }

    pub(super) fn enabled(&self) -> bool {
        self.enabled
    }

    pub(super) fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    pub(super) fn dac_enabled(&self) -> u8 {
        if self.dac_enabled {
            0xFF
        } else {
            0x7F
        }
    }

    pub(super) fn set_dac_enabled(&mut self, value: u8) {
        self.dac_enabled = value & 0x80 > 0;
        self.enabled = self.dac_enabled;
    }

    pub(super) fn wave_ram(&self, index: u16) -> u8 {
        self.wave_ram[index as usize]
    }

    pub(super) fn set_wave_ram(&mut self, index: u16, value: u8) {
        self.wave_ram[index as usize] = value;
    }

    pub(super) fn set_length_timer(&mut self, value: u8) {
        self.initial_length_timer = value as u16;
    }

    pub(super) fn output_level(&self) -> u8 {
        match self.output_level {
            WaveOutputLevel::Mute => 0x9F,
            WaveOutputLevel::Full => 0xBF,
            WaveOutputLevel::Half => 0xDF,
            WaveOutputLevel::Quarter => 0xFF
        }
    }

    pub(super) fn set_output_level(&mut self, value: u8) {
        self.output_level = match (value & 0x60) >> 5 {
            0 => WaveOutputLevel::Mute,
            1 => WaveOutputLevel::Full,
            2 => WaveOutputLevel::Half,
            _ => WaveOutputLevel::Quarter
        };
    }

    pub(super) fn set_period_low(&mut self, value: u8) {
        self.period_low = value;
    }

    pub(super) fn control(&self) -> u8 {
        self.control_register.0 | 0xBF
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control_register.0 = value;

        if !self.length_enabled && self.control_register.length_enabled() {
            self.length_enabled = true;
            self.length_counter = self.initial_length_timer;
        } else if self.length_enabled && !self.control_register.length_enabled() {
            self.length_enabled = false;
        }
        
        if self.control_register.trigger() && self.dac_enabled {
            self.enabled = true;
            self.trigger();
        }
    }

    pub(super) fn tick_period_divider(&mut self) {
        if !self.enabled {
            return;
        }

        self.period_counter += 1;

        if self.period_counter > 0x7FF {
            let (p_low, p_high) = (self.period_low as u16, self.control_register.period_high() as u16);
            self.current_period = (p_high << 8) | p_low;
            self.period_counter = self.current_period;
            self.sample_index = (self.sample_index + 1) % 32;
        }
    }

    pub(super) fn update_buffer(&mut self, clock_time: u32) {
        let mut sample = if self.sample_index & 0x01 == 0 {
            self.wave_ram[self.sample_index / 2] >> 4
        } else {
            self.wave_ram[self.sample_index / 2] & 0x0F
        };

        sample = match self.output_level {
            WaveOutputLevel::Mute => 0,
            WaveOutputLevel::Full => sample,
            WaveOutputLevel::Half => sample >> 1,
            WaveOutputLevel::Quarter => sample >> 2
        };

        let amplitude = sample as i32 * 200;

        self.blip.add_delta(clock_time, amplitude - self.amplitude);
        self.amplitude = amplitude;
    }

    pub(super) fn end_frame(&mut self, clock_duration: u32) {
        self.blip.end_frame(clock_duration);
    }

    pub(super) fn tick_length_timer(&mut self) {
        if !(self.enabled && self.length_enabled) {
            return;
        }

        self.length_counter += 1;

        if self.length_counter < 256 {
            return;
        }

        self.enabled = false;
    }

    pub(super) fn collect_samples(&mut self, count: usize) -> Vec<i16> {
        let mut samples = vec![0; count];
        self.blip.read_samples(&mut samples, false);
        samples
    }

    pub(super) fn reset(&mut self) {
        self.dac_enabled = false;
        self.initial_length_timer = 0;
        self.output_level = WaveOutputLevel::Mute;
        self.period_low = 0;
        self.control_register.0 = 0x38;
    }

    fn trigger(&mut self) {
        if self.length_counter >= 256 {
            self.length_counter = self.initial_length_timer;
        }

        let (p_low, p_high) = (self.period_low as u16, self.control_register.period_high() as u16);
        self.current_period = (p_high << 8) | p_low;
        self.current_output_level = self.output_level;
        self.period_counter = self.current_period;
        self.sample_index = 0;
    }
}

const DIVISORS: [u8; 8] = [8, 16, 32, 48, 64, 80, 96, 112];

pub(super) struct NoiseChannel {
    enabled: bool,
    dac_enabled: bool,

    blip: BlipBuf,

    length_enabled: bool,
    initial_length_timer: u8,
    length_counter: u8,

    volume_register: ChannelVolumeRegister,
    current_volume: u8,
    envelope_direction: Direction,
    envelope_counter: u8,
    sweep_pace: u8,

    randomness_register: ChannelRandomnessRegister,
    period_timer: u32,
    period_counter: u32,

    control_register: ChannelControlRegister,

    lfsr: Lfsr,

    amplitude: i32
}

impl NoiseChannel {
    pub(super) fn new() -> Self {
        let mut blip = BlipBuf::new(4000);
        blip.set_rates((1 << 22) as f64, 48000.0);

        Self {
            enabled: false,
            dac_enabled: false,

            blip,

            length_enabled: false,
            initial_length_timer: 0x3F,
            length_counter: 0x3F,

            volume_register: ChannelVolumeRegister(0x00),
            current_volume: 0,
            envelope_direction: Direction::Decreasing,
            envelope_counter: 0,
            sweep_pace: 0,

            randomness_register: ChannelRandomnessRegister(0x00),
            period_timer: 0,
            period_counter: 0,

            control_register: ChannelControlRegister(0xBF),

            lfsr: Lfsr(0),

            amplitude: 0
        }
    }

    pub(super) fn enabled(&self) -> bool {
        self.enabled
    }

    pub(super) fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    pub(super) fn dac_enabled(&self) -> bool {
        self.dac_enabled
    }

    pub(super) fn set_length_timer(&mut self, value: u8) {
        self.initial_length_timer = value & 0x3F;
    }

    pub(super) fn volume(&self) -> u8 {
        self.volume_register.0
    }

    pub(super) fn set_volume(&mut self, value: u8) {
        self.volume_register.0 = value;
        self.dac_enabled = value & 0xF8 != 0;
    }

    pub(super) fn randomness(&self) -> u8 {
        self.randomness_register.0
    }

    pub(super) fn set_randomness(&mut self, value: u8) {
        self.randomness_register.0 = value;
    }

    pub(super) fn control(&self) -> u8 {
        self.control_register.0 | 0xBF
    }

    pub(super) fn set_control(&mut self, value: u8) {
        self.control_register.0 = value;

        if !self.length_enabled && self.control_register.length_enabled() {
            self.length_enabled = true;
            self.length_counter = self.initial_length_timer & 0x3F;
        } else if self.length_enabled && !self.control_register.length_enabled() {
            self.length_enabled = false;
        }
        
        if self.control_register.trigger() && self.dac_enabled {
            self.enabled = true;
            self.trigger();
        }
    }

    pub(super) fn tick_length_timer(&mut self) {
        if !(self.enabled && self.length_enabled) {
            return;
        }

        self.length_counter += 1;

        if self.length_counter < 64 {
            return;
        }

        self.enabled = false;
    }

    pub(super) fn tick_period_divider(&mut self) {
        if !self.enabled {
            return;
        }

        self.period_counter += 1;

        if self.period_counter < self.period_timer {
            return;
        }

        self.period_counter = 0;
        self.period_timer = (DIVISORS[self.randomness_register.clock_divider() as usize] as u32) << self.randomness_register.clock_shift();
        self.lfsr.shift(self.randomness_register.short_mode());
    }

    pub(super) fn tick_envelope(&mut self) {
        if !(self.enabled && self.sweep_pace > 0) {
            return;
        }

        self.envelope_counter += 1;

        if self.envelope_counter < self.sweep_pace {
            return;
        }

        self.envelope_counter = 0;
        self.current_volume = match self.envelope_direction {
            Direction::Decreasing => self.current_volume.saturating_sub(1),
            Direction::Increasing => u8::max(self.current_volume + 1, 15)
        };
    }

    pub(super) fn update_buffer(&mut self, clock_time: u32) {
        let amplitude = if self.lfsr.0 & 0x01 == 1 {
            self.current_volume as i32 * 200
        } else {
            0
        };

        self.blip.add_delta(clock_time, self.amplitude - amplitude);
        self.amplitude = amplitude
    }

    pub(super) fn end_frame(&mut self, clock_duration: u32) {
        self.blip.end_frame(clock_duration);
    }

    pub(super) fn collect_samples(&mut self, count: usize) -> Vec<i16> {
        let mut samples = vec![0; count];
        self.blip.read_samples(&mut samples, false);
        samples
    }

    pub(super) fn reset(&mut self) {
        self.initial_length_timer = 0xC0;
        self.volume_register.0 = 0;
        self.randomness_register.0 = 0;
        self.control_register.0 = 0x3F;
    }

    fn trigger(&mut self) {
        if self.length_counter >= 64 {
            self.length_counter = self.initial_length_timer & 0x3F;
        }

        self.current_volume = self.volume_register.initial_volume();
        self.envelope_direction = self.volume_register.envelope_direction();
        self.sweep_pace = self.volume_register.sweep_pace();
        self.envelope_counter = 0;
        self.lfsr.0 = 0;
        self.period_timer = (DIVISORS[self.randomness_register.clock_divider() as usize] as u32) << self.randomness_register.clock_shift();
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
enum Direction {
    Decreasing,
    Increasing
}

struct ChannelVolumeRegister(u8);

impl ChannelVolumeRegister {
    fn initial_volume(&self) -> u8 {
        self.0 >> 4
    }

    fn envelope_direction(&self) -> Direction {
        match (self.0 & 0x08) >> 3 {
            0 => Direction::Decreasing,
            _ => Direction::Increasing
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

    fn set_period_high(&mut self, value: u8) {
        self.0 = (self.0 & 0xF8) | (value & 0x07);
    }
}

struct ChannelSweepRegister(u8);

impl ChannelSweepRegister {
    fn pace(&self) -> u8 {
        (self.0 & 0x70) >> 4
    }

    fn direction(&self) -> Direction {
        match self.0 & 0x08 {
            0 => Direction::Increasing,
            _ => Direction::Decreasing
        }
    }

    fn step(&self) -> u8 {
        self.0 & 0x07
    }
}

struct SweepControl {
    enabled: bool,
    register: ChannelSweepRegister,
    sweep_counter: u8,
    freq_shadow: u16,
    current_pace: u8
}

impl SweepControl {
    fn new() -> Self {
        Self {
            enabled: false,
            register: ChannelSweepRegister(0x80),
            sweep_counter: 0,
            freq_shadow: 0,
            current_pace: 0
        }
    }

    fn calculate_frequency(&self) -> u16 {
        let offset = self.freq_shadow >> self.register.step();

        match self.register.direction() {
            Direction::Increasing => self.freq_shadow + offset,
            Direction::Decreasing => self.freq_shadow.wrapping_add_signed(-(offset as i16))
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum WaveOutputLevel {
    Mute,
    Full,
    Half,
    Quarter
}

struct Lfsr(u16);

impl Lfsr {
    fn shift(&mut self, short_mode: bool) {
        let bit_0 = self.0 & 0x01;
        let bit_1 = (self.0 & 0x02) >> 1;
        let result = !(bit_0 ^ bit_1);

        self.0 &= 0x7FFF;
        self.0 |= result << 15;

        if short_mode {
            self.0 &= 0xFF7F;
            self.0 |= result << 7;
        }

        self.0 >>= 1;
    }
}

struct ChannelRandomnessRegister(u8);

impl ChannelRandomnessRegister {
    fn clock_shift(&self) -> u8 {
        (self.0 & 0xF0) >> 4
    }

    fn short_mode(&self) -> bool {
        self.0 & 0x08 == 0
    }

    fn clock_divider(&self) -> u8 {
        self.0 & 0x07
    }
}