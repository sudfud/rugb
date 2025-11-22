use super::bus::{ECHO_START, OAM_START, RAM_START, ROM_START, VRAM_START, WRAM_START};
use super::{Cartridge, Oam, Vram, Wram};

pub(super) struct Dma {
    state: DmaState,
    counter: u8,
    current_address: u16,
}

impl Dma {
    pub(super) fn new() -> Self {
        Self {
            state: DmaState::Idle,
            counter: 0,
            current_address: 0,
        }
    }

    pub(super) fn state(&self) -> DmaState {
        self.state
    }

    pub(super) fn start(&mut self, address: u16) {
        self.state = DmaState::Initializing;
        self.current_address = address;
    }

    pub(super) fn tick(&mut self, cartridge: &Cartridge, vram: &Vram, wram: &Wram, oam: &mut Oam) {
        match self.state {
            DmaState::Idle => {}
            DmaState::Initializing => {
                self.counter += 1;

                if self.counter < 4 {
                    return;
                }

                self.counter = 0;
                self.state = DmaState::Transferring(self.read(cartridge, vram, wram));
            }
            DmaState::Transferring(byte) => {
                self.counter += 1;

                if self.counter < 4 {
                    return;
                }

                self.counter = 0;

                let oam_address = 0xFE00 | (self.current_address & 0x00FF);
                self.write(oam_address, byte, oam);

                self.current_address += 1;

                if self.current_address & 0x00A0 == 0x00A0 {
                    self.state = DmaState::Idle;
                } else {
                    self.state = DmaState::Transferring(self.read(cartridge, vram, wram));
                }
            }
        }
    }

    fn read(&self, cartridge: &Cartridge, vram: &Vram, wram: &Wram) -> u8 {
        match self.current_address {
            ROM_START..VRAM_START => cartridge.read_rom(self.current_address),
            VRAM_START..RAM_START => vram[(self.current_address - VRAM_START) as usize],
            RAM_START..WRAM_START => cartridge.read_ram(self.current_address - RAM_START),
            WRAM_START..ECHO_START => wram[(self.current_address - WRAM_START) as usize],
            _ => 0xFF,
        }
    }

    fn write(&self, address: u16, byte: u8, oam: &mut Oam) {
        oam[(address - OAM_START) as usize] = byte;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum DmaState {
    Idle,
    Initializing,
    Transferring(u8),
}
