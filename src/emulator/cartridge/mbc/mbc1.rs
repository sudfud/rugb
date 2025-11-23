use super::super::CartridgeError;
use super::Mbc;

const ROM_BANK_SIZE: usize = 0x4000;
const RAM_BANK_SIZE: usize = 0x2000;

pub(in super::super) struct Mbc1 {
    rom: Vec<u8>,
    ram: Vec<u8>,
    rom_banks: usize,
    ram_banks: usize,
    rom_bank_number: usize,
    ram_bank_number: usize,
    bank_mode: BankMode,
    enable_external_ram: bool
}

impl Mbc1 {
    pub(in super::super) fn new(rom: Vec<u8>) -> Result<Self, CartridgeError> {
        let rom_banks = match rom.get(0x0148) {
            Some(rom_size) => match rom_size {
                0x00 => 2,
                0x01 => 4,
                0x02 => 8,
                0x03 => 16,
                0x04 => 32,
                0x05 => 64,
                0x06 => 128,
                _ => return Err(CartridgeError::InvalidCartridge)
            },
            None => return Err(CartridgeError::InvalidCartridge)
        };

        let ram_banks = match rom.get(0x0149) {
            Some(ram_size) => match ram_size {
                0x00 => 0,
                0x02 => 1,
                0x03 => 4,
                _ => return Err(CartridgeError::InvalidCartridge)
            },
            None => return Err(CartridgeError::InvalidCartridge)
        };

        Ok(
            Self {
                rom,
                ram: vec![0; ram_banks * RAM_BANK_SIZE],
                rom_banks,
                ram_banks,
                rom_bank_number: 0,
                ram_bank_number: 0,
                bank_mode: BankMode::Simple,
                enable_external_ram: false
            }
        )
    }
}

impl Mbc for Mbc1 {
    fn read_rom(&self, address: u16) -> u8 {
        let bank = if address < 0x4000 {
            match self.bank_mode {
                BankMode::Simple => 0,
                BankMode::Advanced => self.rom_bank_number & 0xE0
            }
        } else {
            self.rom_bank_number
        };

        let index = bank * ROM_BANK_SIZE + ((address as usize) & 0x3FFF);
        
        *self.rom.get(index).unwrap_or(&0xFF)
    }

    fn write_rom(&mut self, address: u16, value: u8) {
        match address {
            0x0000..0x2000 => self.enable_external_ram = value & 0x0A == 0x0A,
            0x2000..0x4000 => {
                let low_bits = match (value as usize) & 0x1F {
                    0 => 1,
                    n => n
                };

                self.rom_bank_number = ((self.rom_bank_number & 0x60) | low_bits) % self.rom_banks;
            },
            0x4000..0x6000 => {
                if self.rom_banks > 32 {
                    let high_bits = (value as usize & 0x03) % (self.rom_banks >> 5);
                    self.rom_bank_number = (self.rom_bank_number & 0x1F) | (high_bits << 5);
                }

                if self.ram_banks > 1 {
                    self.ram_bank_number = value as usize & 0x03;
                }
            },
            0x6000..0x8000 => if self.ram_banks > 1 || self.rom_banks > 32 {
                self.bank_mode = match value & 0x01 {
                    0 => BankMode::Simple,
                    _ => BankMode::Advanced
                };
            }
            _ => {}
        }
    }

    fn read_ram(&self, address: u16) -> u8 {
        if !self.enable_external_ram {
            return 0xFF;
        }

        let bank = match self.bank_mode {
            BankMode::Simple => 0,
            BankMode::Advanced => self.ram_bank_number
        };

        *self.ram.get(bank * RAM_BANK_SIZE + ((address & 0x1FFF) as usize)).unwrap_or(&0xFF)
    }

    fn write_ram(&mut self, address: u16, value: u8) {
        if !self.enable_external_ram {
            return;
        }

        let bank = match self.bank_mode {
            BankMode::Simple => 0,
            BankMode::Advanced => self.ram_bank_number
        };

        let index = bank * RAM_BANK_SIZE + ((address & 0x1FFF) as usize);

        if index < self.ram.len() {
            self.ram[index] = value;
        }
    }
}

enum BankMode {
    Simple = 0,
    Advanced = 1
}