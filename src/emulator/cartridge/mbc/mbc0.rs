use super::Mbc;

pub(in super::super) struct MBC0 {
    memory: Vec<u8>,
}

impl MBC0 {
    pub(in super::super) fn new(memory: Vec<u8>) -> Self {
        Self { memory }
    }
}

impl Mbc for MBC0 {
    fn read_rom(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write_rom(&mut self, _address: u16, _value: u8) {}

    fn read_ram(&self, _address: u16) -> u8 {
        0x00
    }

    fn write_ram(&self, _address: u16, _value: u8) {}
}
