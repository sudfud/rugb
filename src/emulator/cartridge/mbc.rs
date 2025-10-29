pub(super) mod mbc0;

pub(super) trait MBC {
    fn read_rom(&self, address: u16) -> u8;
    fn write_rom(&mut self, address: u16, value: u8);
    fn read_ram(&self, address: u16) -> u8;
    fn write_ram(&self, address: u16, value: u8);
}