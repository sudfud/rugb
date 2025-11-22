mod mbc;

use std::fs::File;
use std::io::Read;
use std::path::Path;

use mbc::Mbc;
use mbc::mbc0::MBC0;

#[derive(Debug)]
pub(crate) enum CartridgeError {
    IO(std::io::Error),
}

impl std::fmt::Display for CartridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::IO(e) => e.to_string(),
            }
        )
    }
}

impl std::error::Error for CartridgeError {}

pub(super) struct Cartridge {
    mbc: Box<dyn Mbc>,
}

impl Cartridge {
    pub(super) fn read_rom(&self, address: u16) -> u8 {
        self.mbc.read_rom(address)
    }

    pub(super) fn write_rom(&mut self, address: u16, value: u8) {
        self.mbc.write_rom(address, value);
    }

    pub(super) fn read_ram(&self, address: u16) -> u8 {
        self.mbc.read_ram(address)
    }

    pub(super) fn write_ram(&mut self, address: u16, value: u8) {
        self.mbc.write_ram(address, value);
    }
}

impl TryFrom<&Path> for Cartridge {
    type Error = CartridgeError;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        let mut memory: Vec<u8> = vec![0; 0x1_0000];
        let mut file = File::open(value).map_err(CartridgeError::IO)?;

        file.read(&mut memory).map_err(CartridgeError::IO)?;

        Ok(Self {
            mbc: Box::new(MBC0::new(memory)),
        })
    }
}
