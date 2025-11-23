mod mbc;

use std::fs::File;
use std::io::Read;
use std::path::Path;

use mbc::Mbc;
use mbc::mbc0::Mbc0;
use mbc::mbc1::Mbc1;

#[derive(Debug)]
pub(crate) enum CartridgeError {
    InvalidCartridge,
    Io(std::io::Error)
}

impl std::fmt::Display for CartridgeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidCartridge => String::from("Invalid cartridge"),
                Self::Io(e) => e.to_string(),
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
        let memory = std::fs::read(value).map_err(CartridgeError::Io)?;
        let mbc: Box<dyn Mbc> = match memory.get(0x0147) {
            Some(cart_type) => match cart_type {
                0x00 => Box::new(Mbc0::new(memory)),
                0x01..=0x02 => Box::new(Mbc1::new(memory)?),
                _ => return Err(CartridgeError::InvalidCartridge)
            },
            None => return Err(CartridgeError::InvalidCartridge)
        };

        Ok(Self { mbc })
    }
}
