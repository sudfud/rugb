#[repr(u8)]
#[derive(Clone, Copy)]
pub(super) enum FlagMask {
    Carry = 0b0001_0000,
    HalfCarry = 0b0010_0000,
    Subtract = 0b0100_0000,
    Zero = 0b1000_0000,
}

pub(crate) struct Registers {
    pub(crate) a: u8,
    pub(crate) b: u8,
    pub(crate) c: u8,
    pub(crate) d: u8,
    pub(crate) e: u8,
    pub(crate) f: u8,
    pub(crate) h: u8,
    pub(crate) l: u8,
    pub(crate) pc: u16,
    pub(crate) sp: u16,
}

impl Registers {
    pub(super) fn new() -> Self {
        Self {
            a: 0x01,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            f: 0xB0,
            h: 0x01,
            l: 0x4D,
            pc: 0x0100,
            sp: 0xFFFE,
        }
    }

    pub(super) fn af(&self) -> u16 {
        u16::from_be_bytes([self.a, self.f])
    }

    pub(super) fn set_af(&mut self, af: u16) {
        let [a, f] = af.to_be_bytes();
        self.a = a;
        self.f = f & 0xF0;
    }

    pub(super) fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b, self.c])
    }

    pub(super) fn set_bc(&mut self, bc: u16) {
        let [b, c] = bc.to_be_bytes();
        self.b = b;
        self.c = c;
    }

    pub(super) fn de(&self) -> u16 {
        u16::from_be_bytes([self.d, self.e])
    }

    pub(super) fn set_de(&mut self, de: u16) {
        let [d, e] = de.to_be_bytes();
        self.d = d;
        self.e = e;
    }

    pub(in crate::emulator) fn hl(&self) -> u16 {
        u16::from_be_bytes([self.h, self.l])
    }

    pub(super) fn set_hl(&mut self, hl: u16) {
        let [h, l] = hl.to_be_bytes();
        self.h = h;
        self.l = l;
    }

    pub(super) fn hli(&mut self) -> u16 {
        let hl = self.hl();
        self.set_hl(hl.wrapping_add(1));
        hl
    }

    pub(super) fn hld(&mut self) -> u16 {
        let hl = self.hl();
        self.set_hl(hl.wrapping_sub(1));
        hl
    }

    pub(super) fn flag(&self, mask: FlagMask) -> bool {
        self.f & (mask as u8) > 0
    }

    pub(super) fn set_flag(&mut self, mask: FlagMask, set: bool) {
        if set {
            self.f |= mask as u8;
        } else {
            self.f &= !(mask as u8);
        }
    }

    pub(super) fn inc_pc(&mut self, count: u16) {
        self.pc = self.pc.wrapping_add(count);
    }

    pub(super) fn inc_sp(&mut self) {
        self.sp = self.sp.wrapping_add(1);
    }

    pub(super) fn dec_sp(&mut self) {
        self.sp = self.sp.wrapping_sub(1);
    }
}

impl std::fmt::Display for Registers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AF: {:04X}\nBC: {:04X}\nDE: {:04X}\nHL: {:04X}\nSP: {:04X}\nPC: {:04X}",
            self.af(),
            self.bc(),
            self.de(),
            self.hl(),
            self.sp,
            self.pc
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_16_bit_registers() {
        let mut registers = Registers::new();

        registers.set_af(0xABCD);
        assert_eq!(registers.af(), 0xABCD);
        assert_eq!(registers.a, 0xAB);
        assert_eq!(registers.f, 0xCD);

        registers.set_bc(0xABCD);
        assert_eq!(registers.bc(), 0xABCD);
        assert_eq!(registers.b, 0xAB);
        assert_eq!(registers.c, 0xCD);

        registers.set_de(0xABCD);
        assert_eq!(registers.de(), 0xABCD);
        assert_eq!(registers.d, 0xAB);
        assert_eq!(registers.e, 0xCD);

        registers.set_hl(0xABCD);
        assert_eq!(registers.hl(), 0xABCD);
        assert_eq!(registers.h, 0xAB);
        assert_eq!(registers.l, 0xCD);
    }

    #[test]
    fn test_hl_inc_dec() {
        let mut registers = Registers::new();

        registers.set_hl(0xABCD);

        let mut hl = registers.hli();

        assert_eq!(hl, 0xABCD);
        assert_eq!(registers.hl(), 0xABCE);

        hl = registers.hld();
        assert_eq!(hl, 0xABCE);
        assert_eq!(registers.hl(), 0xABCD);
    }

    #[test]
    fn test_flags() {
        let mut registers = Registers::new();

        registers.f = 0;

        registers.set_flag(FlagMask::Carry, true);
        assert!(registers.flag(FlagMask::Carry));
        registers.set_flag(FlagMask::Carry, false);
        assert!(!registers.flag(FlagMask::Carry));

        registers.set_flag(FlagMask::HalfCarry, true);
        assert!(registers.flag(FlagMask::HalfCarry));
        registers.set_flag(FlagMask::HalfCarry, false);
        assert!(!registers.flag(FlagMask::HalfCarry));

        registers.set_flag(FlagMask::Subtract, true);
        assert!(registers.flag(FlagMask::Subtract));
        registers.set_flag(FlagMask::Subtract, false);
        assert!(!registers.flag(FlagMask::Subtract));

        registers.set_flag(FlagMask::Zero, true);
        assert!(registers.flag(FlagMask::Zero));
        registers.set_flag(FlagMask::Zero, false);
        assert!(!registers.flag(FlagMask::Zero));
    }
}