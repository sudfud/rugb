mod mmu;
mod registers;

use std::cell::RefCell;
use std::rc::Rc;

use super::Cartridge;

use mmu::MMU;
use registers::{FlagMask, Registers};

#[derive(Debug)]
pub(super) enum CPUError {
    UnsupportedOpcode(u8, u16),
}

impl std::fmt::Display for CPUError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::UnsupportedOpcode(code, address) =>
                    format!("Unsupported opcode {:02X} at address {:04X}", code, address),
            }
        )
    }
}

pub(super) struct CPU {
    registers: Registers,
    mmu: MMU,
    interrupt_master_enable: bool,
    ei_timer: u8,
    di_timer: u8,
    halted: bool
}

impl CPU {
    pub(super) fn new(cartridge: Cartridge) -> Self {
        Self {
            registers: Registers::new(),
            mmu: MMU::new(cartridge),
            interrupt_master_enable: false,
            ei_timer: 0,
            di_timer: 0,
            halted: false
        }
    }

    pub(super) fn read(&self, address: u16) -> u8 {
        self.mmu.read(address)
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        self.mmu.write(address, value);
    }

    pub(super) fn execute(&mut self) -> Result<u32, CPUError> {
        self.update_ime();

        self.step()
    }

    fn step(&mut self) -> Result<u32, CPUError> {
        let opcode = self.next_byte();

        match opcode {
            // NOP
            0x00 => Ok(1),

            // LD BC, d16
            0x01 => {
                let word = self.next_word();
                self.registers.set_bc(word);
                Ok(3)
            },

            // LD (BC), A
            0x02 => {
                self.write(self.registers.bc(), self.registers.a);
                Ok(2)
            },

            // INC BC
            0x03 => {
                self.registers.set_bc(self.registers.bc().wrapping_add(1));
                Ok(2)
            }

            // INC B
            0x04 => {
                self.registers.b = self.inc_8(self.registers.b);
                Ok(1)
            },

            // DEC B
            0x05 => {
                self.registers.b = self.dec_8(self.registers.b);
                Ok(1)
            },

            // LD B, d8
            0x06 => {
                let byte = self.next_byte();
                self.registers.b = byte;
                Ok(2)
            },

            // RLCA
            0x07 => {
                self.registers.a = self.rotate_left(self.registers.a, false, false);
                Ok(1)
            },

            // LD (a16), SP
            0x08 => {
                let address = self.next_word();
                let [lower, upper] = self.registers.sp.to_le_bytes();

                self.mmu.write(address, lower);
                self.mmu.write(address.wrapping_add(1), upper);

                Ok(5)
            },

            // ADD HL, BC
            0x09 => {
                self.add_to_hl(self.registers.bc());
                Ok(2)
            },

            // LD A, (BC)
            0x0A => {
                self.registers.a = self.read(self.registers.bc());
                Ok(2)
            },

            // DEC BC
            0x0B => {
                self.registers.set_bc(self.registers.bc().wrapping_sub(1));
                Ok(2)
            },

            // INC C
            0x0C => {
                self.registers.c = self.inc_8(self.registers.c);
                Ok(1)
            },

            // DEC C
            0x0D => {
                self.registers.c = self.dec_8(self.registers.c);
                Ok(1)
            },

            // LD C, d8
            0x0E => {
                let byte = self.next_byte();
                self.registers.c = byte;
                Ok(2)
            },

            // RRCA
            0x0F => {
                self.registers.a = self.rotate_right(self.registers.a, false, false);
                Ok(1)
            },

            // STOP
            0x10 => {
                self.next_byte();
                Ok(1)
            },

            // LD DE, d16
            0x11 => {
                let word = self.next_word();
                self.registers.set_de(word);
                Ok(3)
            },

            // LD (DE), A
            0x12 => {
                self.write(self.registers.de(), self.registers.a);
                Ok(2)
            },

            // INC DE
            0x13 => {
                self.registers.set_de(self.registers.de().wrapping_add(1));
                Ok(2)
            },

            // INC D
            0x14 => {
                self.registers.d = self.inc_8(self.registers.d);
                Ok(1)
            },

            // DEC D
            0x15 => {
                self.registers.d = self.dec_8(self.registers.d);
                Ok(1)
            },

            // LD D, d8
            0x16 => {
                let byte = self.next_byte();
                self.registers.d = byte;
                Ok(2)
            },

            // RLA
            0x17 => {
                self.registers.a = self.rotate_left(self.registers.a, true, false);
                Ok(1)
            },

            // JR s8
            0x18 => if self.jump_relative(Condition::None) {
                Ok(3)
            } else {
                Ok(2)
            },

            // ADD HL, DE
            0x19 => {
                self.add_to_hl(self.registers.de());
                Ok(2)
            },

            // LD A, (DE)
            0x1A => {
                self.registers.a = self.read(self.registers.de());
                Ok(2)
            },

            // DEC DE
            0x1B => {
                self.registers.set_de(self.registers.de().wrapping_sub(1));
                Ok(2)
            },

            // INC E
            0x1C => {
                self.registers.e = self.inc_8(self.registers.e);
                Ok(1)
            },

            // DEC E
            0x1D => {
                self.registers.e = self.dec_8(self.registers.e);
                Ok(1)
            },

            // LD E, d8
            0x1E => {
                let byte = self.next_byte();
                self.registers.e = byte;
                Ok(2)
            },

            // RRA
            0x1F => {
                self.registers.a = self.rotate_right(self.registers.a, true, false);
                Ok(1)
            },

            // JR NZ, s8
            0x20 => if self.jump_relative(Condition::NotZero) {
                Ok(3)
            } else {
                Ok(2)
            },

            // LD HL, d16
            0x21 => {
                let word = self.next_word();
                self.registers.set_hl(word);
                Ok(3)
            },

            // LD (HL+), A
            0x22 => {
                let address = self.registers.hli();
                self.write(address, self.registers.a);
                Ok(2)
            },

            // INC HL
            0x23 => {
                self.registers.set_hl(self.registers.hl().wrapping_add(1));
                Ok(2)
            },

            // INC H
            0x24 => {
                self.registers.h = self.inc_8(self.registers.h);
                Ok(1)
            },

            // DEC H
            0x25 => {
                self.registers.h = self.dec_8(self.registers.h);
                Ok(1)
            },

            // LD H, d8
            0x26 => {
                let byte = self.next_byte();
                self.registers.h = byte;
                Ok(2)
            },

            // DAA
            0x27 => {
                let mut a = self.registers.a;
                let mut adjust = if self.registers.flag(FlagMask::Carry) { 0x60 } else { 0x00 };

                if self.registers.flag(FlagMask::HalfCarry) {
                    adjust |= 0x06;
                }

                if !self.registers.flag(FlagMask::Subtract) {
                    if a & 0x0F > 0x09 {
                        adjust |= 0x06;
                    }

                    if a > 0x99 {
                        adjust |= 0x60;
                    }

                    a = a.wrapping_add(adjust);
                }
                else {
                    a = a.wrapping_sub(adjust);
                }

                self.registers.set_flag(FlagMask::Zero, a == 0);
                self.registers.set_flag(FlagMask::HalfCarry, false);
                self.registers.set_flag(FlagMask::Carry, adjust >= 0x60);

                self.registers.a = a;

                Ok(1)
            },

            // JR Z, s8
            0x28 => if self.jump_relative(Condition::Zero) {
                Ok(3)
            } else {
                Ok(2)
            },

            // ADD HL, HL
            0x29 => {
                self.add_to_hl(self.registers.hl());
                Ok(2)
            },

            // LD A, (HL+)
            0x2A => {
                let address = self.registers.hli();
                self.registers.a = self.read(address);
                Ok(2)
            },

            // DEC HL
            0x2B => {
                self.registers.set_hl(self.registers.hl().wrapping_sub(1));
                Ok(2)
            },

            // INC L
            0x2C => {
                self.registers.l = self.inc_8(self.registers.l);
                Ok(1)
            },

            // DEC L
            0x2D => {
                self.registers.l = self.dec_8(self.registers.l);
                Ok(1)
            },

            // LD L, d8
            0x2E => {
                let byte = self.next_byte();
                self.registers.l = byte;
                Ok(2)
            },

            // CPL
            0x2F => {
                self.registers.a = !self.registers.a;
                self.registers.set_flag(FlagMask::Subtract, true);
                self.registers.set_flag(FlagMask::HalfCarry, true);
                Ok(1)
            },

            // JR NC, s8
            0x30 => if self.jump_relative(Condition::NotCarry) {
                Ok(3)
            } else {
                Ok(2)
            },

            // LD SP, d16
            0x31 => {
                self.registers.sp = self.next_word();
                Ok(3)
            },

            // LD (HL-), A
            0x32 => {
                let address = self.registers.hld();
                self.write(address, self.registers.a);
                Ok(2)
            },

            // INC SP
            0x33 => {
                self.registers.sp = self.registers.sp.wrapping_add(1);
                Ok(2)
            },

            // INC (HL)
            0x34 => {
                let byte = self.inc_8(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                Ok(3)
            },

            // DEC (HL)
            0x35 => {
                let byte = self.dec_8(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                Ok(3)
            },

            // LD (HL), d8
            0x36 => {
                let byte = self.next_byte();
                self.write(self.registers.hl(), byte);
                Ok(3)
            },

            // SCF
            0x37 => {
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, false);
                self.registers.set_flag(FlagMask::Carry, true);
                Ok(1)
            },

            // JR C, s8
            0x38 => if self.jump_relative(Condition::Carry) {
                Ok(3)
            } else {
                Ok(2)
            },

            // ADD HL, SP
            0x39 => {
                self.add_to_hl(self.registers.sp);
                Ok(2)
            },

            // LD A, (HL-)
            0x3A => {
                let address = self.registers.hld();
                self.registers.a = self.read(address);
                Ok(2)
            },

            // DEC SP
            0x3B => {
                self.registers.sp = self.registers.sp.wrapping_sub(1);
                Ok(2)
            },

            // INC A
            0x3C => {
                self.registers.a = self.inc_8(self.registers.a);
                Ok(1)
            },

            // DEC A
            0x3D => {
                self.registers.a = self.dec_8(self.registers.a);
                Ok(1)
            },

            // LD A, d8
            0x3E => {
                let byte = self.next_byte();
                self.registers.a = byte;
                Ok(2)
            },

            // CCF
            0x3F => {
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, false);
                self.registers.set_flag(FlagMask::Carry, !self.registers.flag(FlagMask::Carry));
                Ok(1)
            },

            // LD B, B
            0x40 => {
                self.registers.b = self.registers.b;
                Ok(1)
            },

            // LD B, C
            0x41 => {
                self.registers.b = self.registers.c;
                Ok(1)
            },

            // LD B, D
            0x42 => {
                self.registers.b = self.registers.d;
                Ok(1)
            },

            // LD B, E
            0x43 => {
                self.registers.b = self.registers.e;
                Ok(1)
            },

            // LD B, H
            0x44 => {
                self.registers.b = self.registers.h;
                Ok(1)
            },

            // LD B, L
            0x45 => {
                self.registers.b = self.registers.l;
                Ok(1)
            },

            // LD B, (HL)
            0x46 => {
                self.registers.b = self.read(self.registers.hl());
                Ok(2)
            },

            // LD B, A
            0x47 => {
                self.registers.b = self.registers.a;
                Ok(1)
            },

            // LD C, B
            0x48 => {
                self.registers.c = self.registers.b;
                Ok(1)
            },

            // LD C, C
            0x49 => {
                self.registers.c = self.registers.c;
                Ok(1)
            },

            // LD C, D
            0x4A => {
                self.registers.c = self.registers.d;
                Ok(1)
            },

            // LD C, E
            0x4B => {
                self.registers.c = self.registers.e;
                Ok(1)
            },

            // LD C, H
            0x4C => {
                self.registers.c = self.registers.h;
                Ok(1)
            },

            // LD C, L
            0x4D => {
                self.registers.c = self.registers.l;
                Ok(1)
            },

            // LD C, (HL)
            0x4E => {
                self.registers.c = self.read(self.registers.hl());
                Ok(2)
            },

            // LD C, A
            0x4F => {
                self.registers.c = self.registers.a;
                Ok(1)
            },

            // LD D, B
            0x50 => {
                self.registers.d = self.registers.b;
                Ok(1)
            },

            // LD D, C
            0x51 => {
                self.registers.d = self.registers.c;
                Ok(1)
            },

            // LD D, D
            0x52 => {
                self.registers.d = self.registers.d;
                Ok(1)
            },

            // LD D, E
            0x53 => {
                self.registers.d = self.registers.e;
                Ok(1)
            },

            // LD D, H
            0x54 => {
                self.registers.d = self.registers.h;
                Ok(1)
            },

            // LD D, L
            0x55 => {
                self.registers.d = self.registers.l;
                Ok(1)
            },

            // LD D, (HL)
            0x56 => {
                self.registers.d = self.read(self.registers.hl());
                Ok(2)
            },

            // LD D, A
            0x57 => {
                self.registers.d = self.registers.a;
                Ok(1)
            },

            // LD E, B
            0x58 => {
                self.registers.e = self.registers.b;
                Ok(1)
            },

            // LD E, C
            0x59 => {
                self.registers.e = self.registers.c;
                Ok(1)
            },

            // LD E, D
            0x5A => {
                self.registers.e = self.registers.d;
                Ok(1)
            },

            // LD E, E
            0x5B => {
                self.registers.e = self.registers.e;
                Ok(1)
            },

            // LD E, H
            0x5C => {
                self.registers.e = self.registers.h;
                Ok(1)
            },

            // LD E, L
            0x5D => {
                self.registers.e = self.registers.l;
                Ok(1)
            },

            // LD E, (HL)
            0x5E => {
                self.registers.e = self.read(self.registers.hl());
                Ok(2)
            },

            // LD E, A
            0x5F => {
                self.registers.e = self.registers.a;
                Ok(1)
            },

            // LD H, B
            0x60 => {
                self.registers.h = self.registers.b;
                Ok(1)
            },

            // LD H, C
            0x61 => {
                self.registers.h = self.registers.c;
                Ok(1)
            },

            // LD H, D
            0x62 => {
                self.registers.h = self.registers.d;
                Ok(1)
            },

            // LD H, E
            0x63 => {
                self.registers.h = self.registers.e;
                Ok(1)
            },

            // LD H, H
            0x64 => {
                self.registers.h = self.registers.h;
                Ok(1)
            },

            // LD H, L
            0x65 => {
                self.registers.h = self.registers.l;
                Ok(1)
            },

            // LD H, (HL)
            0x66 => {
                self.registers.h = self.read(self.registers.hl());
                Ok(2)
            },

            // LD H, A
            0x67 => {
                self.registers.h = self.registers.a;
                Ok(1)
            },

            // LD L, B
            0x68 => {
                self.registers.l = self.registers.b;
                Ok(1)
            },

            // LD L, C
            0x69 => {
                self.registers.l = self.registers.c;
                Ok(1)
            },

            // LD L, D
            0x6A => {
                self.registers.l = self.registers.d;
                Ok(1)
            },

            // LD L, E
            0x6B => {
                self.registers.l = self.registers.e;
                Ok(1)
            },

            // LD L, H
            0x6C => {
                self.registers.l = self.registers.h;
                Ok(1)
            },

            // LD L, L
            0x6D => {
                self.registers.l = self.registers.l;
                Ok(1)
            },

            // LD L, (HL)
            0x6E => {
                self.registers.l = self.read(self.registers.hl());
                Ok(2)
            },

            // LD L, A
            0x6F => {
                self.registers.l = self.registers.a;
                Ok(1)
            },

            // LD (HL), B
            0x70 => {
                self.write(self.registers.hl(), self.registers.b);
                Ok(2)
            },

            // LD (HL), C
            0x71 => {
                self.write(self.registers.hl(), self.registers.c);
                Ok(2)
            },

            // LD (HL), D
            0x72 => {
                self.write(self.registers.hl(), self.registers.d);
                Ok(2)
            },

            // LD (HL), E
            0x73 => {
                self.write(self.registers.hl(), self.registers.e);
                Ok(2)
            },

            // LD (HL), H
            0x74 => {
                self.write(self.registers.hl(), self.registers.h);
                Ok(2)
            },

            // LD (HL), L
            0x75 => {
                self.write(self.registers.hl(), self.registers.l);
                Ok(2)
            },

            // HALT
            0x76 => {
                self.halted = true;
                Ok(1)
            }

            // LD (HL), A
            0x77 => {
                self.write(self.registers.hl(), self.registers.a);
                Ok(2)
            },

            // LD A, B
            0x78 => {
                self.registers.a = self.registers.b;
                Ok(1)
            },

            // LD A, C
            0x79 => {
                self.registers.a = self.registers.c;
                Ok(1)
            },

            // LD A, D
            0x7A => {
                self.registers.a = self.registers.d;
                Ok(1)
            },

            // LD A, E
            0x7B => {
                self.registers.a = self.registers.e;
                Ok(1)
            },

            // LD A, H
            0x7C => {
                self.registers.a = self.registers.h;
                Ok(1)
            },

            // LD A, L
            0x7D => {
                self.registers.a = self.registers.l;
                Ok(1)
            },

            // LD A, (HL)
            0x7E => {
                self.registers.a = self.read(self.registers.hl());
                Ok(2)
            },

            // LD A, A
            0x7F => {
                self.registers.a = self.registers.a;
                Ok(1)
            },

            // ADD A, B
            0x80 => {
                self.add_a(self.registers.b, false);
                Ok(1)
            },

            // ADD A, C
            0x81 => {
                self.add_a(self.registers.c, false);
                Ok(1)
            },

            // ADD A, D
            0x82 => {
                self.add_a(self.registers.d, false);
                Ok(1)
            },

            // ADD A, E
            0x83 => {
                self.add_a(self.registers.e, false);
                Ok(1)
            },

            // ADD A, H
            0x84 => {
                self.add_a(self.registers.h, false);
                Ok(1)
            },

            // ADD A, L
            0x85 => {
                self.add_a(self.registers.l, false);
                Ok(1)
            },

            // ADD A, (HL)
            0x86 => {
                self.add_a(self.read(self.registers.hl()), false);
                Ok(1)
            },

            // ADD A, A
            0x87 => {
                self.add_a(self.registers.a, false);
                Ok(1)
            },

            // ADC A, B
            0x88 => {
                self.add_a(self.registers.b, true);
                Ok(1)
            },

            // ADC A, C
            0x89 => {
                self.add_a(self.registers.c, true);
                Ok(1)
            },

            // ADC A, D
            0x8A => {
                self.add_a(self.registers.d, true);
                Ok(1)
            },

            // ADC A, E
            0x8B => {
                self.add_a(self.registers.e, true);
                Ok(1)
            },

            // ADC A, H
            0x8C => {
                self.add_a(self.registers.h, true);
                Ok(1)
            },

            // ADC A, L
            0x8D => {
                self.add_a(self.registers.l, true);
                Ok(1)
            },

            // ADC A, (HL)
            0x8E => {
                self.add_a(self.read(self.registers.hl()), true);
                Ok(1)
            },

            // ADC A, A
            0x8F => {
                self.add_a(self.registers.a, true);
                Ok(1)
            },

            // SUB A, B
            0x90 => {
                self.sub_a(self.registers.b, false);
                Ok(1)
            },

            // SUB A, C
            0x91 => {
                self.sub_a(self.registers.c, false);
                Ok(1)
            },

            // SUB A, D
            0x92 => {
                self.sub_a(self.registers.d, false);
                Ok(1)
            },

            // SUB A, E
            0x93 => {
                self.sub_a(self.registers.e, false);
                Ok(1)
            },

            // SUB A, H
            0x94 => {
                self.sub_a(self.registers.h, false);
                Ok(1)
            },

            // SUB A, L
            0x95 => {
                self.sub_a(self.registers.l, false);
                Ok(1)
            },

            // SUB A, (HL)
            0x96 => {
                self.sub_a(self.read(self.registers.hl()), false);
                Ok(1)
            },

            // SUB A, A
            0x97 => {
                self.sub_a(self.registers.a, false);
                Ok(1)
            },

            // SBC A, B
            0x98 => {
                self.sub_a(self.registers.b, true);
                Ok(1)
            },

            // SBC A, C
            0x99 => {
                self.sub_a(self.registers.c, true);
                Ok(1)
            },

            // SBC A, D
            0x9A => {
                self.sub_a(self.registers.d, true);
                Ok(1)
            },

            // SBC A, E
            0x9B => {
                self.sub_a(self.registers.e, true);
                Ok(1)
            },

            // SBC A, H
            0x9C => {
                self.sub_a(self.registers.h, true);
                Ok(1)
            },

            // SBC A, L
            0x9D => {
                self.sub_a(self.registers.l, true);
                Ok(1)
            },

            // SBC A, (HL)
            0x9E => {
                self.sub_a(self.read(self.registers.hl()), true);
                Ok(1)
            },

            // SBC A, A
            0x9F => {
                self.sub_a(self.registers.a, true);
                Ok(1)
            },

            // AND A, B
            0xA0 => {
                self.and_a(self.registers.b);
                Ok(1)
            },

            // AND A, C
            0xA1 => {
                self.and_a(self.registers.c);
                Ok(1)
            },

            // AND A, D
            0xA2 => {
                self.and_a(self.registers.d);
                Ok(1)
            },

            // AND A, E
            0xA3 => {
                self.and_a(self.registers.e);
                Ok(1)
            },

            // AND A, H
            0xA4 => {
                self.and_a(self.registers.h);
                Ok(1)
            },

            // AND A, L
            0xA5 => {
                self.and_a(self.registers.l);
                Ok(1)
            },

            // AND A, (HL)
            0xA6 => {
                self.and_a(self.read(self.registers.hl()));
                Ok(1)
            },

            // AND A, A
            0xA7 => {
                self.and_a(self.registers.a);
                Ok(1)
            },

            // XOR A, B
            0xA8 => {
                self.xor_a(self.registers.b);
                Ok(1)
            },

            // XOR A, C
            0xA9 => {
                self.xor_a(self.registers.c);
                Ok(1)
            },

            // XOR A, D
            0xAA => {
                self.xor_a(self.registers.d);
                Ok(1)
            },

            // XOR A, E
            0xAB => {
                self.xor_a(self.registers.e);
                Ok(1)
            },

            // XOR A, H
            0xAC => {
                self.xor_a(self.registers.h);
                Ok(1)
            },

            // XOR A, L
            0xAD => {
                self.xor_a(self.registers.l);
                Ok(1)
            },

            // XOR A, (HL)
            0xAE => {
                self.xor_a(self.read(self.registers.hl()));
                Ok(1)
            },

            // XOR A, A
            0xAF => {
                self.xor_a(self.registers.a);
                Ok(1)
            },

            // OR A, B
            0xB0 => {
                self.or_a(self.registers.b);
                Ok(1)
            },

            // OR A, C
            0xB1 => {
                self.or_a(self.registers.c);
                Ok(1)
            },

            // OR A, D
            0xB2 => {
                self.or_a(self.registers.d);
                Ok(1)
            },

            // OR A, E
            0xB3 => {
                self.or_a(self.registers.e);
                Ok(1)
            },

            // OR A, H
            0xB4 => {
                self.or_a(self.registers.h);
                Ok(1)
            },

            // OR A, L
            0xB5 => {
                self.or_a(self.registers.l);
                Ok(1)
            },

            // OR A, (HL)
            0xB6 => {
                self.or_a(self.read(self.registers.hl()));
                Ok(1)
            },

            // OR A, A
            0xB7 => {
                self.or_a(self.registers.a);
                Ok(1)
            },

            // CP A, B
            0xB8 => {
                self.cp_a(self.registers.b);
                Ok(1)
            },

            // CP A, C
            0xB9 => {
                self.cp_a(self.registers.c);
                Ok(1)
            },

            // CP A, D
            0xBA => {
                self.cp_a(self.registers.d);
                Ok(1)
            },

            // CP A, E
            0xBB => {
                self.cp_a(self.registers.e);
                Ok(1)
            },

            // CP A, H
            0xBC => {
                self.cp_a(self.registers.h);
                Ok(1)
            },

            // CP A, L
            0xBD => {
                self.cp_a(self.registers.l);
                Ok(1)
            },

            // CP A, (HL)
            0xBE => {
                self.cp_a(self.read(self.registers.hl()));
                Ok(1)
            },

            // CP A, A
            0xBF => {
                self.cp_a(self.registers.a);
                Ok(1)
            },

            // RET NZ
            0xC0 => if self.ret(Condition::NotZero) {
                Ok(5)
            } else {
                Ok(2)
            },

            // POP BC
            0xC1 => {
                let word = self.pop_stack();
                self.registers.set_bc(word);
                Ok(3)
            },

            // JP NZ, a16
            0xC2 => if self.jump(Condition::NotZero) {
                Ok(4)
            } else {
                Ok(3)
            },

            // JP a16
            0xC3 => {
                self.jump(Condition::None);
                Ok(4)
            },

            // CALL NZ, a16
            0xC4 => if self.call(Condition::NotZero) {
                Ok(6)
            } else {
                Ok(3)
            },

            // PUSH BC
            0xC5 => {
                self.push_stack(self.registers.bc());
                Ok(4)
            },

            // ADD A, d8
            0xC6 => {
                let value = self.next_byte();
                self.add_a(value, false);
                Ok(2)
            },

            // RST 0
            0xC7 => {
                self.rst(0x0000);
                Ok(4)
            },

            // RET Z
            0xC8 => if self.ret(Condition::Zero) {
                Ok(5)
            } else {
                Ok(2)
            },

            // RET
            0xC9 => {
                self.ret(Condition::None);
                Ok(4)
            },

            // JP Z, a16
            0xCA => if self.jump(Condition::Zero) {
                Ok(4)
            } else {
                Ok(3)
            },

            // CB
            0xCB => Ok(self.step_cb()),

            // CALL Z, a16
            0xCC => if self.call(Condition::Zero) {
                Ok(6)
            } else {
                Ok(3)
            },

            // CALL a16
            0xCD => {
                self.call(Condition::None);
                Ok(6)
            },

            // ADC A, d8
            0xCE => {
                let value = self.next_byte();
                self.add_a(value, true);
                Ok(2)
            },

            // RST 1
            0xCF => {
                self.rst(0x0008);
                Ok(4)
            },

            // RET NC
            0xD0 => if self.ret(Condition::NotCarry) {
                Ok(5)
            } else {
                Ok(2)
            },

            // POP DE
            0xD1 => {
                let word = self.pop_stack();
                self.registers.set_de(word);
                Ok(3)
            },

            // JP NC, a16
            0xD2 => if self.jump(Condition::NotCarry) {
                Ok(4)
            } else {
                Ok(3)
            },

            // CALL NC, a16
            0xD4 => if self.call(Condition::NotCarry) {
                Ok(6)
            } else {
                Ok(3)
            },

            // PUSH DE
            0xD5 => {
                self.push_stack(self.registers.de());
                Ok(4)
            },

            // SUB A, d8
            0xD6 => {
                let value = self.next_byte();
                self.sub_a(value, false);
                Ok(2)
            },

            // RST 2
            0xD7 => {
                self.rst(0x0010);
                Ok(4)
            },

            // RET C
            0xD8 => if self.ret(Condition::Carry) {
                Ok(5)
            } else {
                Ok(2)
            },

            // RETI
            0xD9 => {
                self.ei_timer = 1;
                self.ret(Condition::None);
                Ok(4)
            },

            // JP C, a16
            0xDA => if self.jump(Condition::Carry) {
                Ok(4)
            } else {
                Ok(3)
            },

            // CALL C, a16
            0xDC => if self.call(Condition::Carry) {
                Ok(6)
            } else {
                Ok(3)
            },

            // SBC A, d8
            0xDE => {
                let value = self.next_byte();
                self.sub_a(value, true);
                Ok(2)
            },

            // RST 3
            0xDF => {
                self.rst(0x0018);
                Ok(4)
            },

            // LD (a8), A
            0xE0 => {
                let address = 0xFF00 | self.next_byte() as u16;
                self.mmu.write(address, self.registers.a);
                Ok(3)
            },

            // POP HL
            0xE1 => {
                let word = self.pop_stack();
                self.registers.set_hl(word);
                Ok(3)
            },

            // LD (C), A
            0xE2 => {
                let address = 0xFF00 | self.registers.c as u16;
                self.mmu.write(address, self.registers.a);
                Ok(2)
            },

            // PUSH HL
            0xE5 => {
                self.push_stack(self.registers.hl());
                Ok(4)
            },

            // AND A, d8
            0xE6 => {
                let value = self.next_byte();
                self.and_a(value);
                Ok(2)
            },

            // RST 4
            0xE7 => {
                self.rst(0x0020);
                Ok(4)
            },

            // ADD SP, s8
            0xE8 => {
                let sp = self.registers.sp;
                let offset = self.next_byte() as i8 as i16 as u16;

                self.registers.set_flag(FlagMask::Zero, false);
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, (sp & 0x000F) + (offset & 0x000F) > 0x000F);
                self.registers.set_flag(FlagMask::Carry, (sp & 0x00FF) + (offset & 0x00FF) > 0x00FF);

                self.registers.sp = sp.wrapping_add(offset);

                Ok(4)
            },

            // JP HL
            0xE9 => {
                self.registers.pc = self.registers.hl();
                Ok(1)
            },

            // LD (a16), A
            0xEA => {
                let address = self.next_word();
                self.mmu.write(address, self.registers.a);
                Ok(4)
            },

            // XOR A, d8
            0xEE => {
                let value = self.next_byte();
                self.xor_a(value);
                Ok(2)
            },

            // RST 5
            0xEF => {
                self.rst(0x0028);
                Ok(4)
            },

            // LD A, (a8)
            0xF0 => {
                let address = 0xFF00 | self.next_byte() as u16;
                self.registers.a = self.mmu.read(address);
                Ok(3)
            },

            // POP AF
            0xF1 => {
                let word = self.pop_stack();
                self.registers.set_af(word);
                Ok(3)
            },

            // LD A, (C)
            0xF2 => {
                let address = 0xFF00 | self.registers.c as u16;
                self.registers.a = self.mmu.read(address);
                Ok(2)
            },

            // DI
            0xF3 => {
                self.di_timer = 2;
                Ok(1)
            },

            // PUSH AF
            0xF5 => {
                self.push_stack(self.registers.af());
                Ok(4)
            },

            // OR A, d8
            0xF6 => {
                let value = self.next_byte();
                self.or_a(value);
                Ok(2)
            },

            // RST 6
            0xF7 => {
                self.rst(0x0030);
                Ok(4)
            },

            // LD HL, SP+s8
            0xF8 => {
                let sp = self.registers.sp;
                let offset = self.next_byte() as i8 as i16 as u16;

                self.registers.set_flag(FlagMask::Zero, false);
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, (sp & 0x000F) + (offset & 0x000F) > 0x000F);
                self.registers.set_flag(FlagMask::Carry, (sp & 0x00FF) + (offset & 0x00FF) > 0x00FF);

                self.registers.set_hl(sp.wrapping_add(offset));

                Ok(3)
            },

            // LD SP, HL
            0xF9 => {
                self.registers.sp = self.registers.hl();
                Ok(2)
            },

            // LD A, (a16)
            0xFA => {
                let address = self.next_word();
                self.registers.a = self.mmu.read(address);
                Ok(4)
            }

            // EI
            0xFB => {
                self.ei_timer = 2;
                Ok(1)
            },

            // CP A, d8
            0xFE => {
                let value = self.next_byte();
                self.cp_a(value);
                Ok(2)
            },

            // RST 7
            0xFF => {
                self.rst(0x0038);
                Ok(4)
            },

            _ => return Err(CPUError::UnsupportedOpcode(opcode, self.registers.pc)),
        }
    }

    fn step_cb(&mut self) -> u32 {
        let cb_opcode = self.next_byte();

        match cb_opcode {
            // RLC B
            0x00 => {
                self.registers.b = self.rotate_left(self.registers.b, false, true);
                2
            },

            // RLC C
            0x01 => {
                self.registers.c = self.rotate_left(self.registers.c, false, true);
                2
            },

            // RLC D
            0x02 => {
                self.registers.d = self.rotate_left(self.registers.d, false, true);
                2
            },

            // RLC E
            0x03 => {
                self.registers.e = self.rotate_left(self.registers.e, false, true);
                2
            },

            // RLC H
            0x04 => {
                self.registers.h = self.rotate_left(self.registers.h, false, true);
                2
            },

            // RLC L
            0x05 => {
                self.registers.l = self.rotate_left(self.registers.l, false, true);
                2
            },

            // RLC (HL)
            0x06 => {
                let byte = self.rotate_left(self.read(self.registers.hl()), false, true);
                self.write(self.registers.hl(), byte);
                4
            },

            // RLC A
            0x07 => {
                self.registers.a = self.rotate_left(self.registers.a, false, true);
                2
            },

            // RRC B
            0x08 => {
                self.registers.b = self.rotate_right(self.registers.b, false, true);
                2
            },

            // RRC C
            0x09 => {
                self.registers.c = self.rotate_right(self.registers.c, false, true);
                2
            },

            // RRC D
            0x0A => {
                self.registers.d = self.rotate_right(self.registers.d, false, true);
                2
            },

            // RRC E
            0x0B => {
                self.registers.e = self.rotate_right(self.registers.e, false, true);
                2
            },

            // RRC H
            0x0C => {
                self.registers.h = self.rotate_right(self.registers.h, false, true);
                2
            },

            // RRC L
            0x0D => {
                self.registers.l = self.rotate_right(self.registers.l, false, true);
                2
            },

            // RRC (HL)
            0x0E => {
                let byte = self.rotate_right(self.read(self.registers.hl()), false, true);
                self.write(self.registers.hl(), byte);
                4
            },

            // RRC A
            0x0F => {
                self.registers.a = self.rotate_right(self.registers.a, false, true);
                2
            },

            // RL B
            0x10 => {
                self.registers.b = self.rotate_left(self.registers.b, true, true);
                2
            },

            // RL C
            0x11 => {
                self.registers.c = self.rotate_left(self.registers.c, true, true);
                2
            },

            // RL D
            0x12 => {
                self.registers.d = self.rotate_left(self.registers.d, true, true);
                2
            },

            // RL E
            0x13 => {
                self.registers.e = self.rotate_left(self.registers.e, true, true);
                2
            },

            // RL H
            0x14 => {
                self.registers.h = self.rotate_left(self.registers.h, true, true);
                2
            },

            // RL L
            0x15 => {
                self.registers.l = self.rotate_left(self.registers.l, true, true);
                2
            },

            // RL (HL)
            0x16 => {
                let byte = self.rotate_left(self.read(self.registers.hl()), true, true);
                self.write(self.registers.hl(), byte);
                4
            },

            // RL A
            0x17 => {
                self.registers.a = self.rotate_left(self.registers.a, true, true);
                2
            },

            // RR B
            0x18 => {
                self.registers.b = self.rotate_right(self.registers.b, true, true);
                2
            },

            // RR C
            0x19 => {
                self.registers.c = self.rotate_right(self.registers.c, true, true);
                2
            },

            // RR D
            0x1A => {
                self.registers.d = self.rotate_right(self.registers.d, true, true);
                2
            },

            // RR E
            0x1B => {
                self.registers.e = self.rotate_right(self.registers.e, true, true);
                2
            },

            // RR H
            0x1C => {
                self.registers.h = self.rotate_right(self.registers.h, true, true);
                2
            },

            // RR L
            0x1D => {
                self.registers.l = self.rotate_right(self.registers.l, true, true);
                2
            },

            // RR (HL)
            0x1E => {
                let byte = self.rotate_right(self.read(self.registers.hl()), true, true);
                self.write(self.registers.hl(), byte);
                4
            },

            // RR A
            0x1F => {
                self.registers.a = self.rotate_right(self.registers.a, true, true);
                2
            },

            // SLA B
            0x20 => {
                self.registers.b = self.shift_left_arithmetic(self.registers.b);
                2
            },

            // SLA C
            0x21 => {
                self.registers.c = self.shift_left_arithmetic(self.registers.c);
                2
            },

            // SLA D
            0x22 => {
                self.registers.d = self.shift_left_arithmetic(self.registers.d);
                2
            },

            // SLA E
            0x23 => {
                self.registers.e = self.shift_left_arithmetic(self.registers.e);
                2
            },

            // SLA H
            0x24 => {
                self.registers.h = self.shift_left_arithmetic(self.registers.h);
                2
            },

            // SLA L
            0x25 => {
                self.registers.l = self.shift_left_arithmetic(self.registers.l);
                2
            },

            // SLA (HL)
            0x26 => {
                let byte = self.shift_left_arithmetic(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                4
            },

            // SLA A
            0x27 => {
                self.registers.a = self.shift_left_arithmetic(self.registers.a);
                2
            },

            // SRA B
            0x28 => {
                self.registers.b = self.shift_right_arithmetic(self.registers.b);
                2
            },

            // SRA C
            0x29 => {
                self.registers.c = self.shift_right_arithmetic(self.registers.c);
                2
            },

            // SRA D
            0x2A => {
                self.registers.d = self.shift_right_arithmetic(self.registers.d);
                2
            },

            // SRA E
            0x2B => {
                self.registers.e = self.shift_right_arithmetic(self.registers.e);
                2
            },

            // SRA H
            0x2C => {
                self.registers.h = self.shift_right_arithmetic(self.registers.h);
                2
            },

            // SRA L
            0x2D => {
                self.registers.l = self.shift_right_arithmetic(self.registers.l);
                2
            },

            // SRA (HL)
            0x2E => {
                let byte = self.shift_right_arithmetic(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                4
            },

            // SRA A
            0x2F => {
                self.registers.a = self.shift_right_arithmetic(self.registers.a);
                2
            },

            // SWAP B
            0x30 => {
                self.registers.b = self.swap(self.registers.b);
                2
            },

            // SWAP C
            0x31 => {
                self.registers.c = self.swap(self.registers.c);
                2
            },

            // SWAP D
            0x32 => {
                self.registers.d = self.swap(self.registers.d);
                2
            },

            // SWAP E
            0x33 => {
                self.registers.e = self.swap(self.registers.e);
                2
            },

            // SWAP H
            0x34 => {
                self.registers.h = self.swap(self.registers.h);
                2
            },

            // SWAP L
            0x35 => {
                self.registers.l = self.swap(self.registers.l);
                2
            },

            // SWAP (HL)
            0x36 => {
                let byte = self.swap(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                4
            },

            // SWAP A
            0x37 => {
                self.registers.a = self.swap(self.registers.a);
                2
            },

            // SRL B
            0x38 => {
                self.registers.b = self.shift_right_logical(self.registers.b);
                2
            },

            // SRL C
            0x39 => {
                self.registers.c = self.shift_right_logical(self.registers.c);
                2
            },

            // SRL D
            0x3A => {
                self.registers.d = self.shift_right_logical(self.registers.d);
                2
            },

            // SRL E
            0x3B => {
                self.registers.e = self.shift_right_logical(self.registers.e);
                2
            },

            // SRL H
            0x3C => {
                self.registers.h = self.shift_right_logical(self.registers.h);
                2
            },

            // SRL L
            0x3D => {
                self.registers.l = self.shift_right_logical(self.registers.l);
                2
            },

            // SRL (HL)
            0x3E => {
                let byte = self.shift_right_logical(self.read(self.registers.hl()));
                self.write(self.registers.hl(), byte);
                4
            },

            // SRL A
            0x3F => {
                self.registers.a = self.shift_right_logical(self.registers.a);
                2
            },

            // BIT 0, B
            0x40 => {
                self.bit_test(self.registers.b, 0);
                2
            },

            // BIT 0, C
            0x41 => {
                self.bit_test(self.registers.c, 0);
                2
            },

            // BIT 0, D
            0x42 => {
                self.bit_test(self.registers.d, 0);
                2
            },
            
            // BIT 0, E
            0x43 => {
                self.bit_test(self.registers.e, 0);
                2
            },

            // BIT 0, H
            0x44 => {
                self.bit_test(self.registers.h, 0);
                2
            },

            // BIT 0, L
            0x45 => {
                self.bit_test(self.registers.l, 0);
                2
            },

            // BIT 0, (HL)
            0x46 => {
                self.bit_test(self.read(self.registers.hl()), 0);
                4
            },

            // BIT 0, A
            0x47 => {
                self.bit_test(self.registers.a, 0);
                2
            },

            // BIT 1, B
            0x48 => {
                self.bit_test(self.registers.b, 1);
                2
            },

            // BIT 1, C
            0x49 => {
                self.bit_test(self.registers.c, 1);
                2
            },

            // BIT 1, D
            0x4A => {
                self.bit_test(self.registers.d, 1);
                2
            },
            
            // BIT 1, E
            0x4B => {
                self.bit_test(self.registers.e, 1);
                2
            },

            // BIT 1, H
            0x4C => {
                self.bit_test(self.registers.h, 1);
                2
            },

            // BIT 1, L
            0x4D => {
                self.bit_test(self.registers.l, 1);
                2
            },

            // BIT 1, (HL)
            0x4E => {
                self.bit_test(self.read(self.registers.hl()), 1);
                4
            },

            // BIT 1, A
            0x4F => {
                self.bit_test(self.registers.a, 1);
                2
            },

            // BIT 2, B
            0x50 => {
                self.bit_test(self.registers.b, 2);
                2
            },

            // BIT 2, C
            0x51 => {
                self.bit_test(self.registers.c, 2);
                2
            },

            // BIT 2, D
            0x52 => {
                self.bit_test(self.registers.d, 2);
                2
            },
            
            // BIT 2, E
            0x53 => {
                self.bit_test(self.registers.e, 2);
                2
            },

            // BIT 2, H
            0x54 => {
                self.bit_test(self.registers.h, 2);
                2
            },

            // BIT 2, L
            0x55 => {
                self.bit_test(self.registers.l, 2);
                2
            },

            // BIT 2, (HL)
            0x56 => {
                self.bit_test(self.read(self.registers.hl()), 2);
                4
            },

            // BIT 2, A
            0x57 => {
                self.bit_test(self.registers.a, 2);
                2
            },

            // BIT 3, B
            0x58 => {
                self.bit_test(self.registers.b, 3);
                2
            },

            // BIT 3, C
            0x59 => {
                self.bit_test(self.registers.c, 3);
                2
            },

            // BIT 3, D
            0x5A => {
                self.bit_test(self.registers.d, 3);
                2
            },
            
            // BIT 3, E
            0x5B => {
                self.bit_test(self.registers.e, 3);
                2
            },

            // BIT 3, H
            0x5C => {
                self.bit_test(self.registers.h, 3);
                2
            },

            // BIT 3, L
            0x5D => {
                self.bit_test(self.registers.l, 3);
                2
            },

            // BIT 3, (HL)
            0x5E => {
                self.bit_test(self.read(self.registers.hl()), 3);
                4
            },

            // BIT 3, A
            0x5F => {
                self.bit_test(self.registers.a, 3);
                2
            },

            // BIT 4, B
            0x60 => {
                self.bit_test(self.registers.b, 4);
                2
            },

            // BIT 4, C
            0x61 => {
                self.bit_test(self.registers.c, 4);
                2
            },

            // BIT 4, D
            0x62 => {
                self.bit_test(self.registers.d, 4);
                2
            },
            
            // BIT 4, E
            0x63 => {
                self.bit_test(self.registers.e, 4);
                2
            },

            // BIT 4, H
            0x64 => {
                self.bit_test(self.registers.h, 4);
                2
            },

            // BIT 4, L
            0x65 => {
                self.bit_test(self.registers.l, 4);
                2
            },

            // BIT 4, (HL)
            0x66 => {
                self.bit_test(self.read(self.registers.hl()), 4);
                4
            },

            // BIT 4, A
            0x67 => {
                self.bit_test(self.registers.a, 4);
                2
            },

            // BIT 5, B
            0x68 => {
                self.bit_test(self.registers.b, 5);
                2
            },

            // BIT 5, C
            0x69 => {
                self.bit_test(self.registers.c, 5);
                2
            },

            // BIT 5, D
            0x6A => {
                self.bit_test(self.registers.d, 5);
                2
            },
            
            // BIT 5, E
            0x6B => {
                self.bit_test(self.registers.e, 5);
                2
            },

            // BIT 5, H
            0x6C => {
                self.bit_test(self.registers.h, 5);
                2
            },

            // BIT 5, L
            0x6D => {
                self.bit_test(self.registers.l, 5);
                2
            },

            // BIT 5, (HL)
            0x6E => {
                self.bit_test(self.read(self.registers.hl()), 5);
                4
            },

            // BIT 5, A
            0x6F => {
                self.bit_test(self.registers.a, 5);
                2
            },

            // BIT 6, B
            0x70 => {
                self.bit_test(self.registers.b, 6);
                2
            },

            // BIT 6, C
            0x71 => {
                self.bit_test(self.registers.c, 6);
                2
            },

            // BIT 6, D
            0x72 => {
                self.bit_test(self.registers.d, 6);
                2
            },
            
            // BIT 6, E
            0x73 => {
                self.bit_test(self.registers.e, 6);
                2
            },

            // BIT 6, H
            0x74 => {
                self.bit_test(self.registers.h, 6);
                2
            },

            // BIT 6, L
            0x75 => {
                self.bit_test(self.registers.l, 6);
                2
            },

            // BIT 6, (HL)
            0x76 => {
                self.bit_test(self.read(self.registers.hl()), 6);
                4
            },

            // BIT 6, A
            0x77 => {
                self.bit_test(self.registers.a, 6);
                2
            },

            // BIT 7, B
            0x78 => {
                self.bit_test(self.registers.b, 7);
                2
            },

            // BIT 7, C
            0x79 => {
                self.bit_test(self.registers.c, 7);
                2
            },

            // BIT 7, D
            0x7A => {
                self.bit_test(self.registers.d, 7);
                2
            },
            
            // BIT 7, E
            0x7B => {
                self.bit_test(self.registers.e, 7);
                2
            },

            // BIT 7, H
            0x7C => {
                self.bit_test(self.registers.h, 7);
                2
            },

            // BIT 7, L
            0x7D => {
                self.bit_test(self.registers.l, 7);
                2
            },

            // BIT 7, (HL)
            0x7E => {
                self.bit_test(self.read(self.registers.hl()), 7);
                4
            },

            // BIT 7, A
            0x7F => {
                self.bit_test(self.registers.a, 7);
                2
            },

            // RES 0, B
            0x80 => {
                self.registers.b = self.bit_reset(self.registers.b, 0);
                2
            },

            // RES 0, C
            0x81 => {
                self.registers.c = self.bit_reset(self.registers.c, 0);
                2
            },

            // RES 0, D
            0x82 => {
                self.registers.d = self.bit_reset(self.registers.d, 0);
                2
            },

            // RES 0, E
            0x83 => {
                self.registers.e = self.bit_reset(self.registers.e, 0);
                2
            },

            // RES 0, H
            0x84 => {
                self.registers.h = self.bit_reset(self.registers.h, 0);
                2
            },

            // RES 0, L
            0x85 => {
                self.registers.l = self.bit_reset(self.registers.l, 0);
                2
            },

            // RES 0, (HL)
            0x86 => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 0);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 0, A
            0x87 => {
                self.registers.a = self.bit_reset(self.registers.a, 0);
                2
            },

            // RES 1, B
            0x88 => {
                self.registers.b = self.bit_reset(self.registers.b, 1);
                2
            },

            // RES 1, C
            0x89 => {
                self.registers.c = self.bit_reset(self.registers.c, 1);
                2
            },

            // RES 1, D
            0x8A => {
                self.registers.d = self.bit_reset(self.registers.d, 1);
                2
            },

            // RES 1, E
            0x8B => {
                self.registers.e = self.bit_reset(self.registers.e, 1);
                2
            },

            // RES 1, H
            0x8C => {
                self.registers.h = self.bit_reset(self.registers.h, 1);
                2
            },

            // RES 1, L
            0x8D => {
                self.registers.l = self.bit_reset(self.registers.l, 1);
                2
            },

            // RES 1, (HL)
            0x8E => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 1);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 1, A
            0x8F => {
                self.registers.a = self.bit_reset(self.registers.a, 1);
                2
            },

            // RES 2, B
            0x90 => {
                self.registers.b = self.bit_reset(self.registers.b, 2);
                2
            },

            // RES 2, C
            0x91 => {
                self.registers.c = self.bit_reset(self.registers.c, 2);
                2
            },

            // RES 2, D
            0x92 => {
                self.registers.d = self.bit_reset(self.registers.d, 2);
                2
            },

            // RES 2, E
            0x93 => {
                self.registers.e = self.bit_reset(self.registers.e, 2);
                2
            },

            // RES 2, H
            0x94 => {
                self.registers.h = self.bit_reset(self.registers.h, 2);
                2
            },

            // RES 2, L
            0x95 => {
                self.registers.l = self.bit_reset(self.registers.l, 2);
                2
            },

            // RES 2, (HL)
            0x96 => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 2);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 2, A
            0x97 => {
                self.registers.a = self.bit_reset(self.registers.a, 2);
                2
            },

            // RES 3, B
            0x98 => {
                self.registers.b = self.bit_reset(self.registers.b, 3);
                2
            },

            // RES 3, C
            0x99 => {
                self.registers.c = self.bit_reset(self.registers.c, 3);
                2
            },

            // RES 3, D
            0x9A => {
                self.registers.d = self.bit_reset(self.registers.d, 3);
                2
            },

            // RES 3, E
            0x9B => {
                self.registers.e = self.bit_reset(self.registers.e, 3);
                2
            },

            // RES 3, H
            0x9C => {
                self.registers.h = self.bit_reset(self.registers.h, 3);
                2
            },

            // RES 3, L
            0x9D => {
                self.registers.l = self.bit_reset(self.registers.l, 3);
                2
            },

            // RES 3, (HL)
            0x9E => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 3);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 3, A
            0x9F => {
                self.registers.a = self.bit_reset(self.registers.a, 3);
                2
            },

            // RES 4, B
            0xA0 => {
                self.registers.b = self.bit_reset(self.registers.b, 4);
                2
            },

            // RES 4, C
            0xA1 => {
                self.registers.c = self.bit_reset(self.registers.c, 4);
                2
            },

            // RES 4, D
            0xA2 => {
                self.registers.d = self.bit_reset(self.registers.d, 4);
                2
            },

            // RES 4, E
            0xA3 => {
                self.registers.e = self.bit_reset(self.registers.e, 4);
                2
            },

            // RES 4, H
            0xA4 => {
                self.registers.h = self.bit_reset(self.registers.h, 4);
                2
            },

            // RES 4, L
            0xA5 => {
                self.registers.l = self.bit_reset(self.registers.l, 4);
                2
            },

            // RES 4, (HL)
            0xA6 => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 4);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 4, A
            0xA7 => {
                self.registers.a = self.bit_reset(self.registers.a, 4);
                2
            },

            // RES 5, B
            0xA8 => {
                self.registers.b = self.bit_reset(self.registers.b, 5);
                2
            },

            // RES 5, C
            0xA9 => {
                self.registers.c = self.bit_reset(self.registers.c, 5);
                2
            },

            // RES 5, D
            0xAA => {
                self.registers.d = self.bit_reset(self.registers.d, 5);
                2
            },

            // RES 5, E
            0xAB => {
                self.registers.e = self.bit_reset(self.registers.e, 5);
                2
            },

            // RES 5, H
            0xAC => {
                self.registers.h = self.bit_reset(self.registers.h, 5);
                2
            },

            // RES 5, L
            0xAD => {
                self.registers.l = self.bit_reset(self.registers.l, 5);
                2
            },

            // RES 5, (HL)
            0xAE => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 5);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 5, A
            0xAF => {
                self.registers.a = self.bit_reset(self.registers.a, 5);
                2
            },

            // RES 6, B
            0xB0 => {
                self.registers.b = self.bit_reset(self.registers.b, 6);
                2
            },

            // RES 6, C
            0xB1 => {
                self.registers.c = self.bit_reset(self.registers.c, 6);
                2
            },

            // RES 6, D
            0xB2 => {
                self.registers.d = self.bit_reset(self.registers.d, 6);
                2
            },

            // RES 6, E
            0xB3 => {
                self.registers.e = self.bit_reset(self.registers.e, 6);
                2
            },

            // RES 6, H
            0xB4 => {
                self.registers.h = self.bit_reset(self.registers.h, 6);
                2
            },

            // RES 6, L
            0xB5 => {
                self.registers.l = self.bit_reset(self.registers.l, 6);
                2
            },

            // RES 6, (HL)
            0xB6 => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 6);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 6, A
            0xB7 => {
                self.registers.a = self.bit_reset(self.registers.a, 6);
                2
            },

            // RES 7, B
            0xB8 => {
                self.registers.b = self.bit_reset(self.registers.b, 7);
                2
            },

            // RES 7, C
            0xB9 => {
                self.registers.c = self.bit_reset(self.registers.c, 7);
                2
            },

            // RES 7, D
            0xBA => {
                self.registers.d = self.bit_reset(self.registers.d, 7);
                2
            },

            // RES 7, E
            0xBB => {
                self.registers.e = self.bit_reset(self.registers.e, 7);
                2
            },

            // RES 7, H
            0xBC => {
                self.registers.h = self.bit_reset(self.registers.h, 7);
                2
            },

            // RES 7, L
            0xBD => {
                self.registers.l = self.bit_reset(self.registers.l, 7);
                2
            },

            // RES 7, (HL)
            0xBE => {
                let byte = self.bit_reset(self.read(self.registers.hl()), 7);
                self.write(self.registers.hl(), byte);
                2
            },

            // RES 7, A
            0xBF => {
                self.registers.a = self.bit_reset(self.registers.a, 7);
                2
            },

            // SET 0, B
            0xC0 => {
                self.registers.b = self.bit_set(self.registers.b, 0);
                2
            },

            // SET 0, C
            0xC1 => {
                self.registers.c = self.bit_set(self.registers.c, 0);
                2
            },

            // SET 0, D
            0xC2 => {
                self.registers.d = self.bit_set(self.registers.d, 0);
                2
            },

            // SET 0, E
            0xC3 => {
                self.registers.e = self.bit_set(self.registers.e, 0);
                2
            },

            // SET 0, H
            0xC4 => {
                self.registers.h = self.bit_set(self.registers.h, 0);
                2
            },

            // SET 0, L
            0xC5 => {
                self.registers.l = self.bit_set(self.registers.l, 0);
                2
            },

            // SET 0, (HL)
            0xC6 => {
                let byte = self.bit_set(self.read(self.registers.hl()), 0);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 0, A
            0xC7 => {
                self.registers.a = self.bit_set(self.registers.a, 0);
                2
            },

            // SET 1, B
            0xC8 => {
                self.registers.b = self.bit_set(self.registers.b, 1);
                2
            },

            // SET 1, C
            0xC9 => {
                self.registers.c = self.bit_set(self.registers.c, 1);
                2
            },

            // SET 1, D
            0xCA => {
                self.registers.d = self.bit_set(self.registers.d, 1);
                2
            },

            // SET 1, E
            0xCB => {
                self.registers.e = self.bit_set(self.registers.e, 1);
                2
            },

            // SET 1, H
            0xCC => {
                self.registers.h = self.bit_set(self.registers.h, 1);
                2
            },

            // SET 1, L
            0xCD => {
                self.registers.l = self.bit_set(self.registers.l, 1);
                2
            },

            // SET 1, (HL)
            0xCE => {
                let byte = self.bit_set(self.read(self.registers.hl()), 1);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 1, A
            0xCF => {
                self.registers.a = self.bit_set(self.registers.a, 1);
                2
            },

            // SET 2, B
            0xD0 => {
                self.registers.b = self.bit_set(self.registers.b, 2);
                2
            },

            // SET 2, C
            0xD1 => {
                self.registers.c = self.bit_set(self.registers.c, 2);
                2
            },

            // SET 2, D
            0xD2 => {
                self.registers.d = self.bit_set(self.registers.d, 2);
                2
            },

            // SET 2, E
            0xD3 => {
                self.registers.e = self.bit_set(self.registers.e, 2);
                2
            },

            // SET 2, H
            0xD4 => {
                self.registers.h = self.bit_set(self.registers.h, 2);
                2
            },

            // SET 2, L
            0xD5 => {
                self.registers.l = self.bit_set(self.registers.l, 2);
                2
            },

            // SET 2, (HL)
            0xD6 => {
                let byte = self.bit_set(self.read(self.registers.hl()), 2);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 2, A
            0xD7 => {
                self.registers.a = self.bit_set(self.registers.a, 2);
                2
            },

            // SET 3, B
            0xD8 => {
                self.registers.b = self.bit_set(self.registers.b, 3);
                2
            },

            // SET 3, C
            0xD9 => {
                self.registers.c = self.bit_set(self.registers.c, 3);
                2
            },

            // SET 3, D
            0xDA => {
                self.registers.d = self.bit_set(self.registers.d, 3);
                2
            },

            // SET 3, E
            0xDB => {
                self.registers.e = self.bit_set(self.registers.e, 3);
                2
            },

            // SET 3, H
            0xDC => {
                self.registers.h = self.bit_set(self.registers.h, 3);
                2
            },

            // SET 3, L
            0xDD => {
                self.registers.l = self.bit_set(self.registers.l, 3);
                2
            },

            // SET 3, (HL)
            0xDE => {
                let byte = self.bit_set(self.read(self.registers.hl()), 3);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 3, A
            0xDF => {
                self.registers.a = self.bit_set(self.registers.a, 3);
                2
            },

            // SET 4, B
            0xE0 => {
                self.registers.b = self.bit_set(self.registers.b, 4);
                2
            },

            // SET 4, C
            0xE1 => {
                self.registers.c = self.bit_set(self.registers.c, 4);
                2
            },

            // SET 4, D
            0xE2 => {
                self.registers.d = self.bit_set(self.registers.d, 4);
                2
            },

            // SET 4, E
            0xE3 => {
                self.registers.e = self.bit_set(self.registers.e, 4);
                2
            },

            // SET 4, H
            0xE4 => {
                self.registers.h = self.bit_set(self.registers.h, 4);
                2
            },

            // SET 4, L
            0xE5 => {
                self.registers.l = self.bit_set(self.registers.l, 4);
                2
            },

            // SET 4, (HL)
            0xE6 => {
                let byte = self.bit_set(self.read(self.registers.hl()), 4);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 4, A
            0xE7 => {
                self.registers.a = self.bit_set(self.registers.a, 4);
                2
            },

            // SET 5, B
            0xE8 => {
                self.registers.b = self.bit_set(self.registers.b, 5);
                2
            },

            // SET 5, C
            0xE9 => {
                self.registers.c = self.bit_set(self.registers.c, 5);
                2
            },

            // SET 5, D
            0xEA => {
                self.registers.d = self.bit_set(self.registers.d, 5);
                2
            },

            // SET 5, E
            0xEB => {
                self.registers.e = self.bit_set(self.registers.e, 5);
                2
            },

            // SET 5, H
            0xEC => {
                self.registers.h = self.bit_set(self.registers.h, 5);
                2
            },

            // SET 5, L
            0xED => {
                self.registers.l = self.bit_set(self.registers.l, 5);
                2
            },

            // SET 5, (HL)
            0xEE => {
                let byte = self.bit_set(self.read(self.registers.hl()), 5);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 5, A
            0xEF => {
                self.registers.a = self.bit_set(self.registers.a, 5);
                2
            },

            // SET 6, B
            0xF0 => {
                self.registers.b = self.bit_set(self.registers.b, 6);
                2
            },

            // SET 6, C
            0xF1 => {
                self.registers.c = self.bit_set(self.registers.c, 6);
                2
            },

            // SET 6, D
            0xF2 => {
                self.registers.d = self.bit_set(self.registers.d, 6);
                2
            },

            // SET 6, E
            0xF3 => {
                self.registers.e = self.bit_set(self.registers.e, 6);
                2
            },

            // SET 6, H
            0xF4 => {
                self.registers.h = self.bit_set(self.registers.h, 6);
                2
            },

            // SET 6, L
            0xF5 => {
                self.registers.l = self.bit_set(self.registers.l, 6);
                2
            },

            // SET 6, (HL)
            0xF6 => {
                let byte = self.bit_set(self.read(self.registers.hl()), 6);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 6, A
            0xF7 => {
                self.registers.a = self.bit_set(self.registers.a, 6);
                2
            },

            // SET 7, B
            0xF8 => {
                self.registers.b = self.bit_set(self.registers.b, 7);
                2
            },

            // SET 7, C
            0xF9 => {
                self.registers.c = self.bit_set(self.registers.c, 7);
                2
            },

            // SET 7, D
            0xFA => {
                self.registers.d = self.bit_set(self.registers.d, 7);
                2
            },

            // SET 7, E
            0xFB => {
                self.registers.e = self.bit_set(self.registers.e, 7);
                2
            },

            // SET 7, H
            0xFC => {
                self.registers.h = self.bit_set(self.registers.h, 7);
                2
            },

            // SET 7, L
            0xFD => {
                self.registers.l = self.bit_set(self.registers.l, 7);
                2
            },

            // SET 7, (HL)
            0xFE => {
                let byte = self.bit_set(self.read(self.registers.hl()), 7);
                self.write(self.registers.hl(), byte);
                2
            },

            // SET 7, A
            0xFF => {
                self.registers.a = self.bit_set(self.registers.a, 7);
                2
            }
        }
    }

    fn add_to_hl(&mut self, value: u16) {
        let hl = self.registers.hl();
        let sum = hl.wrapping_add(value);

        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, (hl & 0x0FFF) + (value & 0x0FFF) > 0x0FFF);
        self.registers.set_flag(FlagMask::Carry, hl > 0xFFFF - value);
        self.registers.set_hl(sum);
    }

    fn inc_8(&mut self, value: u8) -> u8 {
        let sum = value.wrapping_add(1);

        self.registers.set_flag(FlagMask::Zero, sum == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, (value & 0x0F) + 1 > 0x0F);

        sum
    }

    fn dec_8(&mut self, value: u8) -> u8 {
        let diff = value.wrapping_sub(1);

        self.registers.set_flag(FlagMask::Zero, diff == 0);
        self.registers.set_flag(FlagMask::Subtract, true);
        self.registers.set_flag(FlagMask::HalfCarry, (value & 0x0F) == 0);

        diff
    }

    fn rotate_left(&mut self, value: u8, through_carry: bool, cb: bool) -> u8 {
        let carry = (value & 0x80) >> 7;
        let mut rot = value << 1;

        
        if !through_carry {
            rot |= carry;
        }
        else if self.registers.flag(FlagMask::Carry) {
            rot |= 0x01;
        }

        self.registers.set_flag(FlagMask::Zero, if cb { rot == 0 } else { false });
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, carry == 0x01);

        rot
    }

    fn rotate_right(&mut self, value: u8, through_carry: bool, cb: bool) -> u8 {
        let carry = value & 0x01;
        let mut rot = value >> 1;

        if !through_carry {
            rot |= carry << 7;
        }
        else if self.registers.flag(FlagMask::Carry) {
            rot |= 0x80;
        }

        self.registers.set_flag(FlagMask::Zero, if cb { rot == 0 } else { false });
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, carry == 0x01);

        rot
    }

    fn jump_relative(&mut self, condition: Condition) -> bool {
        let jump = (self.next_byte() as i8) as i16;

        if self.evaluate(condition) {
            self.registers.pc = self.registers.pc.wrapping_add_signed(jump);
            return true;
        }

        false
    }

    fn jump(&mut self, condition: Condition) -> bool {
        let jump = self.next_word();

        if self.evaluate(condition) {
            self.registers.pc = jump;
            return true;
        }

        false
    }

    fn add_a(&mut self, value: u8, with_carry: bool) {
        let a = self.registers.a;
        let carry = if with_carry && self.registers.flag(FlagMask::Carry) { 1 } else { 0 };
        let sum = a.wrapping_add(value).wrapping_add(carry);

        self.registers.set_flag(FlagMask::Zero, sum == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, (a & 0x0F) + (value & 0x0F) + carry > 0x0F);
        self.registers
            .set_flag(FlagMask::Carry, (a as u16) + (value as u16) + (carry as u16) > 0xFF);

        self.registers.a = sum;
    }

    fn sub_a(&mut self, value: u8, with_carry: bool) {
        let a = self.registers.a;
        let carry = if with_carry && self.registers.flag(FlagMask::Carry) { 1 } else { 0 };
        let diff = a.wrapping_sub(value).wrapping_sub(carry);

        self.registers.set_flag(FlagMask::Zero, diff == 0);
        self.registers.set_flag(FlagMask::Subtract, true);
        self.registers.set_flag(FlagMask::HalfCarry, (a & 0x0F) < (value & 0x0F) + carry);
        self.registers.set_flag(FlagMask::Carry, (a as u16) < (value as u16) + (carry as u16));

        self.registers.a = diff;
    }

    fn and_a(&mut self, value: u8) {
        let and = self.registers.a & value;

        self.registers.set_flag(FlagMask::Zero, and == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, true);
        self.registers.set_flag(FlagMask::Carry, false);

        self.registers.a = and;
    }

    fn xor_a(&mut self, value: u8) {
        let xor = self.registers.a ^ value;

        self.registers.set_flag(FlagMask::Zero, xor == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, false);

        self.registers.a = xor;
    }

    fn or_a(&mut self, value: u8) {
        let or = self.registers.a | value;

        self.registers.set_flag(FlagMask::Zero, or == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, false);

        self.registers.a = or;
    }

    fn cp_a(&mut self, value: u8) {
        let a = self.registers.a;
        self.sub_a(value, false);

        self.registers.a = a;
    }

    fn call(&mut self, condition: Condition) -> bool {
        let jump = self.next_word();

        if self.evaluate(condition) {
            let address = self.registers.pc;
            self.push_stack(address);
            self.registers.pc = jump;
            return true;
        }

        false
    }

    fn ret(&mut self, condition: Condition) -> bool {
        if self.evaluate(condition) {
            self.registers.pc = self.pop_stack();
            return true;
        }

        false
    }

    fn rst(&mut self, address: u16) {
        self.push_stack(self.registers.pc);
        self.registers.pc = address;
    }

    fn shift_left_arithmetic(&mut self, value: u8) -> u8 {
        let carry = (value & 0x80) >> 7;
        let shifted = value << 1;

        self.registers.set_flag(FlagMask::Zero, shifted == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, carry == 1);

        shifted
    }

    fn shift_right_arithmetic(&mut self, value: u8) -> u8 {
        let carry = value & 0x01;
        let msb = value & 0x80;
        let shifted = (value >> 1) | msb;

        self.registers.set_flag(FlagMask::Zero, shifted == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, carry == 1);

        shifted
    }

    fn shift_right_logical(&mut self, value: u8) -> u8 {
        let carry = value & 0x01;
        let shifted = value >> 1;

        self.registers.set_flag(FlagMask::Zero, shifted == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, carry == 1);

        shifted
    }

    fn swap(&mut self, value: u8) -> u8 {
        let swapped = ((value & 0xF0) >> 4) | ((value & 0x0F) << 4);

        self.registers.set_flag(FlagMask::Zero, swapped == 0);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, false);
        self.registers.set_flag(FlagMask::Carry, false);

        swapped
    }

    fn bit_test(&mut self, value: u8, bit: u8) {
        let mask = 0x01 << bit;

        self.registers.set_flag(FlagMask::Zero, value & mask == 0x00);
        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, true);
    }

    fn bit_reset(&mut self, value: u8, bit: u8) -> u8 {
        let mask = !(0x01 << bit);
        value & mask
    }

    fn bit_set(&mut self, value: u8, bit: u8) -> u8 {
        let mask = 0x01 << bit;
        value | mask
    }

    fn pop_stack(&mut self) -> u16 {
        let lower = self.mmu.read(self.registers.sp);
        self.registers.inc_sp();
        let upper = self.mmu.read(self.registers.sp);
        self.registers.inc_sp();

        u16::from_le_bytes([lower, upper])
    }

    fn push_stack(&mut self, value: u16) {
        let [upper, lower] = value.to_be_bytes();

        self.registers.dec_sp();
        self.mmu.write(self.registers.sp, upper);
        self.registers.dec_sp();
        self.mmu.write(self.registers.sp, lower);
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.mmu.read(self.registers.pc);

        self.registers.inc_pc(1);

        byte
    }

    fn next_word(&mut self) -> u16 {
        let bytes = [
            self.mmu.read(self.registers.pc),
            self.mmu.read(self.registers.pc.wrapping_add(1)),
        ];

        self.registers.inc_pc(2);

        u16::from_le_bytes(bytes)
    }

    fn evaluate(&self, condition: Condition) -> bool {
        match condition {
            Condition::None => true,
            Condition::Zero => self.registers.flag(FlagMask::Zero),
            Condition::NotZero => !self.registers.flag(FlagMask::Zero),
            Condition::Carry => self.registers.flag(FlagMask::Carry),
            Condition::NotCarry => !self.registers.flag(FlagMask::Carry)
        }
    }

    fn update_ime(&mut self) {
        self.di_timer = match self.di_timer {
            2 => 1,
            1 => {
                self.interrupt_master_enable = false;
                0
            },
            _ => 0
        };

        self.ei_timer = match self.ei_timer {
            2 => 1,
            1 => {
                self.interrupt_master_enable = true;
                0
            },
            _ => 0
        };
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Condition {
    None,
    Zero,
    NotZero,
    Carry,
    NotCarry
}