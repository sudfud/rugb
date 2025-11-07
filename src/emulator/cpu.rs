mod mmu;
mod registers;

use crate::LCD;

use super::Cartridge;

use mmu::{Interrupt, MMU};
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
    pub(super) fn new(cartridge: Cartridge, lcd: LCD) -> Self {
        Self {
            registers: Registers::new(),
            mmu: MMU::new(cartridge, lcd),
            interrupt_master_enable: false,
            ei_timer: 0,
            di_timer: 0,
            halted: false
        }
    }

    pub(super) fn pc(&self) -> u16 {
        self.registers.pc
    }

    pub(super) fn halted(&self) -> bool {
        self.halted
    }

    pub(super) fn read(&self, address: u16) -> u8 {
        self.mmu.read(address)
    }

    pub(super) fn write(&mut self, address: u16, value: u8) {
        self.mmu.write(address, value);
    }

    pub(super) fn execute(&mut self) -> Result<(), CPUError> {
        self.update_ime();

        if self.handle_interrupts() {
            return Ok(());
        }

        if self.halted {
            self.mmu.cycle(4);
            return Ok(());
        }

        self.step()?;

        Ok(())
    }

    fn step(&mut self) -> Result<(), CPUError> {
        let opcode = self.next_byte();

        match opcode {
            // NOP
            0x00 => {},

            // LD BC, d16
            0x01 => {
                let word = self.next_word();
                self.registers.set_bc(word);
            },

            // LD (BC), A
            0x02 => self.mmu.write_cycle(self.registers.bc(), self.registers.a),

            // INC BC
            0x03 => {
                self.registers.set_bc(self.registers.bc().wrapping_add(1));
                self.mmu.cycle(4);
            }

            // INC B
            0x04 => self.registers.b = self.inc_8(self.registers.b),

            // DEC B
            0x05 => self.registers.b = self.dec_8(self.registers.b),

            // LD B, d8
            0x06 => {
                let byte = self.next_byte();
                self.registers.b = byte;
            },

            // RLCA
            0x07 => self.registers.a = self.rotate_left(self.registers.a, false, false),

            // LD (a16), SP
            0x08 => {
                let address = self.next_word();
                let [lower, upper] = self.registers.sp.to_le_bytes();

                self.mmu.write_cycle(address, lower);
                self.mmu.write_cycle(address.wrapping_add(1), upper);
            },

            // ADD HL, BC
            0x09 => self.add_to_hl(self.registers.bc()),

            // LD A, (BC)
            0x0A => self.registers.a = self.mmu.read_cycle(self.registers.bc()),

            // DEC BC
            0x0B => {
                self.registers.set_bc(self.registers.bc().wrapping_sub(1));
                self.mmu.cycle(4);
            },

            // INC C
            0x0C => self.registers.c = self.inc_8(self.registers.c),

            // DEC C
            0x0D => self.registers.c = self.dec_8(self.registers.c),

            // LD C, d8
            0x0E => {
                let byte = self.next_byte();
                self.registers.c = byte;
            },

            // RRCA
            0x0F => self.registers.a = self.rotate_right(self.registers.a, false, false),

            // STOP
            0x10 => self.registers.inc_pc(1),

            // LD DE, d16
            0x11 => {
                let word = self.next_word();
                self.registers.set_de(word);
            },

            // LD (DE), A
            0x12 => self.mmu.write_cycle(self.registers.de(), self.registers.a),

            // INC DE
            0x13 => {
                self.registers.set_de(self.registers.de().wrapping_add(1));
                self.mmu.cycle(4);
            },

            // INC D
            0x14 => self.registers.d = self.inc_8(self.registers.d),

            // DEC D
            0x15 => self.registers.d = self.dec_8(self.registers.d),

            // LD D, d8
            0x16 => {
                let byte = self.next_byte();
                self.registers.d = byte;
            },

            // RLA
            0x17 => self.registers.a = self.rotate_left(self.registers.a, true, false),

            // JR s8
            0x18 => self.jump_relative(Condition::None),

            // ADD HL, DE
            0x19 => self.add_to_hl(self.registers.de()),

            // LD A, (DE)
            0x1A => self.registers.a = self.mmu.read_cycle(self.registers.de()),

            // DEC DE
            0x1B => {
                self.registers.set_de(self.registers.de().wrapping_sub(1));
                self.mmu.cycle(4);
            },

            // INC E
            0x1C => self.registers.e = self.inc_8(self.registers.e),

            // DEC E
            0x1D => self.registers.e = self.dec_8(self.registers.e),

            // LD E, d8
            0x1E => {
                let byte = self.next_byte();
                self.registers.e = byte;
            },

            // RRA
            0x1F => self.registers.a = self.rotate_right(self.registers.a, true, false),

            // JR NZ, s8
            0x20 => self.jump_relative(Condition::NotZero),

            // LD HL, d16
            0x21 => {
                let word = self.next_word();
                self.registers.set_hl(word);
            },

            // LD (HL+), A
            0x22 => {
                let address = self.registers.hli();
                self.mmu.write_cycle(address, self.registers.a);
            },

            // INC HL
            0x23 => {
                self.registers.set_hl(self.registers.hl().wrapping_add(1));
                self.mmu.cycle(4);
            },

            // INC H
            0x24 => self.registers.h = self.inc_8(self.registers.h),

            // DEC H
            0x25 => self.registers.h = self.dec_8(self.registers.h),

            // LD H, d8
            0x26 => {
                let byte = self.next_byte();
                self.registers.h = byte;
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
            },

            // JR Z, s8
            0x28 => self.jump_relative(Condition::Zero),

            // ADD HL, HL
            0x29 => self.add_to_hl(self.registers.hl()),

            // LD A, (HL+)
            0x2A => {
                let address = self.registers.hli();
                self.registers.a = self.mmu.read_cycle(address);
            },

            // DEC HL
            0x2B => {
                self.registers.set_hl(self.registers.hl().wrapping_sub(1));
                self.mmu.cycle(4);
            },

            // INC L
            0x2C => self.registers.l = self.inc_8(self.registers.l),

            // DEC L
            0x2D => self.registers.l = self.dec_8(self.registers.l),

            // LD L, d8
            0x2E => {
                let byte = self.next_byte();
                self.registers.l = byte;
            },

            // CPL
            0x2F => {
                self.registers.a = !self.registers.a;
                self.registers.set_flag(FlagMask::Subtract, true);
                self.registers.set_flag(FlagMask::HalfCarry, true);
            },

            // JR NC, s8
            0x30 => self.jump_relative(Condition::NotCarry),

            // LD SP, d16
            0x31 => self.registers.sp = self.next_word(),

            // LD (HL-), A
            0x32 => {
                let address = self.registers.hld();
                self.mmu.write_cycle(address, self.registers.a);
            },

            // INC SP
            0x33 => {
                self.registers.sp = self.registers.sp.wrapping_add(1);
                self.mmu.cycle(4);
            },

            // INC (HL)
            0x34 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.inc_8(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // DEC (HL)
            0x35 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.dec_8(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // LD (HL), d8
            0x36 => {
                let byte = self.next_byte();
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SCF
            0x37 => {
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, false);
                self.registers.set_flag(FlagMask::Carry, true);
            },

            // JR C, s8
            0x38 => self.jump_relative(Condition::Carry),

            // ADD HL, SP
            0x39 => self.add_to_hl(self.registers.sp),

            // LD A, (HL-)
            0x3A => {
                let address = self.registers.hld();
                self.registers.a = self.mmu.read_cycle(address);
            },

            // DEC SP
            0x3B => {
                self.registers.sp = self.registers.sp.wrapping_sub(1);
                self.mmu.cycle(4);
            },

            // INC A
            0x3C => self.registers.a = self.inc_8(self.registers.a),

            // DEC A
            0x3D => self.registers.a = self.dec_8(self.registers.a),

            // LD A, d8
            0x3E => {
                let byte = self.next_byte();
                self.registers.a = byte;
            },

            // CCF
            0x3F => {
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, false);
                self.registers.set_flag(FlagMask::Carry, !self.registers.flag(FlagMask::Carry));
            },

            // LD B, B
            0x40 => self.registers.b = self.registers.b,

            // LD B, C
            0x41 => self.registers.b = self.registers.c,

            // LD B, D
            0x42 => self.registers.b = self.registers.d,

            // LD B, E
            0x43 => self.registers.b = self.registers.e,

            // LD B, H
            0x44 => self.registers.b = self.registers.h,

            // LD B, L
            0x45 => self.registers.b = self.registers.l,

            // LD B, (HL)
            0x46 => self.registers.b = self.mmu.read_cycle(self.registers.hl()),

            // LD B, A
            0x47 => self.registers.b = self.registers.a,

            // LD C, B
            0x48 => self.registers.c = self.registers.b,

            // LD C, C
            0x49 => self.registers.c = self.registers.c,

            // LD C, D
            0x4A => self.registers.c = self.registers.d,

            // LD C, E
            0x4B => self.registers.c = self.registers.e,

            // LD C, H
            0x4C => self.registers.c = self.registers.h,

            // LD C, L
            0x4D => self.registers.c = self.registers.l,

            // LD C, (HL)
            0x4E => self.registers.c = self.mmu.read_cycle(self.registers.hl()),

            // LD C, A
            0x4F => self.registers.c = self.registers.a,

            // LD D, B
            0x50 => self.registers.d = self.registers.b,

            // LD D, C
            0x51 => self.registers.d = self.registers.c,

            // LD D, D
            0x52 => self.registers.d = self.registers.d,

            // LD D, E
            0x53 => self.registers.d = self.registers.e,

            // LD D, H
            0x54 => self.registers.d = self.registers.h,

            // LD D, L
            0x55 => self.registers.d = self.registers.l,

            // LD D, (HL)
            0x56 => self.registers.d = self.mmu.read_cycle(self.registers.hl()),

            // LD D, A
            0x57 => self.registers.d = self.registers.a,

            // LD E, B
            0x58 => self.registers.e = self.registers.b,

            // LD E, C
            0x59 => self.registers.e = self.registers.c,

            // LD E, D
            0x5A => self.registers.e = self.registers.d,

            // LD E, E
            0x5B => self.registers.e = self.registers.e,

            // LD E, H
            0x5C => self.registers.e = self.registers.h,

            // LD E, L
            0x5D => self.registers.e = self.registers.l,

            // LD E, (HL)
            0x5E => self.registers.e = self.mmu.read_cycle(self.registers.hl()),

            // LD E, A
            0x5F => self.registers.e = self.registers.a,

            // LD H, B
            0x60 => self.registers.h = self.registers.b,

            // LD H, C
            0x61 => self.registers.h = self.registers.c,

            // LD H, D
            0x62 => self.registers.h = self.registers.d,

            // LD H, E
            0x63 => self.registers.h = self.registers.e,

            // LD H, H
            0x64 => self.registers.h = self.registers.h,

            // LD H, L
            0x65 => self.registers.h = self.registers.l,

            // LD H, (HL)
            0x66 => self.registers.h = self.mmu.read_cycle(self.registers.hl()),

            // LD H, A
            0x67 => self.registers.h = self.registers.a,

            // LD L, B
            0x68 => self.registers.l = self.registers.b,

            // LD L, C
            0x69 => self.registers.l = self.registers.c,

            // LD L, D
            0x6A => self.registers.l = self.registers.d,

            // LD L, E
            0x6B => self.registers.l = self.registers.e,

            // LD L, H
            0x6C => self.registers.l = self.registers.h,

            // LD L, L
            0x6D => self.registers.l = self.registers.l,

            // LD L, (HL)
            0x6E => self.registers.l = self.mmu.read_cycle(self.registers.hl()),

            // LD L, A
            0x6F => self.registers.l = self.registers.a,

            // LD (HL), B
            0x70 => self.mmu.write_cycle(self.registers.hl(), self.registers.b),

            // LD (HL), C
            0x71 => self.mmu.write_cycle(self.registers.hl(), self.registers.c),

            // LD (HL), D
            0x72 => self.mmu.write_cycle(self.registers.hl(), self.registers.d),

            // LD (HL), E
            0x73 => self.mmu.write_cycle(self.registers.hl(), self.registers.e),

            // LD (HL), H
            0x74 => self.mmu.write_cycle(self.registers.hl(), self.registers.h),

            // LD (HL), L
            0x75 => self.mmu.write_cycle(self.registers.hl(), self.registers.l),

            // HALT
            0x76 => self.halted = true,

            // LD (HL), A
            0x77 => self.mmu.write_cycle(self.registers.hl(), self.registers.a),

            // LD A, B
            0x78 => self.registers.a = self.registers.b,

            // LD A, C
            0x79 => self.registers.a = self.registers.c,

            // LD A, D
            0x7A => self.registers.a = self.registers.d,

            // LD A, E
            0x7B => self.registers.a = self.registers.e,

            // LD A, H
            0x7C => self.registers.a = self.registers.h,

            // LD A, L
            0x7D => self.registers.a = self.registers.l,

            // LD A, (HL)
            0x7E => self.registers.a = self.mmu.read_cycle(self.registers.hl()),

            // LD A, A
            0x7F => self.registers.a = self.registers.a,

            // ADD A, B
            0x80 => self.add_a(self.registers.b, false),

            // ADD A, C
            0x81 => self.add_a(self.registers.c, false),

            // ADD A, D
            0x82 => self.add_a(self.registers.d, false),

            // ADD A, E
            0x83 => self.add_a(self.registers.e, false),

            // ADD A, H
            0x84 => self.add_a(self.registers.h, false),

            // ADD A, L
            0x85 => self.add_a(self.registers.l, false),

            // ADD A, (HL)
            0x86 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.add_a(byte, false);
            },

            // ADD A, A
            0x87 => self.add_a(self.registers.a, false),

            // ADC A, B
            0x88 => self.add_a(self.registers.b, true),

            // ADC A, C
            0x89 => self.add_a(self.registers.c, true),

            // ADC A, D
            0x8A => self.add_a(self.registers.d, true),

            // ADC A, E
            0x8B => self.add_a(self.registers.e, true),

            // ADC A, H
            0x8C => self.add_a(self.registers.h, true),

            // ADC A, L
            0x8D => self.add_a(self.registers.l, true),

            // ADC A, (HL)
            0x8E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.add_a(byte, true);
            },

            // ADC A, A
            0x8F => self.add_a(self.registers.a, true),

            // SUB A, B
            0x90 => self.sub_a(self.registers.b, false),

            // SUB A, C
            0x91 => self.sub_a(self.registers.c, false),

            // SUB A, D
            0x92 => self.sub_a(self.registers.d, false),

            // SUB A, E
            0x93 => self.sub_a(self.registers.e, false),

            // SUB A, H
            0x94 => self.sub_a(self.registers.h, false),

            // SUB A, L
            0x95 => self.sub_a(self.registers.l, false),

            // SUB A, (HL)
            0x96 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.sub_a(byte, false);
            },

            // SUB A, A
            0x97 => self.sub_a(self.registers.a, false),

            // SBC A, B
            0x98 => self.sub_a(self.registers.b, true),

            // SBC A, C
            0x99 => self.sub_a(self.registers.c, true),

            // SBC A, D
            0x9A => self.sub_a(self.registers.d, true),

            // SBC A, E
            0x9B => self.sub_a(self.registers.e, true),

            // SBC A, H
            0x9C => self.sub_a(self.registers.h, true),

            // SBC A, L
            0x9D => self.sub_a(self.registers.l, true),

            // SBC A, (HL)
            0x9E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.sub_a(byte, true);
            },

            // SBC A, A
            0x9F => self.sub_a(self.registers.a, true),

            // AND A, B
            0xA0 => self.and_a(self.registers.b),

            // AND A, C
            0xA1 => self.and_a(self.registers.c),

            // AND A, D
            0xA2 => self.and_a(self.registers.d),

            // AND A, E
            0xA3 => self.and_a(self.registers.e),

            // AND A, H
            0xA4 => self.and_a(self.registers.h),

            // AND A, L
            0xA5 => self.and_a(self.registers.l),

            // AND A, (HL)
            0xA6 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.and_a(byte);
            },

            // AND A, A
            0xA7 => self.and_a(self.registers.a),

            // XOR A, B
            0xA8 => self.xor_a(self.registers.b),

            // XOR A, C
            0xA9 => self.xor_a(self.registers.c),

            // XOR A, D
            0xAA => self.xor_a(self.registers.d),

            // XOR A, E
            0xAB => self.xor_a(self.registers.e),

            // XOR A, H
            0xAC => self.xor_a(self.registers.h),

            // XOR A, L
            0xAD => self.xor_a(self.registers.l),

            // XOR A, (HL)
            0xAE => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.xor_a(byte);
            },

            // XOR A, A
            0xAF => self.xor_a(self.registers.a),

            // OR A, B
            0xB0 => self.or_a(self.registers.b),

            // OR A, C
            0xB1 => self.or_a(self.registers.c),

            // OR A, D
            0xB2 => self.or_a(self.registers.d),

            // OR A, E
            0xB3 => self.or_a(self.registers.e),

            // OR A, H
            0xB4 => self.or_a(self.registers.h),

            // OR A, L
            0xB5 => self.or_a(self.registers.l),

            // OR A, (HL)
            0xB6 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.or_a(byte);
            },

            // OR A, A
            0xB7 => self.or_a(self.registers.a),

            // CP A, B
            0xB8 => self.cp_a(self.registers.b),

            // CP A, C
            0xB9 => self.cp_a(self.registers.c),

            // CP A, D
            0xBA => self.cp_a(self.registers.d),

            // CP A, E
            0xBB => self.cp_a(self.registers.e),

            // CP A, H
            0xBC => self.cp_a(self.registers.h),

            // CP A, L
            0xBD => self.cp_a(self.registers.l),

            // CP A, (HL)
            0xBE => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.cp_a(byte);
            },

            // CP A, A
            0xBF => self.cp_a(self.registers.a),

            // RET NZ
            0xC0 => self.ret(Condition::NotZero),

            // POP BC
            0xC1 => {
                let word = self.pop_stack();
                self.registers.set_bc(word);
            },

            // JP NZ, a16
            0xC2 => self.jump(Condition::NotZero),

            // JP a16
            0xC3 => self.jump(Condition::None),

            // CALL NZ, a16
            0xC4 => self.call(Condition::NotZero),

            // PUSH BC
            0xC5 => self.push_stack(self.registers.bc()),

            // ADD A, d8
            0xC6 => {
                let value = self.next_byte();
                self.add_a(value, false);
            },

            // RST 0
            0xC7 => self.rst(0x0000),

            // RET Z
            0xC8 => self.ret(Condition::Zero),

            // RET
            0xC9 => self.ret(Condition::None),

            // JP Z, a16
            0xCA => self.jump(Condition::Zero),

            // CB
            0xCB => self.step_cb(),

            // CALL Z, a16
            0xCC => self.call(Condition::Zero),

            // CALL a16
            0xCD => self.call(Condition::None),

            // ADC A, d8
            0xCE => {
                let value = self.next_byte();
                self.add_a(value, true);
            },

            // RST 1
            0xCF => self.rst(0x0008),

            // RET NC
            0xD0 => self.ret(Condition::NotCarry),

            // POP DE
            0xD1 => {
                let word = self.pop_stack();
                self.registers.set_de(word);
            },

            // JP NC, a16
            0xD2 => self.jump(Condition::NotCarry),

            // CALL NC, a16
            0xD4 => self.call(Condition::NotCarry),

            // PUSH DE
            0xD5 => self.push_stack(self.registers.de()),

            // SUB A, d8
            0xD6 => {
                let value = self.next_byte();
                self.sub_a(value, false);
            },

            // RST 2
            0xD7 => self.rst(0x0010),

            // RET C
            0xD8 => self.ret(Condition::Carry),

            // RETI
            0xD9 => {
                self.ei_timer = 1;
                self.ret(Condition::None);
            },

            // JP C, a16
            0xDA => self.jump(Condition::Carry),

            // CALL C, a16
            0xDC => self.call(Condition::Carry),

            // SBC A, d8
            0xDE => {
                let value = self.next_byte();
                self.sub_a(value, true);
            },

            // RST 3
            0xDF => self.rst(0x0018),

            // LD (a8), A
            0xE0 => {
                let address = 0xFF00 | self.next_byte() as u16;
                self.mmu.write_cycle(address, self.registers.a);
            },

            // POP HL
            0xE1 => {
                let word = self.pop_stack();
                self.registers.set_hl(word);
            },

            // LD (C), A
            0xE2 => {
                let address = 0xFF00 | self.registers.c as u16;
                self.mmu.write_cycle(address, self.registers.a);
            },

            // PUSH HL
            0xE5 => self.push_stack(self.registers.hl()),

            // AND A, d8
            0xE6 => {
                let value = self.next_byte();
                self.and_a(value);
            },

            // RST 4
            0xE7 => self.rst(0x0020),

            // ADD SP, s8
            0xE8 => {
                let sp = self.registers.sp;
                let offset = self.next_byte() as i8 as i16 as u16;

                self.registers.set_flag(FlagMask::Zero, false);
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, (sp & 0x000F) + (offset & 0x000F) > 0x000F);
                self.registers.set_flag(FlagMask::Carry, (sp & 0x00FF) + (offset & 0x00FF) > 0x00FF);

                self.registers.sp = sp.wrapping_add(offset);

                self.mmu.cycle(8);
            },

            // JP HL
            0xE9 => self.registers.pc = self.registers.hl(),

            // LD (a16), A
            0xEA => {
                let address = self.next_word();
                self.mmu.write_cycle(address, self.registers.a);
            },

            // XOR A, d8
            0xEE => {
                let value = self.next_byte();
                self.xor_a(value);
            },

            // RST 5
            0xEF => self.rst(0x0028),

            // LD A, (a8)
            0xF0 => {
                let address = 0xFF00 | self.next_byte() as u16;
                self.registers.a = self.mmu.read_cycle(address);
            },

            // POP AF
            0xF1 => {
                let word = self.pop_stack();
                self.registers.set_af(word);
            },

            // LD A, (C)
            0xF2 => {
                let address = 0xFF00 | self.registers.c as u16;
                self.registers.a = self.mmu.read_cycle(address);
            },

            // DI
            0xF3 => self.di_timer = 1,

            // PUSH AF
            0xF5 => self.push_stack(self.registers.af()),

            // OR A, d8
            0xF6 => {
                let value = self.next_byte();
                self.or_a(value);
            },

            // RST 6
            0xF7 => self.rst(0x0030),

            // LD HL, SP+s8
            0xF8 => {
                let sp = self.registers.sp;
                let offset = self.next_byte() as i8 as i16 as u16;

                self.registers.set_flag(FlagMask::Zero, false);
                self.registers.set_flag(FlagMask::Subtract, false);
                self.registers.set_flag(FlagMask::HalfCarry, (sp & 0x000F) + (offset & 0x000F) > 0x000F);
                self.registers.set_flag(FlagMask::Carry, (sp & 0x00FF) + (offset & 0x00FF) > 0x00FF);

                self.registers.set_hl(sp.wrapping_add(offset));

                self.mmu.cycle(4);
            },

            // LD SP, HL
            0xF9 => {
                self.registers.sp = self.registers.hl();
                self.mmu.cycle(4);
            },

            // LD A, (a16)
            0xFA => {
                let address = self.next_word();
                self.registers.a = self.mmu.read_cycle(address);
            }

            // EI
            0xFB => self.ei_timer = 2,

            // CP A, d8
            0xFE => {
                let value = self.next_byte();
                self.cp_a(value);
            },

            // RST 7
            0xFF => self.rst(0x0038),

            _ => return Err(CPUError::UnsupportedOpcode(opcode, self.registers.pc)),
        }

        Ok(())
    }

    fn step_cb(&mut self) {
        let cb_opcode = self.next_byte();

        match cb_opcode {
            // RLC B
            0x00 => self.registers.b = self.rotate_left(self.registers.b, false, true),

            // RLC C
            0x01 => self.registers.c = self.rotate_left(self.registers.c, false, true),

            // RLC D
            0x02 => self.registers.d = self.rotate_left(self.registers.d, false, true),

            // RLC E
            0x03 => self.registers.e = self.rotate_left(self.registers.e, false, true),

            // RLC H
            0x04 => self.registers.h = self.rotate_left(self.registers.h, false, true),

            // RLC L
            0x05 => self.registers.l = self.rotate_left(self.registers.l, false, true),

            // RLC (HL)
            0x06 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.rotate_left(byte, false, true);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RLC A
            0x07 => self.registers.a = self.rotate_left(self.registers.a, false, true),

            // RRC B
            0x08 => self.registers.b = self.rotate_right(self.registers.b, false, true),

            // RRC C
            0x09 => self.registers.c = self.rotate_right(self.registers.c, false, true),

            // RRC D
            0x0A => self.registers.d = self.rotate_right(self.registers.d, false, true),

            // RRC E
            0x0B => self.registers.e = self.rotate_right(self.registers.e, false, true),

            // RRC H
            0x0C => self.registers.h = self.rotate_right(self.registers.h, false, true),

            // RRC L
            0x0D => self.registers.l = self.rotate_right(self.registers.l, false, true),

            // RRC (HL)
            0x0E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.rotate_right(byte, false, true);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RRC A
            0x0F => self.registers.a = self.rotate_right(self.registers.a, false, true),

            // RL B
            0x10 => self.registers.b = self.rotate_left(self.registers.b, true, true),

            // RL C
            0x11 => self.registers.c = self.rotate_left(self.registers.c, true, true),

            // RL D
            0x12 => self.registers.d = self.rotate_left(self.registers.d, true, true),

            // RL E
            0x13 => self.registers.e = self.rotate_left(self.registers.e, true, true),

            // RL H
            0x14 => self.registers.h = self.rotate_left(self.registers.h, true, true),

            // RL L
            0x15 => self.registers.l = self.rotate_left(self.registers.l, true, true),

            // RL (HL)
            0x16 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.rotate_left(byte, true, true);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RL A
            0x17 => self.registers.a = self.rotate_left(self.registers.a, true, true),

            // RR B
            0x18 => self.registers.b = self.rotate_right(self.registers.b, true, true),

            // RR C
            0x19 => self.registers.c = self.rotate_right(self.registers.c, true, true),

            // RR D
            0x1A => self.registers.d = self.rotate_right(self.registers.d, true, true),

            // RR E
            0x1B => self.registers.e = self.rotate_right(self.registers.e, true, true),

            // RR H
            0x1C => self.registers.h = self.rotate_right(self.registers.h, true, true),

            // RR L
            0x1D => self.registers.l = self.rotate_right(self.registers.l, true, true),

            // RR (HL)
            0x1E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.rotate_right(byte, true, true);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RR A
            0x1F => self.registers.a = self.rotate_right(self.registers.a, true, true),

            // SLA B
            0x20 => self.registers.b = self.shift_left_arithmetic(self.registers.b),

            // SLA C
            0x21 => self.registers.c = self.shift_left_arithmetic(self.registers.c),

            // SLA D
            0x22 => self.registers.d = self.shift_left_arithmetic(self.registers.d),

            // SLA E
            0x23 => self.registers.e = self.shift_left_arithmetic(self.registers.e),

            // SLA H
            0x24 => self.registers.h = self.shift_left_arithmetic(self.registers.h),

            // SLA L
            0x25 => self.registers.l = self.shift_left_arithmetic(self.registers.l),

            // SLA (HL)
            0x26 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.shift_left_arithmetic(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SLA A
            0x27 => self.registers.a = self.shift_left_arithmetic(self.registers.a),

            // SRA B
            0x28 => self.registers.b = self.shift_right_arithmetic(self.registers.b),

            // SRA C
            0x29 => self.registers.c = self.shift_right_arithmetic(self.registers.c),

            // SRA D
            0x2A => self.registers.d = self.shift_right_arithmetic(self.registers.d),

            // SRA E
            0x2B => self.registers.e = self.shift_right_arithmetic(self.registers.e),

            // SRA H
            0x2C => self.registers.h = self.shift_right_arithmetic(self.registers.h),

            // SRA L
            0x2D => self.registers.l = self.shift_right_arithmetic(self.registers.l),

            // SRA (HL)
            0x2E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.shift_right_arithmetic(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SRA A
            0x2F => self.registers.a = self.shift_right_arithmetic(self.registers.a),

            // SWAP B
            0x30 => self.registers.b = self.swap(self.registers.b),

            // SWAP C
            0x31 => self.registers.c = self.swap(self.registers.c),

            // SWAP D
            0x32 => self.registers.d = self.swap(self.registers.d),

            // SWAP E
            0x33 => self.registers.e = self.swap(self.registers.e),

            // SWAP H
            0x34 => self.registers.h = self.swap(self.registers.h),

            // SWAP L
            0x35 => self.registers.l = self.swap(self.registers.l),

            // SWAP (HL)
            0x36 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.swap(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SWAP A
            0x37 => self.registers.a = self.swap(self.registers.a),

            // SRL B
            0x38 => self.registers.b = self.shift_right_logical(self.registers.b),

            // SRL C
            0x39 => self.registers.c = self.shift_right_logical(self.registers.c),

            // SRL D
            0x3A => self.registers.d = self.shift_right_logical(self.registers.d),

            // SRL E
            0x3B => self.registers.e = self.shift_right_logical(self.registers.e),

            // SRL H
            0x3C => self.registers.h = self.shift_right_logical(self.registers.h),

            // SRL L
            0x3D => self.registers.l = self.shift_right_logical(self.registers.l),

            // SRL (HL)
            0x3E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.shift_right_logical(byte);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SRL A
            0x3F => self.registers.a = self.shift_right_logical(self.registers.a),

            // BIT 0, B
            0x40 => self.bit_test(self.registers.b, 0),

            // BIT 0, C
            0x41 => self.bit_test(self.registers.c, 0),

            // BIT 0, D
            0x42 => self.bit_test(self.registers.d, 0),
            
            // BIT 0, E
            0x43 => self.bit_test(self.registers.e, 0),

            // BIT 0, H
            0x44 => self.bit_test(self.registers.h, 0),

            // BIT 0, L
            0x45 => self.bit_test(self.registers.l, 0),

            // BIT 0, (HL)
            0x46 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 0);
            },

            // BIT 0, A
            0x47 => self.bit_test(self.registers.a, 0),

            // BIT 1, B
            0x48 => self.bit_test(self.registers.b, 1),

            // BIT 1, C
            0x49 => self.bit_test(self.registers.c, 1),

            // BIT 1, D
            0x4A => self.bit_test(self.registers.d, 1),
            
            // BIT 1, E
            0x4B => self.bit_test(self.registers.e, 1),

            // BIT 1, H
            0x4C => self.bit_test(self.registers.h, 1),

            // BIT 1, L
            0x4D => self.bit_test(self.registers.l, 1),

            // BIT 1, (HL)
            0x4E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 1);
            },

            // BIT 1, A
            0x4F => self.bit_test(self.registers.a, 1),

            // BIT 2, B
            0x50 => self.bit_test(self.registers.b, 2),

            // BIT 2, C
            0x51 => self.bit_test(self.registers.c, 2),

            // BIT 2, D
            0x52 => self.bit_test(self.registers.d, 2),
            
            // BIT 2, E
            0x53 => self.bit_test(self.registers.e, 2),

            // BIT 2, H
            0x54 => self.bit_test(self.registers.h, 2),

            // BIT 2, L
            0x55 => self.bit_test(self.registers.l, 2),

            // BIT 2, (HL)
            0x56 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 2);
            },

            // BIT 2, A
            0x57 => self.bit_test(self.registers.a, 2),

            // BIT 3, B
            0x58 => self.bit_test(self.registers.b, 3),

            // BIT 3, C
            0x59 => self.bit_test(self.registers.c, 3),

            // BIT 3, D
            0x5A => self.bit_test(self.registers.d, 3),
            
            // BIT 3, E
            0x5B => self.bit_test(self.registers.e, 3),

            // BIT 3, H
            0x5C => self.bit_test(self.registers.h, 3),

            // BIT 3, L
            0x5D => self.bit_test(self.registers.l, 3),

            // BIT 3, (HL)
            0x5E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 3);
            },

            // BIT 3, A
            0x5F => self.bit_test(self.registers.a, 3),

            // BIT 4, B
            0x60 => self.bit_test(self.registers.b, 4),

            // BIT 4, C
            0x61 => self.bit_test(self.registers.c, 4),

            // BIT 4, D
            0x62 => self.bit_test(self.registers.d, 4),
            
            // BIT 4, E
            0x63 => self.bit_test(self.registers.e, 4),

            // BIT 4, H
            0x64 => self.bit_test(self.registers.h, 4),

            // BIT 4, L
            0x65 => self.bit_test(self.registers.l, 4),

            // BIT 4, (HL)
            0x66 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 4);
            },

            // BIT 4, A
            0x67 => self.bit_test(self.registers.a, 4),

            // BIT 5, B
            0x68 => self.bit_test(self.registers.b, 5),

            // BIT 5, C
            0x69 => self.bit_test(self.registers.c, 5),

            // BIT 5, D
            0x6A => self.bit_test(self.registers.d, 5),
            
            // BIT 5, E
            0x6B => self.bit_test(self.registers.e, 5),

            // BIT 5, H
            0x6C => self.bit_test(self.registers.h, 5),

            // BIT 5, L
            0x6D => self.bit_test(self.registers.l, 5),

            // BIT 5, (HL)
            0x6E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 5);
            },

            // BIT 5, A
            0x6F => self.bit_test(self.registers.a, 5),

            // BIT 6, B
            0x70 => self.bit_test(self.registers.b, 6),

            // BIT 6, C
            0x71 => self.bit_test(self.registers.c, 6),

            // BIT 6, D
            0x72 => self.bit_test(self.registers.d, 6),
            
            // BIT 6, E
            0x73 => self.bit_test(self.registers.e, 6),

            // BIT 6, H
            0x74 => self.bit_test(self.registers.h, 6),

            // BIT 6, L
            0x75 => self.bit_test(self.registers.l, 6),

            // BIT 6, (HL)
            0x76 => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 6);
            },

            // BIT 6, A
            0x77 => self.bit_test(self.registers.a, 6),

            // BIT 7, B
            0x78 => self.bit_test(self.registers.b, 7),

            // BIT 7, C
            0x79 => self.bit_test(self.registers.c, 7),

            // BIT 7, D
            0x7A => self.bit_test(self.registers.d, 7),
            
            // BIT 7, E
            0x7B => self.bit_test(self.registers.e, 7),

            // BIT 7, H
            0x7C => self.bit_test(self.registers.h, 7),

            // BIT 7, L
            0x7D => self.bit_test(self.registers.l, 7),

            // BIT 7, (HL)
            0x7E => {
                let byte = self.mmu.read_cycle(self.registers.hl());
                self.bit_test(byte, 7);
            },

            // BIT 7, A
            0x7F => self.bit_test(self.registers.a, 7),

            // RES 0, B
            0x80 => self.registers.b = self.bit_reset(self.registers.b, 0),

            // RES 0, C
            0x81 => self.registers.c = self.bit_reset(self.registers.c, 0),

            // RES 0, D
            0x82 => self.registers.d = self.bit_reset(self.registers.d, 0),

            // RES 0, E
            0x83 => self.registers.e = self.bit_reset(self.registers.e, 0),

            // RES 0, H
            0x84 => self.registers.h = self.bit_reset(self.registers.h, 0),

            // RES 0, L
            0x85 => self.registers.l = self.bit_reset(self.registers.l, 0),

            // RES 0, (HL)
            0x86 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 0);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 0, A
            0x87 => self.registers.a = self.bit_reset(self.registers.a, 0),

            // RES 1, B
            0x88 => self.registers.b = self.bit_reset(self.registers.b, 1),

            // RES 1, C
            0x89 => self.registers.c = self.bit_reset(self.registers.c, 1),

            // RES 1, D
            0x8A => self.registers.d = self.bit_reset(self.registers.d, 1),

            // RES 1, E
            0x8B => self.registers.e = self.bit_reset(self.registers.e, 1),

            // RES 1, H
            0x8C => self.registers.h = self.bit_reset(self.registers.h, 1),

            // RES 1, L
            0x8D => self.registers.l = self.bit_reset(self.registers.l, 1),

            // RES 1, (HL)
            0x8E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 1);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 1, A
            0x8F => self.registers.a = self.bit_reset(self.registers.a, 1),

            // RES 2, B
            0x90 => self.registers.b = self.bit_reset(self.registers.b, 2),

            // RES 2, C
            0x91 => self.registers.c = self.bit_reset(self.registers.c, 2),

            // RES 2, D
            0x92 => self.registers.d = self.bit_reset(self.registers.d, 2),

            // RES 2, E
            0x93 => self.registers.e = self.bit_reset(self.registers.e, 2),

            // RES 2, H
            0x94 => self.registers.h = self.bit_reset(self.registers.h, 2),

            // RES 2, L
            0x95 => self.registers.l = self.bit_reset(self.registers.l, 2),

            // RES 2, (HL)
            0x96 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 2);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 2, A
            0x97 => self.registers.a = self.bit_reset(self.registers.a, 2),

            // RES 3, B
            0x98 => self.registers.b = self.bit_reset(self.registers.b, 3),

            // RES 3, C
            0x99 => self.registers.c = self.bit_reset(self.registers.c, 3),

            // RES 3, D
            0x9A => self.registers.d = self.bit_reset(self.registers.d, 3),

            // RES 3, E
            0x9B => self.registers.e = self.bit_reset(self.registers.e, 3),

            // RES 3, H
            0x9C => self.registers.h = self.bit_reset(self.registers.h, 3),

            // RES 3, L
            0x9D => self.registers.l = self.bit_reset(self.registers.l, 3),

            // RES 3, (HL)
            0x9E => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 3);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 3, A
            0x9F => self.registers.a = self.bit_reset(self.registers.a, 3),

            // RES 4, B
            0xA0 => self.registers.b = self.bit_reset(self.registers.b, 4),

            // RES 4, C
            0xA1 => self.registers.c = self.bit_reset(self.registers.c, 4),

            // RES 4, D
            0xA2 => self.registers.d = self.bit_reset(self.registers.d, 4),

            // RES 4, E
            0xA3 => self.registers.e = self.bit_reset(self.registers.e, 4),

            // RES 4, H
            0xA4 => self.registers.h = self.bit_reset(self.registers.h, 4),

            // RES 4, L
            0xA5 => self.registers.l = self.bit_reset(self.registers.l, 4),

            // RES 4, (HL)
            0xA6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 4);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 4, A
            0xA7 => self.registers.a = self.bit_reset(self.registers.a, 4),

            // RES 5, B
            0xA8 => self.registers.b = self.bit_reset(self.registers.b, 5),

            // RES 5, C
            0xA9 => self.registers.c = self.bit_reset(self.registers.c, 5),

            // RES 5, D
            0xAA => self.registers.d = self.bit_reset(self.registers.d, 5),

            // RES 5, E
            0xAB => self.registers.e = self.bit_reset(self.registers.e, 5),

            // RES 5, H
            0xAC => self.registers.h = self.bit_reset(self.registers.h, 5),

            // RES 5, L
            0xAD => self.registers.l = self.bit_reset(self.registers.l, 5),

            // RES 5, (HL)
            0xAE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 5);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 5, A
            0xAF => self.registers.a = self.bit_reset(self.registers.a, 5),

            // RES 6, B
            0xB0 => self.registers.b = self.bit_reset(self.registers.b, 6),

            // RES 6, C
            0xB1 => self.registers.c = self.bit_reset(self.registers.c, 6),

            // RES 6, D
            0xB2 => self.registers.d = self.bit_reset(self.registers.d, 6),

            // RES 6, E
            0xB3 => self.registers.e = self.bit_reset(self.registers.e, 6),

            // RES 6, H
            0xB4 => self.registers.h = self.bit_reset(self.registers.h, 6),

            // RES 6, L
            0xB5 => self.registers.l = self.bit_reset(self.registers.l, 6),

            // RES 6, (HL)
            0xB6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 6);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 6, A
            0xB7 => self.registers.a = self.bit_reset(self.registers.a, 6),

            // RES 7, B
            0xB8 => self.registers.b = self.bit_reset(self.registers.b, 7),

            // RES 7, C
            0xB9 => self.registers.c = self.bit_reset(self.registers.c, 7),

            // RES 7, D
            0xBA => self.registers.d = self.bit_reset(self.registers.d, 7),

            // RES 7, E
            0xBB => self.registers.e = self.bit_reset(self.registers.e, 7),

            // RES 7, H
            0xBC => self.registers.h = self.bit_reset(self.registers.h, 7),

            // RES 7, L
            0xBD => self.registers.l = self.bit_reset(self.registers.l, 7),

            // RES 7, (HL)
            0xBE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_reset(byte, 7);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // RES 7, A
            0xBF => self.registers.a = self.bit_reset(self.registers.a, 7),

            // SET 0, B
            0xC0 => self.registers.b = self.bit_set(self.registers.b, 0),

            // SET 0, C
            0xC1 => self.registers.c = self.bit_set(self.registers.c, 0),

            // SET 0, D
            0xC2 => self.registers.d = self.bit_set(self.registers.d, 0),

            // SET 0, E
            0xC3 => self.registers.e = self.bit_set(self.registers.e, 0),

            // SET 0, H
            0xC4 => self.registers.h = self.bit_set(self.registers.h, 0),

            // SET 0, L
            0xC5 => self.registers.l = self.bit_set(self.registers.l, 0),

            // SET 0, (HL)
            0xC6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 0);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 0, A
            0xC7 => self.registers.a = self.bit_set(self.registers.a, 0),

            // SET 1, B
            0xC8 => self.registers.b = self.bit_set(self.registers.b, 1),

            // SET 1, C
            0xC9 => self.registers.c = self.bit_set(self.registers.c, 1),

            // SET 1, D
            0xCA => self.registers.d = self.bit_set(self.registers.d, 1),

            // SET 1, E
            0xCB => self.registers.e = self.bit_set(self.registers.e, 1),

            // SET 1, H
            0xCC => self.registers.h = self.bit_set(self.registers.h, 1),

            // SET 1, L
            0xCD => self.registers.l = self.bit_set(self.registers.l, 1),

            // SET 1, (HL)
            0xCE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 1);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 1, A
            0xCF => self.registers.a = self.bit_set(self.registers.a, 1),

            // SET 2, B
            0xD0 => self.registers.b = self.bit_set(self.registers.b, 2),

            // SET 2, C
            0xD1 => self.registers.c = self.bit_set(self.registers.c, 2),

            // SET 2, D
            0xD2 => self.registers.d = self.bit_set(self.registers.d, 2),

            // SET 2, E
            0xD3 => self.registers.e = self.bit_set(self.registers.e, 2),

            // SET 2, H
            0xD4 => self.registers.h = self.bit_set(self.registers.h, 2),

            // SET 2, L
            0xD5 => self.registers.l = self.bit_set(self.registers.l, 2),

            // SET 2, (HL)
            0xD6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 2);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 2, A
            0xD7 => self.registers.a = self.bit_set(self.registers.a, 2),

            // SET 3, B
            0xD8 => self.registers.b = self.bit_set(self.registers.b, 3),

            // SET 3, C
            0xD9 => self.registers.c = self.bit_set(self.registers.c, 3),

            // SET 3, D
            0xDA => self.registers.d = self.bit_set(self.registers.d, 3),

            // SET 3, E
            0xDB => self.registers.e = self.bit_set(self.registers.e, 3),

            // SET 3, H
            0xDC => self.registers.h = self.bit_set(self.registers.h, 3),

            // SET 3, L
            0xDD => self.registers.l = self.bit_set(self.registers.l, 3),

            // SET 3, (HL)
            0xDE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 3);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 3, A
            0xDF => self.registers.a = self.bit_set(self.registers.a, 3),

            // SET 4, B
            0xE0 => self.registers.b = self.bit_set(self.registers.b, 4),

            // SET 4, C
            0xE1 => self.registers.c = self.bit_set(self.registers.c, 4),

            // SET 4, D
            0xE2 => self.registers.d = self.bit_set(self.registers.d, 4),

            // SET 4, E
            0xE3 => self.registers.e = self.bit_set(self.registers.e, 4),

            // SET 4, H
            0xE4 => self.registers.h = self.bit_set(self.registers.h, 4),

            // SET 4, L
            0xE5 => self.registers.l = self.bit_set(self.registers.l, 4),

            // SET 4, (HL)
            0xE6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 4);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 4, A
            0xE7 => self.registers.a = self.bit_set(self.registers.a, 4),

            // SET 5, B
            0xE8 => self.registers.b = self.bit_set(self.registers.b, 5),

            // SET 5, C
            0xE9 => self.registers.c = self.bit_set(self.registers.c, 5),

            // SET 5, D
            0xEA => self.registers.d = self.bit_set(self.registers.d, 5),

            // SET 5, E
            0xEB => self.registers.e = self.bit_set(self.registers.e, 5),

            // SET 5, H
            0xEC => self.registers.h = self.bit_set(self.registers.h, 5),

            // SET 5, L
            0xED => self.registers.l = self.bit_set(self.registers.l, 5),

            // SET 5, (HL)
            0xEE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 5);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 5, A
            0xEF => self.registers.a = self.bit_set(self.registers.a, 5),

            // SET 6, B
            0xF0 => self.registers.b = self.bit_set(self.registers.b, 6),

            // SET 6, C
            0xF1 => self.registers.c = self.bit_set(self.registers.c, 6),

            // SET 6, D
            0xF2 => self.registers.d = self.bit_set(self.registers.d, 6),

            // SET 6, E
            0xF3 => self.registers.e = self.bit_set(self.registers.e, 6),

            // SET 6, H
            0xF4 => self.registers.h = self.bit_set(self.registers.h, 6),

            // SET 6, L
            0xF5 => self.registers.l = self.bit_set(self.registers.l, 6),

            // SET 6, (HL)
            0xF6 => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 6);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 6, A
            0xF7 => self.registers.a = self.bit_set(self.registers.a, 6),

            // SET 7, B
            0xF8 => self.registers.b = self.bit_set(self.registers.b, 7),

            // SET 7, C
            0xF9 => self.registers.c = self.bit_set(self.registers.c, 7),

            // SET 7, D
            0xFA => self.registers.d = self.bit_set(self.registers.d, 7),

            // SET 7, E
            0xFB => self.registers.e = self.bit_set(self.registers.e, 7),

            // SET 7, H
            0xFC => self.registers.h = self.bit_set(self.registers.h, 7),

            // SET 7, L
            0xFD => self.registers.l = self.bit_set(self.registers.l, 7),

            // SET 7, (HL)
            0xFE => {
                let mut byte = self.mmu.read_cycle(self.registers.hl());
                byte = self.bit_set(byte, 7);
                self.mmu.write_cycle(self.registers.hl(), byte);
            },

            // SET 7, A
            0xFF => self.registers.a = self.bit_set(self.registers.a, 7)
        }
    }

    fn add_to_hl(&mut self, value: u16) {
        let hl = self.registers.hl();
        let sum = hl.wrapping_add(value);

        self.registers.set_flag(FlagMask::Subtract, false);
        self.registers.set_flag(FlagMask::HalfCarry, (hl & 0x0FFF) + (value & 0x0FFF) > 0x0FFF);
        self.registers.set_flag(FlagMask::Carry, hl > 0xFFFF - value);
        self.registers.set_hl(sum);

        self.mmu.cycle(4);
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

    fn jump_relative(&mut self, condition: Condition) {
        let jump = (self.next_byte() as i8) as i16;

        if self.evaluate(condition) {
            self.registers.pc = self.registers.pc.wrapping_add_signed(jump);
            self.mmu.cycle(4);
        }
    }

    fn jump(&mut self, condition: Condition) {
        let jump = self.next_word();

        if self.evaluate(condition) {
            self.registers.pc = jump;
            self.mmu.cycle(4);
        }
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

    fn call(&mut self, condition: Condition) {
        let jump = self.next_word();

        if self.evaluate(condition) {
            let address = self.registers.pc;
            self.push_stack(address);
            self.registers.pc = jump;
        }
    }

    fn ret(&mut self, condition: Condition) {
        if self.evaluate(condition) {
            if condition != Condition::None {
                self.mmu.cycle(4);
            }

            self.registers.pc = self.pop_stack();
            self.mmu.cycle(4);
            return;
        }

        self.mmu.cycle(4);
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
        let lower = self.mmu.read_cycle(self.registers.sp);
        self.registers.inc_sp();
        let upper = self.mmu.read_cycle(self.registers.sp);
        self.registers.inc_sp();

        u16::from_le_bytes([lower, upper])
    }

    fn push_stack(&mut self, value: u16) {
        let [upper, lower] = value.to_be_bytes();

        self.mmu.cycle(4);

        self.registers.dec_sp();
        self.mmu.write_cycle(self.registers.sp, upper);
        self.registers.dec_sp();
        self.mmu.write_cycle(self.registers.sp, lower);
    }

    fn next_byte(&mut self) -> u8 {
        let byte = self.mmu.read_cycle(self.registers.pc);

        self.registers.inc_pc(1);

        byte
    }

    fn next_word(&mut self) -> u16 {
        let bytes = [
            self.mmu.read_cycle(self.registers.pc),
            self.mmu.read_cycle(self.registers.pc.wrapping_add(1)),
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

    fn handle_interrupts(&mut self) -> bool {
        if self.mmu.interrupt_enabled() {
            // println!("Unhalt!");
            self.halted = false;

            if self.interrupt_master_enable {
                self.interrupt_master_enable = false;

                self.mmu.cycle(8);

                if self.mmu.interrupt_flag(Interrupt::VBlank) {
                    self.push_stack(self.registers.pc);
                    self.registers.pc = 0x0040;
                    self.mmu.unflag_interrupt(Interrupt::VBlank);
                    return true;
                }

                if self.mmu.interrupt_flag(Interrupt::LCD) {
                    self.push_stack(self.registers.pc);
                    self.registers.pc = 0x0048;
                    self.mmu.unflag_interrupt(Interrupt::LCD);
                    return true;
                }

                if self.mmu.interrupt_flag(Interrupt::Timer) {
                    self.push_stack(self.registers.pc);
                    self.registers.pc = 0x0050;
                    self.mmu.unflag_interrupt(Interrupt::Timer);
                    return true;
                }

                if self.mmu.interrupt_flag(Interrupt::Serial) {
                    self.push_stack(self.registers.pc);
                    self.registers.pc = 0x0058;
                    self.mmu.unflag_interrupt(Interrupt::Serial);
                    return true;
                }

                if self.mmu.interrupt_flag(Interrupt::Joypad) {
                    self.push_stack(self.registers.pc);
                    self.registers.pc = 0x0060;
                    self.mmu.unflag_interrupt(Interrupt::Joypad);
                    return true;
                }
            }
        }

        false
    }
}

impl std::fmt::Display for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}",
            self.registers.a,
            self.registers.f,
            self.registers.b,
            self.registers.c,
            self.registers.d,
            self.registers.e,
            self.registers.h,
            self.registers.l,
            self.registers.sp,
            self.registers.pc,
            self.read(self.registers.pc),
            self.read(self.registers.pc.wrapping_add(1)),
            self.read(self.registers.pc.wrapping_add(2)),
            self.read(self.registers.pc.wrapping_add(3))
        )
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum Condition {
    None,
    Zero,
    NotZero,
    Carry,
    NotCarry
}