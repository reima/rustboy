//
// Instruction decoding
//

#[derive(Debug, Clone, Copy)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Clone, Copy)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Clone, Copy)]
pub enum Addr8 {
    Imm(u8),
    Ind(u8),
    Imm16Ind(u16),
    Reg8Dir(Reg8),
    Reg8Ind(Reg8),
    Reg16Ind(Reg16),
    Reg16IndInc(Reg16),
    Reg16IndDec(Reg16),
}

#[derive(Debug, Clone, Copy)]
pub enum Addr16 {
    Imm(u16),
    Ind(u16),
    Reg16Dir(Reg16),
}

#[derive(Debug, Copy, Clone)]
pub enum Cond {
    None,
    Z,
    NZ,
    C,
    NC,
}

#[derive(Debug, Clone)]
pub enum Op {
    // Misc/control
    Nop,

    Ei,
    Di,

    Halt,
    Stop(u8),

    // Jump/call
    Jp(Cond, Addr16),
    Jr(Cond, i8),

    Call(Cond, Addr16),
    Rst(u8),

    Ret(Cond),
    Reti,

    // Load/store/move
    Ld8(Addr8, Addr8),
    Ld16(Addr16, Addr16),
    Ldh(Addr8, Addr8),
    Ldhl(i8),

    Push(Addr16),
    Pop(Addr16),

    // Arithmetic/logic
    Add8(Addr8),
    Add16(Addr16, Addr16),
    Addsp(i8),
    Adc(Addr8),

    Sub(Addr8),
    Sbc(Addr8),

    Inc8(Addr8),
    Inc16(Addr16),
    Dec8(Addr8),
    Dec16(Addr16),

    And(Addr8),
    Or(Addr8),
    Xor(Addr8),

    Cp(Addr8),

    Cpl,

    Scf,
    Ccf,

    Daa,

    // Rotation/shift/bit
    Rlca,
    Rla,
    Rrca,
    Rra,

    Rlc(Addr8),
    Rl(Addr8),
    Rrc(Addr8),
    Rr(Addr8),

    Sla(Addr8),
    Sra(Addr8),
    Srl(Addr8),

    Bit(u8, Addr8),
    Res(u8, Addr8),
    Set(u8, Addr8),

    Swap(Addr8),

    // Undefined/illegal
    Undef(u8),
}

impl Op {
    pub fn decode<F: Fetch>(f: &mut F) -> Op {
        // Source: http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
        let fetchw = |f: &mut F| -> u16 {
            let lo = f.fetch();
            let hi = f.fetch();
            (u16::from(hi) << 8) | u16::from(lo)
        };

        let opcode = f.fetch();
        match opcode {
            // 0x00
            0x00 => Op::Nop,
            0x01 => {
                let imm = fetchw(f);
                Op::Ld16(Addr16::Reg16Dir(Reg16::BC), Addr16::Imm(imm))
            }
            0x02 => Op::Ld8(Addr8::Reg16Ind(Reg16::BC), Addr8::Reg8Dir(Reg8::A)),
            0x03 => Op::Inc16(Addr16::Reg16Dir(Reg16::BC)),
            0x04 => Op::Inc8(Addr8::Reg8Dir(Reg8::B)),
            0x05 => Op::Dec8(Addr8::Reg8Dir(Reg8::B)),
            0x06 => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::B), Addr8::Imm(imm))
            }
            0x07 => Op::Rlca,

            0x08 => {
                let ind = fetchw(f);
                Op::Ld16(Addr16::Ind(ind), Addr16::Reg16Dir(Reg16::SP))
            }
            0x09 => Op::Add16(Addr16::Reg16Dir(Reg16::HL), Addr16::Reg16Dir(Reg16::BC)),
            0x0a => Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Reg16Ind(Reg16::BC)),
            0x0b => Op::Dec16(Addr16::Reg16Dir(Reg16::BC)),
            0x0c => Op::Inc8(Addr8::Reg8Dir(Reg8::C)),
            0x0d => Op::Dec8(Addr8::Reg8Dir(Reg8::C)),
            0x0e => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::C), Addr8::Imm(imm))
            }
            0x0f => Op::Rrca,

            // 0x10
            0x10 => {
                let val = f.fetch();
                Op::Stop(val)
            }
            0x11 => {
                let imm = fetchw(f);
                Op::Ld16(Addr16::Reg16Dir(Reg16::DE), Addr16::Imm(imm))
            }
            0x12 => Op::Ld8(Addr8::Reg16Ind(Reg16::DE), Addr8::Reg8Dir(Reg8::A)),
            0x13 => Op::Inc16(Addr16::Reg16Dir(Reg16::DE)),
            0x14 => Op::Inc8(Addr8::Reg8Dir(Reg8::D)),
            0x15 => Op::Dec8(Addr8::Reg8Dir(Reg8::D)),
            0x16 => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::D), Addr8::Imm(imm))
            }
            0x17 => Op::Rla,

            0x18 => {
                let imm = f.fetch();
                Op::Jr(Cond::None, imm as i8)
            }
            0x19 => Op::Add16(Addr16::Reg16Dir(Reg16::HL), Addr16::Reg16Dir(Reg16::DE)),
            0x1a => Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Reg16Ind(Reg16::DE)),
            0x1b => Op::Dec16(Addr16::Reg16Dir(Reg16::DE)),
            0x1c => Op::Inc8(Addr8::Reg8Dir(Reg8::E)),
            0x1d => Op::Dec8(Addr8::Reg8Dir(Reg8::E)),
            0x1e => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::E), Addr8::Imm(imm))
            }
            0x1f => Op::Rra,

            // 0x20
            0x20 => {
                let imm = f.fetch();
                Op::Jr(Cond::NZ, imm as i8)
            }
            0x21 => {
                let imm = fetchw(f);
                Op::Ld16(Addr16::Reg16Dir(Reg16::HL), Addr16::Imm(imm))
            }
            0x22 => Op::Ld8(Addr8::Reg16IndInc(Reg16::HL), Addr8::Reg8Dir(Reg8::A)),
            0x23 => Op::Inc16(Addr16::Reg16Dir(Reg16::HL)),
            0x24 => Op::Inc8(Addr8::Reg8Dir(Reg8::H)),
            0x25 => Op::Dec8(Addr8::Reg8Dir(Reg8::H)),
            0x26 => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::H), Addr8::Imm(imm))
            }
            0x27 => Op::Daa,

            0x28 => {
                let imm = f.fetch();
                Op::Jr(Cond::Z, imm as i8)
            }
            0x29 => Op::Add16(Addr16::Reg16Dir(Reg16::HL), Addr16::Reg16Dir(Reg16::HL)),
            0x2a => Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Reg16IndInc(Reg16::HL)),
            0x2b => Op::Dec16(Addr16::Reg16Dir(Reg16::HL)),
            0x2c => Op::Inc8(Addr8::Reg8Dir(Reg8::L)),
            0x2d => Op::Dec8(Addr8::Reg8Dir(Reg8::L)),
            0x2e => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::L), Addr8::Imm(imm))
            }
            0x2f => Op::Cpl,

            // 0x30
            0x30 => {
                let imm = f.fetch();
                Op::Jr(Cond::NC, imm as i8)
            }
            0x31 => {
                let imm = fetchw(f);
                Op::Ld16(Addr16::Reg16Dir(Reg16::SP), Addr16::Imm(imm))
            }
            0x32 => Op::Ld8(Addr8::Reg16IndDec(Reg16::HL), Addr8::Reg8Dir(Reg8::A)),
            0x33 => Op::Inc16(Addr16::Reg16Dir(Reg16::SP)),
            0x34 => Op::Inc8(Addr8::Reg16Ind(Reg16::HL)),
            0x35 => Op::Dec8(Addr8::Reg16Ind(Reg16::HL)),
            0x36 => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg16Ind(Reg16::HL), Addr8::Imm(imm))
            }
            0x37 => Op::Scf,

            0x38 => {
                let imm = f.fetch();
                Op::Jr(Cond::C, imm as i8)
            }
            0x39 => Op::Add16(Addr16::Reg16Dir(Reg16::HL), Addr16::Reg16Dir(Reg16::SP)),
            0x3a => Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Reg16IndDec(Reg16::HL)),
            0x3b => Op::Dec16(Addr16::Reg16Dir(Reg16::SP)),
            0x3c => Op::Inc8(Addr8::Reg8Dir(Reg8::A)),
            0x3d => Op::Dec8(Addr8::Reg8Dir(Reg8::A)),
            0x3e => {
                let imm = f.fetch();
                Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Imm(imm))
            }
            0x3f => Op::Ccf,

            // 0x40-0x70
            0x40..=0x75 | 0x77..=0x7f => Op::Ld8(decode_addr(opcode >> 3), decode_addr(opcode)),
            0x76 => Op::Halt,

            // 0x80
            0x80..=0x87 => Op::Add8(decode_addr(opcode)),
            0x88..=0x8f => Op::Adc(decode_addr(opcode)),

            // 0x90
            0x90..=0x97 => Op::Sub(decode_addr(opcode)),
            0x98..=0x9f => Op::Sbc(decode_addr(opcode)),

            // 0xa0
            0xa0..=0xa7 => Op::And(decode_addr(opcode)),
            0xa8..=0xaf => Op::Xor(decode_addr(opcode)),

            // 0xb0
            0xb0..=0xb7 => Op::Or(decode_addr(opcode)),
            0xb8..=0xbf => Op::Cp(decode_addr(opcode)),

            // 0xc0
            0xc0 => Op::Ret(Cond::NZ),
            0xc1 => Op::Pop(Addr16::Reg16Dir(Reg16::BC)),
            0xc2 => {
                let imm = fetchw(f);
                Op::Jp(Cond::NZ, Addr16::Imm(imm))
            }
            0xc3 => {
                let imm = fetchw(f);
                Op::Jp(Cond::None, Addr16::Imm(imm))
            }
            0xc4 => {
                let imm = fetchw(f);
                Op::Call(Cond::NZ, Addr16::Imm(imm))
            }
            0xc5 => Op::Push(Addr16::Reg16Dir(Reg16::BC)),
            0xc6 => {
                let imm = f.fetch();
                Op::Add8(Addr8::Imm(imm))
            }
            0xc7 => Op::Rst(0x00),

            0xc8 => Op::Ret(Cond::Z),
            0xc9 => Op::Ret(Cond::None),
            0xca => {
                let imm = fetchw(f);
                Op::Jp(Cond::Z, Addr16::Imm(imm))
            }
            0xcb => {
                let extra = f.fetch();
                let addr = decode_addr(extra);

                match extra & 0xf8 {
                    0x00 => Op::Rlc(addr),
                    0x08 => Op::Rrc(addr),
                    0x10 => Op::Rl(addr),
                    0x18 => Op::Rr(addr),
                    0x20 => Op::Sla(addr),
                    0x28 => Op::Sra(addr),
                    0x30 => Op::Swap(addr),
                    0x38 => Op::Srl(addr),
                    0x40..=0x78 => Op::Bit((extra >> 3) & 0b111, addr),
                    0x80..=0xb8 => Op::Res((extra >> 3) & 0b111, addr),
                    0xc0..=0xf8 => Op::Set((extra >> 3) & 0b111, addr),
                    _ => panic!("logic error"),
                }
            }
            0xcc => {
                let imm = fetchw(f);
                Op::Call(Cond::Z, Addr16::Imm(imm))
            }
            0xcd => {
                let imm = fetchw(f);
                Op::Call(Cond::None, Addr16::Imm(imm))
            }
            0xce => {
                let imm = f.fetch();
                Op::Adc(Addr8::Imm(imm))
            }
            0xcf => Op::Rst(0x08),

            // 0xd0
            0xd0 => Op::Ret(Cond::NC),
            0xd1 => Op::Pop(Addr16::Reg16Dir(Reg16::DE)),
            0xd2 => {
                let imm = fetchw(f);
                Op::Jp(Cond::NC, Addr16::Imm(imm))
            }
            /*0xd3 => Op::Nop,*/
            0xd4 => {
                let imm = fetchw(f);
                Op::Call(Cond::NC, Addr16::Imm(imm))
            }
            0xd5 => Op::Push(Addr16::Reg16Dir(Reg16::DE)),
            0xd6 => {
                let imm = f.fetch();
                Op::Sub(Addr8::Imm(imm))
            }
            0xd7 => Op::Rst(0x10),

            0xd8 => Op::Ret(Cond::C),
            0xd9 => Op::Reti,
            0xda => {
                let imm = fetchw(f);
                Op::Jp(Cond::C, Addr16::Imm(imm))
            }
            /*0xdb => Op::Nop,*/
            0xdc => {
                let imm = fetchw(f);
                Op::Call(Cond::C, Addr16::Imm(imm))
            }
            /*0xdd => d.nop(),*/
            0xde => {
                let imm = f.fetch();
                Op::Sbc(Addr8::Imm(imm))
            }
            0xdf => Op::Rst(0x18),

            // 0xe0
            0xe0 => {
                let ind = f.fetch();
                Op::Ldh(Addr8::Ind(ind), Addr8::Reg8Dir(Reg8::A))
            }
            0xe1 => Op::Pop(Addr16::Reg16Dir(Reg16::HL)),
            0xe2 => Op::Ld8(Addr8::Reg8Ind(Reg8::C), Addr8::Reg8Dir(Reg8::A)),
            /*0xe3 => Op::Nop,*/
            /*0xe4 => Op::Nop,*/
            0xe5 => Op::Push(Addr16::Reg16Dir(Reg16::HL)),
            0xe6 => {
                let imm = f.fetch();
                Op::And(Addr8::Imm(imm))
            }
            0xe7 => Op::Rst(0x20),

            0xe8 => {
                let imm = f.fetch();
                Op::Addsp(imm as i8)
            }
            0xe9 => Op::Jp(Cond::None, Addr16::Reg16Dir(Reg16::HL)),
            0xea => {
                let ind = fetchw(f);
                Op::Ld8(Addr8::Imm16Ind(ind), Addr8::Reg8Dir(Reg8::A))
            }
            /*0xeb => Op::Nop,*/
            /*0xec => Op::Nop,*/
            /*0xed => Op::Nop,*/
            0xee => {
                let imm = f.fetch();
                Op::Xor(Addr8::Imm(imm))
            }
            0xef => Op::Rst(0x28),

            // 0xf0
            0xf0 => {
                let ind = f.fetch();
                Op::Ldh(Addr8::Reg8Dir(Reg8::A), Addr8::Ind(ind))
            }
            0xf1 => Op::Pop(Addr16::Reg16Dir(Reg16::AF)),
            0xf2 => Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Reg8Ind(Reg8::C)),
            0xf3 => Op::Di,
            /*0xf4 => Op::Nop,*/
            0xf5 => Op::Push(Addr16::Reg16Dir(Reg16::AF)),
            0xf6 => {
                let imm = f.fetch();
                Op::Or(Addr8::Imm(imm))
            }
            0xf7 => Op::Rst(0x30),

            0xf8 => {
                let imm = f.fetch();
                Op::Ldhl(imm as i8)
            }
            0xf9 => Op::Ld16(Addr16::Reg16Dir(Reg16::SP), Addr16::Reg16Dir(Reg16::HL)),
            0xfa => {
                let ind = fetchw(f);
                Op::Ld8(Addr8::Reg8Dir(Reg8::A), Addr8::Imm16Ind(ind))
            }
            0xfb => Op::Ei,
            /*0xfc => Op::Nop,*/
            /*0xfd => Op::Nop,*/
            0xfe => {
                let imm = f.fetch();
                Op::Cp(Addr8::Imm(imm))
            }
            0xff => Op::Rst(0x38),

            _ => Op::Undef(opcode),
        }
    }
}

pub trait Fetch {
    fn fetch(&mut self) -> u8;
}

// Several instructions use a 3 bit part of the opcode to encode common
// operands, this method decodes the operand from the lower 3 bits
fn decode_addr(code: u8) -> Addr8 {
    match code & 0x07 {
        0x00 => Addr8::Reg8Dir(Reg8::B),
        0x01 => Addr8::Reg8Dir(Reg8::C),
        0x02 => Addr8::Reg8Dir(Reg8::D),
        0x03 => Addr8::Reg8Dir(Reg8::E),
        0x04 => Addr8::Reg8Dir(Reg8::H),
        0x05 => Addr8::Reg8Dir(Reg8::L),
        0x06 => Addr8::Reg16Ind(Reg16::HL),
        0x07 => Addr8::Reg8Dir(Reg8::A),
        _ => panic!("logic error"),
    }
}
