use crate::{
    cartridge::Cartridge,
    cpu::Cpu,
    interrupt::{InterruptCtrl, IRQ_LCD, IRQ_TIMER, IRQ_VBLANK},
    joypad::{Button, Joypad},
    mem::Mem,
    ram::WorkRam,
    serial::SerialIO,
    sound::Sound,
    timer::{Signal as TimerSignal, Timer},
    video::{Signal as VideoSignal, Video},
};

//
// Memory Map
//

struct Dummy;
impl Mem for Dummy {
    fn loadb(&self, _: u16) -> u8 {
        //debug!("load in unmapped memory at 0x{:04X}", addr);
        0xff
    }

    fn storeb(&mut self, _: u16, _: u8) {
        //debug!("store in unmapped memory at 0x{:04X}", addr);
    }
}

pub struct MemMap<'a> {
    cart: Cartridge,
    wram: WorkRam,
    timer: Timer,
    intr: InterruptCtrl,
    sound: Sound,
    video: Video,
    serial: SerialIO<'a>,
    joypad: Joypad,
    dummy: Dummy,
}

impl<'a> MemMap<'a> {
    fn mem_from_addr_mut(&mut self, addr: u16) -> &mut dyn Mem {
        match addr {
            0x0000..=0x7fff | // ROM banks
            0xa000..=0xbfff   // External RAM
                            => &mut self.cart as &mut dyn Mem,
            0x8000..=0x9fff | // VRAM
            0xfe00..=0xfe9f | // OAM
            0xff40..=0xff4b   // Video I/O
                            => &mut self.video as &mut dyn Mem,
            0xc000..=0xfdff | // WRAM (including echo area 0xe000-0xfdff)
            0xff80..=0xfffe   // HRAM
                            => &mut self.wram as &mut dyn Mem,
            0xff00           => &mut self.joypad as &mut dyn Mem,
            0xff01..=0xff02  => &mut self.serial as &mut dyn Mem,
            0xff04..=0xff07  => &mut self.timer as &mut dyn Mem,
            0xff0f | 0xffff  => &mut self.intr as &mut dyn Mem,
            0xff10..=0xff3f  => &mut self.sound as &mut dyn Mem,
            _ => &mut self.dummy as &mut dyn Mem,
        }
    }

    fn mem_from_addr(&self, addr: u16) -> &dyn Mem {
        match addr {
            0x0000..=0x7fff | // ROM banks
            0xa000..=0xbfff   // External RAM
                            => &self.cart as &dyn Mem,
            0x8000..=0x9fff | // VRAM
            0xfe00..=0xfe9f | // OAM
            0xff40..=0xff4b   // Video I/O
                            => &self.video as &dyn Mem,
            0xc000..=0xfdff | // WRAM (including echo area 0xe000-0xfdff)
            0xff80..=0xfffe   // HRAM
                            => &self.wram as &dyn Mem,
            0xff00           => &self.joypad as &dyn Mem,
            0xff01..=0xff02  => &self.serial as &dyn Mem,
            0xff04..=0xff07  => &self.timer as &dyn Mem,
            0xff0f | 0xffff  => &self.intr as &dyn Mem,
            0xff10..=0xff3f  => &self.sound as &dyn Mem,
            _ => &self.dummy as &dyn Mem,
        }
    }
}

impl<'a> Mem for MemMap<'a> {
    fn loadb(&self, addr: u16) -> u8 {
        self.mem_from_addr(addr).loadb(addr)
    }

    fn storeb(&mut self, addr: u16, val: u8) {
        self.mem_from_addr_mut(addr).storeb(addr, val)
    }
}

//
// Game Boy
//

pub struct GameBoy<'a> {
    cpu: Cpu,
    mem: MemMap<'a>,
}

impl<'a> GameBoy<'a> {
    pub fn new(cart: Cartridge) -> GameBoy<'a> {
        let mut cpu = Cpu::new();
        cpu.regs.pc = 0x100;

        let mem = MemMap {
            cart,
            wram: WorkRam::new(),
            timer: Timer::new(),
            intr: InterruptCtrl::new(),
            sound: Sound,
            video: Video::new(),
            serial: SerialIO::new(None),
            joypad: Joypad::new(),
            dummy: Dummy,
        };

        GameBoy { cpu, mem }
    }

    pub fn cpu(&self) -> &Cpu {
        &self.cpu
    }

    pub fn mem(&self) -> &dyn Mem {
        &self.mem
    }

    pub fn pixels(&self) -> &[u8] {
        &self.mem.video.screen
    }

    pub fn step(&mut self) -> bool {
        let cycles = self.cpu.step(&mut self.mem);

        if let Some(TimerSignal::TIMAOverflow) = self.mem.timer.tick(cycles) {
            self.mem.intr.irq(IRQ_TIMER);
        }

        let mut new_frame = false;
        let video_signals = self.mem.video.tick(cycles);
        for signal in &video_signals {
            match *signal {
                VideoSignal::Dma(base) => {
                    // Do DMA transfer instantaneously
                    let base_addr = u16::from(base) << 8;
                    for offset in 0x00u16..0xa0u16 {
                        let val = self.mem.loadb(base_addr + offset);
                        self.mem.storeb(0xfe00 + offset, val);
                    }
                }
                VideoSignal::VBlank => {
                    self.mem.intr.irq(IRQ_VBLANK);
                    new_frame = true;
                }
                VideoSignal::Lcd => self.mem.intr.irq(IRQ_LCD),
            }
        }

        new_frame
    }

    pub fn set_button(&mut self, button: Button, pressed: bool) {
        self.mem.joypad.set_button(button, pressed);
    }
}
