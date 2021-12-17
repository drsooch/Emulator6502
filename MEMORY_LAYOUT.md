# Memory Layout

The current layout for this 6502 emulator is as follows:

__Common to 6502__

- 0x0000 \- 0x00FF: Zero Page
- 0x0100 \- 0x01FF: Stack location
- 0xFFFA \- 0xFFFB: Non Maskable Interrupt Vector
- 0xFFFC \- 0xFFFD: Rest Routine
- 0xFFFE \- 0xFFFF: IRQ/BRK Vector

__Implementation Defined__

Since I am not aiming to emulate any particular system (BBC Micro, C64, SNES/NES), I have decided to construct the emulator with it's own layout as follows:

-- TODO --

