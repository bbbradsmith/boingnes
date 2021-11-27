//
// boing.s
// Boing Ball NES demo by Brad Smith 2021
// http://rainwarrior.ca
//

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

// Boing Ball data

#include "temp/sprite.h"
extern uint8 amiga_nmt[1024];
extern uint8 atari_nmt[1024];

// NES library

extern uint8 input[2];
extern uint8 input_new[2];
extern uint8 cnrom_bank;
extern uint8 i,j,k,l;
extern uint16 mx,nx,ox,px;
#pragma zpsym("input")
#pragma zpsym("input_new")
#pragma zpsym("cnrom_bank")
#pragma zpsym("i")
#pragma zpsym("j")
#pragma zpsym("k")
#pragma zpsym("l")
#pragma zpsym("mx")
#pragma zpsym("nx")
#pragma zpsym("ox")
#pragma zpsym("px")

extern uint8 palette[32];
extern uint8 ppu_send[72];
extern uint8 oam[256];

extern void ppu_latch(uint16 addr);
extern void ppu_write(uint8 value);
extern void ppu_load(uint8* data, uint16 count);
extern void ppu_fill(uint8 value, uint16 count);
extern void ppu_packet(uint16 addr, uint8* data, uint8 count);
extern void ppu_packet_apply();
extern void ppu_ctrl(uint8 v);
extern void ppu_control_apply(uint8 v);
extern void ppu_mask(uint8 v);
extern void ppu_scroll(uint16 x, uint16 y);
extern void ppu_render_frame();
extern void ppu_wait_frame();
extern void ppu_render_off();
extern void ppu_nmi_off();
extern void ppu_nmi_on();

extern void sprite_begin();
extern void sprite_end();
extern void sprite_tile(uint8 t, uint8 a, uint8 x, uint8 y);
extern void sprite(uint8 e, uint8 x, uint8 y);

extern void sound_play(uint8* sound);
extern uint8 input_poll;
extern uint8 input_poll2();

void nescopy(void* dst, void* src, uint8 count);

// palettes

const uint8 pal[32] = {
	0x0F, 0x00, 0x04, 0x24, // Amiga BG
	0x0F, 0x0B, 0x2B, 0x3B, // Atari ST BG
	0x0F, 0x01, 0x11, 0x22, // (filler blue)
	0x0F, 0x03, 0x13, 0x23, // (filler purple)
	0x0F, 0x0F, 0x16, 0x30, // ball red-white
	0x0F, 0x0F, 0x30, 0x16, // ball white-red
	0x0F, 0x07, 0x17, 0x27, // (filler yellow)
	0x0F, 0x09, 0x19, 0x29, // (filler green)
};

// TODO test suite for all library functions (hold select+start at reset?)

void main()
{
	nescopy(palette,pal,32);
	ppu_latch(0x2000);
	ppu_load(amiga_nmt,1024);
	sprite_begin();
	sprite(0,128,120);
	sprite_end();
	ppu_render_frame();
	while(1) ppu_wait_frame();
}
