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

extern uint8 nmi_count;
extern uint8 input[2];
extern uint8 input_new[2];
extern uint8 ascii_offset;
extern uint8 cnrom_bank;
extern uint8 i,j,k,l;
extern uint16 mx,nx,ox,px;
#pragma zpsym("nmi_count")
#pragma zpsym("input")
#pragma zpsym("input_new")
#pragma zpsym("ascii_offset")
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
extern void ppu_latch_at(uint8 x, uint8 y);
extern void ppu_write(uint8 value);
extern void ppu_load(const uint8* data, uint16 count);
extern void ppu_fill(uint8 value, uint16 count);
extern void ppu_string(const char* s);
extern void ppu_packet(uint16 addr, const uint8* data, uint8 count);
extern void ppu_packet_apply();
extern void ppu_ctrl(uint8 v);
extern void ppu_control_apply(uint8 v);
extern void ppu_mask(uint8 v);
extern void ppu_mask_apply(uint8 v);
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
extern void sprite_flip(uint8 e, uint8 x, uint8 y);

extern void sound_play(const uint8* sound);
extern uint8 input_poll();
extern uint8 input_poll2();
void nescopy(void* dst, const void* src, uint8 count);

#define PAD_A       0x80
#define PAD_B       0x40
#define PAD_SELECT  0x20
#define PAD_START   0x10
#define PAD_U       0x08
#define PAD_D       0x04
#define PAD_L       0x02
#define PAD_R       0x01

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

// NES library test (press SELECT+START to activate)

const uint8 hextest[] = {
	0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,
	0x21,0x22,0x23,0x24,0x25,0x26 };

void libtest()
{
	static uint8 bx[2] = {128,128};
	static uint8 by[2] = {100,117};
	static uint16 tsx = 0;
	static uint16 tsy = 0;
	static uint8 hexpackets[6];

	cnrom_bank = 0;
	ascii_offset = 0x20;
	ppu_latch(0x2000);
	ppu_fill(0,1024);
	ppu_latch(0x2400);
	for (mx = 0; mx<1024; ++mx) ppu_write(mx & 0xFF);
	ppu_latch_at(4,4);
	ppu_string("NES Library Test");
	ppu_latch_at(32+8,32+22); // goes to "bottom right" screen
	ppu_string("OFFSCREEN");
	ppu_packet(0x2000+(6*32)+4,hextest,sizeof(hextest));
	ppu_packet_apply();
	while(1)
	{
		input_poll2();
		for (i=0;i<2;++i)
		{
			if (input[i] & PAD_L) --bx[i];
			if (input[i] & PAD_R) ++bx[i];
			if (input[i] & PAD_U) --by[i];
			if (input[i] & PAD_D) ++by[i];
		}
		sprite_begin();
		sprite(     SPRITE_boing,bx[0],by[0]);
		sprite_flip(SPRITE_boing,bx[1],by[1]);
		sprite_end();
		hexpackets[0] = hextest[bx[0]     >> 4];
		hexpackets[2] = hextest[by[0]     >> 4];
		hexpackets[4] = hextest[nmi_count >> 4];
		hexpackets[1] = hextest[bx[0]     & 0xF];
		hexpackets[3] = hextest[by[0]     & 0xF];
		hexpackets[5] = hextest[nmi_count & 0xF];
		ppu_packet(0x2000+( 8*32)+4,hexpackets+0,2);
		ppu_packet(0x2000+( 9*32)+4,hexpackets+2,2);
		ppu_packet(0x2000+(10*32)+4,hexpackets+4,2);
		if (input[0] & PAD_B) ++tsx;
		if (input[0] & PAD_A) ++tsy;
		if (input[0] & PAD_SELECT)
		{
			ppu_mask_apply(0x1F); // greyscale immediately
			ppu_mask(0x1E); // normal render at next NMI
		}
		if (input[1] & PAD_B) ppu_wait_frame(); // slow to half
		if (input_new[1] & PAD_A) // suspend NMI (can watch counter stop/resume)
		{
			ppu_nmi_off();
			input_poll2();
			while (!((input_new[0] | input_new[1]) & PAD_A))
				input_poll2();
			ppu_nmi_on();
		}
		ppu_scroll(tsx,tsy);
		ppu_render_frame();
		if ((input_new[0] | input_new[1]) & PAD_START) break;
	}
	ppu_render_off();
}

// Boing Ball

uint8 boing_bg = 0;
uint8 boing_par = 0;

uint16 boing_x = 128<<8;
uint16 boing_y = 120<<8;
uint16 boing_r = 0;

uint16 boing_vx = 0; // TODO
uint16 boing_vy = 0; // TODO
uint16 boing_vr = 256/2; // spin rate

uint8 boing_sprite = SPRITE_squ0000;

void boing_redraw()
{
	ppu_latch(0x2000);
	ppu_load(boing_bg ? atari_nmt : amiga_nmt,1024);
	ppu_scroll(0,0);
}

void boing_animate()
{
	boing_sprite = boing_par ? SPRITE_pau0000 : SPRITE_squ0000;
	boing_sprite += (boing_r >> 8) * 2; // rotation
	if (nmi_count & 1) boing_sprite += 1; // shadow

	boing_r += boing_vr;
	while (boing_r >= (128<<8)) boing_r += (12<<8); // wrap negative 0-11
	while (boing_r >= ( 12<<8)) boing_r -= (12<<8); // wrap positive 0-11
	
	// TODO vx/vy and bounce sound

	// TODO controls
	
	sprite_begin();
	sprite(boing_sprite,boing_x>>8,boing_y>>8); // TODO boing sprite
	sprite_end();
	cnrom_bank = boing_par;
}

// main loop

void main()
{
	nescopy(palette,pal,32);
	boing_redraw();

	while(1)
	{
		input_poll();
		// select+start for library test
		if ((input[0] & (PAD_SELECT | PAD_START)) == (PAD_SELECT | PAD_START) &&
		    (input_new[0] & (PAD_SELECT | PAD_START)))
		{
			ppu_render_off();
			libtest();
			boing_redraw();
		}
		// select for amiga/atari switch
		else if (input_new[0] &  PAD_SELECT)
		{
			ppu_render_off();
			boing_bg ^= 1;
			boing_redraw();
		}
		// B/A to select pixel-aspect-ratio
		if (input_new[0] & PAD_B) boing_par = 1;
		if (input_new[0] & PAD_A) boing_par = 0;
		boing_animate();
		ppu_render_frame();
	}
}
