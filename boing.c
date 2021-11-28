//
// boing.s
// Boing Ball NES demo by Brad Smith 2021
// http://rainwarrior.ca
//

#include "temp/sprite.h" // sprite enums

//
// NES library
//

typedef unsigned char      uint8;
typedef   signed char      sint8;
typedef unsigned short int uint16;
typedef   signed short int sint16;
typedef unsigned long  int uint32;
typedef   signed long  int sint32;

extern uint8 nmi_count;    // every NMI increments this value
extern uint8 input[2];     // gamepad inputs
extern uint8 input_new[2]; // new buttons pressed at last poll
extern uint8 ascii_offset; // offset from CHR index to ASCII in ppu_string
extern uint8 cnrom_bank;   // applies CNROM CHR bank at next o
extern uint8 i,j,k,l;      // convenient zeropage temporaries
extern uint16 mx,nx,ox,px; // 16-bit ZP temporaries
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

extern uint8 palette[32];  // NES render palette
extern uint8 ppu_send[72]; // data packets uploaded to PPU (see ppu_packet)
extern uint8 oam[256];     // sprite OAM data

// PPU updates while rendering is off
extern void ppu_latch(uint16 addr); // sets PPU write address
extern void ppu_latch_at(uint8 x, uint8 y); // latches a nametable tile (x>32=$2400, y>32=$2800, etc.)
extern void ppu_write(uint8 value); // writes one byte at latched address
extern void ppu_load(const uint8* data, uint16 count); // writes block of data
extern void ppu_fill(uint8 value, uint16 count); // writes repeating byte
extern void ppu_string(const char* s); // writes a string (set ascii_offset before using)

// PPU updates while rendering is on, during vblank
extern void ppu_packet(uint16 addr, const uint8* data, uint8 count);
extern void ppu_packet_apply(); // immediately apply packets while rendering is off
// Internal packet format, stored in ppu_send:
//   uint8: count (number of bytes to update), 0 = end of packets
//   uint16: address (PPU address for data)
//   uint8 * count: bytes of data

// PPU controls
extern void ppu_ctrl(uint8 v); // new value for $2000 at next render
extern void ppu_control_apply(uint8 v); // set immediately
extern void ppu_mask(uint8 v); // $2001 at next render
extern void ppu_mask_apply(uint8 v);
extern void ppu_scroll(uint16 x, uint16 y); // scroll at next render (note 240<=y<256 is "between" nametables)
extern void ppu_render_frame(); // send updates to renderer and wait for next frame
extern void ppu_wait_frame(); // wait 1 frame without applying updates
extern void ppu_render_off(); // turn renderer off
extern void ppu_nmi_off(); // disable NMI
extern void ppu_nmi_on();

// Sprites
extern void sprite_begin(); // begin a new sprite list
extern void sprite_end(); // conclude the sprite list
extern void sprite_tile(uint8 t, uint8 a, uint8 x, uint8 y); // add a single tile
extern void sprite(uint8 e, uint8 x, uint8 y); // add sprite "e" at x/y
extern void sprite_flip(uint8 e, uint8 x, uint8 y); // sprite with horizontal flip

extern void sound_play(const uint8* sound); // play the sound data
extern uint8 input_poll(); // poll the first player gamepad (result also in input/input_new)
extern uint8 input_poll2(); // poll both gamepads
void nescopy(void* dst, const void* src, uint8 count); // copies bytes (memcpy replacement), but count is <256

#define PAD_A       0x80
#define PAD_B       0x40
#define PAD_SELECT  0x20
#define PAD_START   0x10
#define PAD_U       0x08
#define PAD_D       0x04
#define PAD_L       0x02
#define PAD_R       0x01

//
// NES library test (press SELECT+START to activate)
//

const uint8 hextest[] = { // 0123456789ABCDEF
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
	ascii_offset = 0x20; // $20 subtracted from ASCII to get CHR tile ($20 ' ' is located at 0)
	ppu_latch(0x2000); // empty first screen
	ppu_fill(0,1024);
	ppu_latch(0x2C00); // second screen displays all tiles / all palettes
	for (mx = 0; mx<(1024-64); ++mx) ppu_write(mx & 0xFF);
	for (i=0; i<4; ++i) ppu_fill(i*0x55,16);
	ppu_latch_at(4,4);
	ppu_string("NES Library Test");
	ppu_latch_at(32+8,32+22); // goes to "bottom right" screen
	ppu_string("OFFSCREEN");
	ppu_packet(0x2000+(6*32)+4,hextest,sizeof(hextest)); // test packet_apply with the hexadecimal string
	ppu_packet_apply();
	while(1)
	{
		input_poll2(); // poll both gamepads
		for (i=0;i<2;++i) // d-pad moves 2 sprites around the screen
		{
			if (input[i] & PAD_L) --bx[i];
			if (input[i] & PAD_R) ++bx[i];
			if (input[i] & PAD_U) --by[i];
			if (input[i] & PAD_D) ++by[i];
		}
		sprite_begin();
		sprite(     SPRITE_boing,bx[0],by[0]);
		sprite_flip(SPRITE_boing,bx[1],by[1]); // 2P sprite is flipped
		sprite_end();
		hexpackets[0] = hextest[bx[0]     >> 4];
		hexpackets[2] = hextest[by[0]     >> 4];
		hexpackets[4] = hextest[nmi_count >> 4];
		hexpackets[1] = hextest[bx[0]     & 0xF];
		hexpackets[3] = hextest[by[0]     & 0xF];
		hexpackets[5] = hextest[nmi_count & 0xF];
		ppu_packet(0x2000+( 8*32)+4,hexpackets+0,2); // P1 X
		ppu_packet(0x2000+( 9*32)+4,hexpackets+2,2); // P1 Y
		ppu_packet(0x2000+(10*32)+4,hexpackets+4,2); // nmi_count (observes NMI activity)
		if (input[0] & PAD_B) ++tsx; // B/A scrolls screen
		if (input[0] & PAD_A) ++tsy;
		if (input[0] & PAD_SELECT) // SELECT shows immediate greyscale at scanline end of frame activity
		{
			ppu_mask_apply(0x1F); // greyscale immediately
			ppu_mask(0x1E); // normal render at next NMI
		}
		if (input[1] & PAD_B) ppu_wait_frame(); // P2 B = slow to half speed (note nmi_count maintains pace)
		if (input_new[1] & PAD_A) // P2 A = suspend NMI (note nmi_count does stop/resume)
		{
			ppu_nmi_off();
			input_poll2();
			while (!((input_new[0] | input_new[1]) & PAD_A)) // resume when A pressed again
				input_poll2();
			ppu_nmi_on();
		}
		ppu_scroll(tsx,tsy); // test scroll
		ppu_render_frame();
		if ((input_new[0] | input_new[1]) & PAD_START) break; // START exits test
	}
	ppu_render_off();
}

//
// Boing Ball
//

// data
extern uint8 amiga_nmt[1024];
extern uint8 atari_nmt[1024];

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

// variables
uint8 boing_bg = 0;
uint8 boing_par = 0;

uint16 boing_x = 128<<8;
uint16 boing_y = 120<<8;
uint16 boing_r = 0;

uint16 boing_vx = 0; // TODO
uint16 boing_vy = 0; // TODO
uint16 boing_vr = 256/2; // spin rate

uint8 boing_sprite = SPRITE_squ0000;

// redraw backgrounds while rendering is off
void boing_redraw()
{
	ppu_latch(0x2000);
	ppu_load(boing_bg ? atari_nmt : amiga_nmt,1024);
	ppu_scroll(0,0);
}

// animated ball
void boing_animate()
{
	boing_sprite = boing_par ? SPRITE_pau0000 : SPRITE_squ0000;
	boing_sprite += (boing_r >> 8) * 2; // rotation
	if (nmi_count & 1) boing_sprite += 1; // shadow

	boing_r += boing_vr;
	while (boing_r >= (128<<8)) boing_r += (12<<8); // wrap negative 0-11
	while (boing_r >= ( 12<<8)) boing_r -= (12<<8); // wrap positive 0-11
	
	// TODO vx/vy and bounce sounds

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
		// TODO left/right up/down
		boing_animate();
		ppu_render_frame();
	}
}
