;
; boing.s
; Boing Ball NES demo by Brad Smith 2021
; http://rainwarrior.ca
;

.exportzp _input            ; uint8 input[2]
.exportzp _cnrom_bank       ; uint8 cnrom_bank
.exportzp _i,_j,_k,_l       ; uint8 i,j,k,l
.exportzp _mx,_nx,_ox,_px   ; uint16 mx,nx,ox,px

.export _palette            ; uint8 palette[32]
.export _ppu_send           ; uint8 ppu_send[72]
.export _oam                ; uint8 oam[256]

.export _ppu_latch          ; void(uint16 addr);
.export _ppu_write          ; void ppu_write(uint8 value);
.export _ppu_load           ; void ppu_load(uint8* data, uint16 count)
.export _ppu_fill           ; void ppu_fill(uint8 value, uint16 count)
.export _ppu_packet         ; void ppu_packet(uint16 addr, uint8* data, uint8 count)
.export _ppu_packet_apply   ; void ppu_packet_apply()
.export _ppu_ctrl           ; void ppu_ctrl(uint8 v)
.export _ppu_ctrl_apply     ; void ppu_control_apply(uint8 v)
.export _ppu_mask           ; void ppu_mask(uint8 v)
.export _ppu_scroll         ; void ppu_scroll(uint16 x, uint16 y)
.export _ppu_render_frame   ; void ppu_render_frame()
.export _ppu_wait_frame     ; void ppu_wait_frame()
.export _ppu_render_off     ; void ppu_render_off()
.export _ppu_nmi_off        ; void ppu_nmi_off()
.export _ppu_nmi_on         ; void ppu_nmi_on()

.export _sprite_begin       ; void sprite_begin()
.export _sprite_end         ; void sprite_end()
.export _sprite             ; void sprite(uint8 e, uint8 x, uint8 y)

.export _sound_play         ; void sound_play(uint8* sound)
.export _input_poll         ; uint8 input_poll
.export _input_poll2        ; uint8 input_poll2()

.import _main

; ==========
; Boing Ball
; ==========

.segment "TILES0A"
.incbin "temp/bg.chr"
.segment "TILES0B"
.incbin "temp/fg0.chr"

.segment "TILES1A"
.incbin "temp/bg.chr"
.segment "TILES1B"
.incbin "temp/fg1.chr"

.segment "RODATA"
sprite_data:
.incbin "temp/sprite.bin"
.include "temp/sprite.inc"

; ===
; RAM
; ===

CSTACK_SIZE = 128 ; cc65 internal C stack

.segment "ZEROPAGE"
ppu_ready:    .res 1 ; 0 = not ready, 1 = render on and send update, 2 = render off
nmi_count:    .res 1 ; increments every NMI
nmi_lock:     .res 1 ; prevents NMI re-entry (disables updates)
ppu_2000:     .res 1 ; PPU register settings (applied with ppu_send)
ppu_2001:     .res 1
ppu_2005:     .res 2
ppu_send_pos: .res 1
sfx_ptr:      .res 2 ; sound state
sfx_ptr_next: .res 2
_input:       .res 2 ; last result of input_poll
_cnrom_bank:  .res 1
_i:           .res 1 ; convenient index/temporary values
_j:           .res 1
_k:           .res 1
_l:           .res 1
_mx:          .res 2
_nx:          .res 2
_ox:          .res 2
_px:          .res 2

.segment "BSS"
cstack: .res CSTACK_SIZE ; cc65 internal C stack
_palette: .res 32

.segment "STACK"
_ppu_send: .res 72 ; bytes to send on next NMI when ppu_ready

.segment "OAM"
.align 256
_oam: .res 256

.segment "CODE"

; =========
; Rendering
; =========

_ppu_latch: ; void(uint16 addr);
_ppu_write: ; void ppu_write(uint8 value);
_ppu_load: ; void ppu_load(uint8* data, uint16 count)
_ppu_fill: ; void ppu_fill(uint8 value, uint16 count)
_ppu_packet: ; void ppu_packet(uint16 addr, uint8* data, uint8 count)
_ppu_packet_apply: ; void ppu_packet_apply()
_ppu_ctrl: ; void ppu_ctrl(uint8 v)
_ppu_ctrl_apply: ; void ppu_control_apply(uint8 v)
_ppu_mask: ; void ppu_mask(uint8 v)
_ppu_scroll: ; void ppu_scroll(uint16 x, uint16 y)
_ppu_render_frame: ; void ppu_render_frame()
_ppu_wait_frame: ; void ppu_wait_frame()
_ppu_render_off: ; void ppu_render_off()
_ppu_nmi_off: ; void ppu_nmi_off()
_ppu_nmi_on: ; void ppu_nmi_on()
	; TODO
	rts

; =======
; Sprites
; =======

_sprite_begin:
_sprite_end:
_sprite:
	; TODO
	rts

; =====
; Sound
; =====

SFX_FRAME = $FD
SFX_OFF   = $FE
SFX_END   = $FF

sfx_init: ; full reset of APU
.byte $15,%00000000 ; $4015 silence/reset all channels
.byte $00,%00110000 ; $4000 SQ0 volume/duty 0, disable length counter
.byte $04,%00110000 ; $4004 SQ1 volume/duty 0, disable length counter
.byte $08,%10000000 ; $4008 TRI halt
.byte $0C,%00110000 ; $400C NSE volume 0, disable length counter
.byte $10,%00000000 ; $4010 DMC control
.byte $11,%00000000 ; $4011 DMC output counter
.byte $12,%00000000 ; $4012 DMC address
.byte $13,%00000000 ; $4013 DMC length
.byte $15,%00001111 ; $4015 turn on 4 channels (not DMC)
sfx_off:
.byte $00,%00110000 ; $4000 volume/duty 0
.byte $04,%00110000 ; $4004
.byte $08,%10000000 ; $4008
.byte $0C,%00110000 ; $400C
.byte $01,%01111111 ; $4001 SQ0 sweep disable
.byte $05,%01111111 ; $4005 SQ1 sweep disable
.byte $02,%00000000 ; $4002 SQ0 frequency low
.byte $06,%00000000 ; $4006 SQ1 frequency low
.byte $0A,%00000000 ; $400A TRI frequency low
.byte $0E,%00000000 ; $400E NSE frequency, period
.byte $03,%00001000 ; $4003 SQ0 frequency high, length counter
.byte $07,%00001000 ; $4007 SQ1 frequency high, length counter
.byte $0B,%00001000 ; $400B TRI frequency high, length counter
.byte $0F,%00001000 ; $400F NSE length counter
sfx_end:
.byte SFX_END

sound_init:
	lda #<sfx_end
	sta sfx_ptr+0
	lda #>sfx_end
	sta sfx_ptr+1
	ldx #0
sfx_restart: ; quick restart: X = sfx_off - sfx_init
:
	ldy sfx_init, X
	cpy #SFX_END
	beq :+
	inx
	lda sfx_init, X
	sta $4000, Y
	inx
	bne :-
:
	rts

sound_tick:
	; start new sound effect if requested
	lda sfx_ptr_next+0
	ora sfx_ptr_next+1
	beq :+
		lda sfx_ptr_next+0
		sta sfx_ptr+0
		lda sfx_ptr_next+1
		sta sfx_ptr+1
		ldx #(sfx_off - sfx_init)
		jsr sfx_restart
		lda #0
		sta sfx_ptr_next+0
		sta sfx_ptr_next+1
	:
	; continue sound from sfx_ptr
	ldy #0
sfx_next:
	lda (sfx_ptr), Y
	iny
	cmp #SFX_END ; halted
	bne :+
		rts
	:
	cmp #SFX_FRAME ; advance to next frame
	bcc sfx_register
	bne :+
		tya
		clc
		adc sfx_ptr+0
		sta sfx_ptr+0
		lda sfx_ptr+1
		adc #0
		sta sfx_ptr+1
		rts
	:
	cmp #SFX_OFF ; silence all channels and halt sound
	bne sfx_register
		lda #<sfx_off
		sta sfx_ptr+0
		lda #>sfx_off
		sta sfx_ptr+1
		jmp sound_tick
	sfx_register: ; play next register
		tax
		lda (sfx_ptr), Y
		sta $4000, X
		iny
		jmp sfx_next

_sound_play: ; X:A = byte* s (sound data to play)
	sta sfx_ptr_next+0
	stx sfx_ptr_next+1
	rts

; =====
; Input
; =====

input_poll0: ; polls only controller 0
	ldx #0
input_poll: ; polls controller X (0,1)
	lda _input, X
	pha ; remember last poll value for comparison
@poll:
	ldy #1
	sty $4016 ; latch strobe
	dey
	sty $4016
	lda #$01 ; 1 bit guard marks end of 8 bit read
	sta _input, X
	:
		lda $4016, X
		and #%00000011
		cmp #%01
		rol _input, X
		bcc :- ; guard reached when carry set
	pla
	cmp _input, X
	bne :+
		rts ; last 2 polls match: done (result in A but flags = 0)
	:
	; mismatch = probable DPCM conflict, try again
	lda _input, X
	pha
	jmp @poll

_input_poll2: ; uint8 input_poll2()
	ldx #1
	jsr input_poll
_input_poll:  ; uint8 input_poll()
	jsr input_poll0
	;ldx #0
	lda _input+0
	rts

; ===
; NMI
; ===

nmi:
	inc nmi_count
	pha
	lda nmi_lock
	beq :+
		pla
		rti
	:
	lda #$FF
	sta nmi_lock ; prevent re-entry
	txa
	pha
	tya
	pha
	lda ppu_ready
	beq @jmp_post_send ; 0 = no update
	cmp #2 
	bcc @send ; 2 = render off
	lda ppu_2001
	and #%11100001
	sta $2001
@jmp_post_send:
	jmp @post_send
@send: ; 1 = send ppu data
	; OAM
	ldx #0
	stx $2003
	lda #>_oam
	sta $4014
	; palettes
	;ldx #0
	stx $2000 ; set horizontal increment
	bit $2002
	lda #>$3F00
	sta $2006
	;ldx #<$3F00
	stx $2006
	;ldx #0
	:
		lda _palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; set $2000 to requested state
	lda ppu_2000
	sta $2000
	; upload _ppu_send
	; move _ppu_send address to stack
	.assert (>_ppu_send = $01), error, "_ppu_send expected on stack page."
	tsx
	txa
	tay ; save stack position
	ldx #<_ppu_send
	txs
@send_packet:
	; first byte of packet = count
	pla
	beq @send_finish ; 0 = done
	tax
	pla
	sta $2006
	pla
	sta $2006
	:
		pla
		sta $2007
		dex
		bne :-
	beq @send_packet
@send_finish:
	; restore stack
	tya
	tax
	txs
	lda #0
	sta _ppu_send ; clear packet
	sta ppu_send_pos
	lda ppu_2005+0
	sta $2005
	lda ppu_2005+1
	sta $2005
	lda ppu_2001
	sta $2001
	; set CNROM CHR bank
	lda _cnrom_bank
	and #1 ; 2 banks
	tax
	sta cnrom_table
@post_send:
	jsr sound_tick
	pla
	tay
	pla
	tax
	pla
	inc nmi_lock ; =0
	rti

cnrom_table: .byte 0, 1 ; bus conflict table

; ===
; IRQ
; ===

irq:
	rti

; =====
; Reset
; =====

reset:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup stack
	; wait for vblank #1
	bit $2002
	:
		bit $2002
		bpl :-
	lda #0 ; clear RAM
	tax
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	lda #$FF ; clear OAM
	:
		sta _oam, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; initialize internal variables
	jsr sound_init
	lda #%00011110 ; No emphasis, no greyscale, BG and sprite shown, no hidden column
	sta ppu_2001
	lda #%10001000 ; NMI on, 8-pixel sprites, BG page 0, Sprite page 1
	sta ppu_2000
	sta $2000 ; turn NMI on permanently
	; initialize CC65 and enter main()
	jsr cc65_init
	jsr _main
	jmp ($FFFC)

; ==========
; CC65 setup
; ==========

; simplified version of cc65 libsrc/nes/crt0.s

.import copydata ; cc65 "DATA" segment setup
.importzp sp ; cc65 C stack pointer
.export __STARTUP__: absolute = 1

.segment "CODE"
cc65_init:
	jsr copydata
	lda #<(cstack + CSTACK_SIZE)
	sta sp+0
	lda #>(cstack + CSTACK_SIZE)
	sta sp+1
	rts

; =======
; NES ROM
; =======

.segment "HEADER"

INES_MAPPER     = 3 ; CNROM
INES_MIRROR     = 1 ; horizontal nametables
INES_PRG_16K    = 2 ; 32K
INES_CHR_8K     = 2 ; 16K
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 0
INES2_CHRBAT    = 0
INES2_REGION    = 2 ; 0=NTSC, 1=PAL, 2=Dual

; iNES 1 header
.byte 'N', 'E', 'S', $1A ; ID
.byte <INES_PRG_16K
.byte INES_CHR_8K
.byte INES_MIRROR | (INES_BATTERY << 1) | ((INES_MAPPER & $f) << 4)
.byte (<INES_MAPPER & %11110000) | INES2
; iNES 2 section
.byte (INES2_SUBMAPPER << 4) | (INES_MAPPER>>8)
.byte ((INES_CHR_8K >> 8) << 4) | (INES_PRG_16K >> 8)
.byte (INES2_PRGBAT << 4) | INES2_PRGRAM
.byte (INES2_CHRBAT << 4) | INES2_CHRRAM
.byte INES2_REGION
.byte $00 ; VS system
.byte $00, $00 ; padding/reserved
.assert * = 16, error, "NES header must be 16 bytes."

.segment "VECTORS"
.word nmi
.word reset
.word irq

; end of file
