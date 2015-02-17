;
; Hunt and kill maze algorithm
; https://github.com/jamis/csmazes
;

.IFDEF ALG_HUNT_AND_KILL

.LOCAL maze_alg

rng		= $d20a		; Random number generator

row_lut_lo = $4000	; screen row lookup table, lo byte
row_lut_hi = $4100	; screen row lookup table, hi byte

		ICL "definitions.asm"
		OPT R+						; enable macro code optimization
;
; Lookup tables constructed as in http://www.atariarchives.org/agagd/chapter8.php
;
.MACRO init_luts
		ldy #0						; lut offset
		lda sm_ptr					; load screen memory lo byte
		ldx sm_ptr+1
		
store_to_lut
		sta maze_alg.row_lut_lo,y	; Store y coordinate lookup table lo byte
		pha							; Push offset to stack
		txa							; X reg contains the hi byte 
		sta maze_alg.row_lut_hi,y
		pla
		
		iny
		cpy #num_rows				; Have we done all 24 rows already
		beq lut_done
		clc
		adc #bytes_per_row			; Add row width with carry
		bcc store_to_lut				
		inx							; Carry set, increase hi byte
		jmp store_to_lut
		
lut_done
.ENDM

;
; Initialize maze algorithm
; - lookup tables
; - flags
; - random starting cell
;
.PROC init
	init_luts

	lda #0								; Reset maze ready flag
	sta maze_ready
		
	mod rng, #num_rows					; Random starting point, y coord 
	sta coord_y
	mod rng, #num_cols
	sta coord_x

	rts	
.ENDP

;
; Takes one step of maze generation.
; Either continues from current cell, or scans
; for new cell to continue.
;
wall_mask	.BY %00000001

.PROC step
	.ZPVAR wild_hunt .byte	; did we just hunt

	mva #0 wild_hunt
	check_neighbours
	txa
	bne carve_new_cell
	
	; Start hunt from upper-left corner
	mva #1 wild_hunt
	lda #0
	sta coord_x
	sta coord_y
	setup_position_from_y_coord position, coord_y
	ldy #0
	
hunt
	lda (position),y
	beq empty_cell			; cell is empty, check neighbours

next_cell
	iny
	cpy #num_cols
	bne hunt
	
	lda coord_y
	cmp #[num_rows - 1]
	beq maze_done
	inc coord_y
	setup_position_from_y_coord position, coord_y
	ldy #0
	jmp hunt

maze_done
	lda #1
	sta maze_ready
	rts
	
empty_cell					; we're in empty cell, are there neighbours	
	check_neighbours
	cpx #$F					; all free, not good
	beq next_cell
	
carve_new_cell
	and rng					; bitwise and the neighbours status with rng

	bit wall_mask
	bne left
	lsr
	bit wall_mask
	bne right
	lsr
	bit wall_mask
	bne up
	lsr
	bit wall_mask
	bne	 down
	
	txa
	jmp carve_new_cell		; that was bad random number ;), let's try again

left
	ldx #wall_left
	connect_cell
	lda wild_hunt
	bne done				; if we came here from hunt, don't update coords
	dec coord_x
	rts
	
right
	ldx #wall_right
	connect_cell
	lda wild_hunt
	bne done
	inc coord_x
	rts

up
	ldx #wall_up
	connect_cell
	lda wild_hunt
	bne done
	dec coord_y
	rts
	
down
	ldx #wall_down
	connect_cell
	lda wild_hunt
	bne done
	inc coord_y
		
done
	rts
.ENDP

;
; modulo
; https://gist.github.com/hausdorff/5993556
;
.MACRO mod val1,val2
	lda	:val1
	sec
	
loop
	sbc :val2
	bcs loop
	adc :val2
.ENDM

;
; Write walls nybble to current cell
;
.MACRO write_cell walls
	setup_position_from_y_coord position, coord_y
	
	ldy coord_x
	lda :walls
	sta (position),y
.ENDM

;
; Setup screen memory pointer pos_ptr according to coordinate y
;
.MACRO setup_position_from_y_coord pos_ptr, y
	ldy :y
	mva row_lut_lo,y :pos_ptr
	mva row_lut_hi,y :pos_ptr+1
.ENDM

;
; Removes walls between current cell and cell in direction in register x.
; Procedure assumes direction has already been cbecked to be valid.
;
.PROC connect_cell
	.ZPVAR tmp_y .byte

	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda (position),y
	
left
	cpx #wall_left
	bne right
	and #[$F - wall_left]
	sta (position),y
	dey
	lda (position),y
	and #[$F - wall_right]
	sta (position),y
	rts
	
right
	cpx #wall_right
	bne up
	and #[$F - wall_right]
	sta (position),y
	iny
	lda (position),y
	and #[$F - wall_left]
	sta (position),y
	rts
	
up
	cpx #wall_up
	bne down
	and #[$F - wall_up]
	sta (position),y
	mva coord_y tmp_y
	dec tmp_y
	setup_position_from_y_coord position, tmp_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_down]
	sta (position),y
	rts	

down
	and #[$F - wall_down]
	sta (position),y
	mva coord_y tmp_y
	inc tmp_y
	setup_position_from_y_coord position, tmp_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_up]
	sta (position),y
	rts
.ENDP

;
; Check neighbour cells of current cell (coord_x, coord_y).
; Status returned in x register low nybble.
; Bit 1 - free cell, bit 0 - occupied
; 
.PROC check_neighbours
	.ZPVAR check_position .WORD
	.ZPVAR check_x check_y .BYTE
	
	mva coord_x check_x		; copy current cell coords
	mva coord_y check_y

	ldx	#0					; x contains the neighbour bits, 1 - free, 0 - occupied

	lda	coord_y
	beq down				; if coord_y == 0, skip up

up							; check up for neighbours
	sta check_y
	dec check_y
	setup_position_from_y_coord check_position, check_y
	ldy check_x
	lda (check_position),y
	cmp #walled_in
	bne down				; if != F, cell is occupied already
	txa
	ora #wall_up
	tax
	lda coord_y
	
down
	cmp #[num_rows - 1]
	beq left
	sta check_y
	inc check_y
	setup_position_from_y_coord check_position, check_y
	ldy check_x
	lda (check_position),y
	cmp #walled_in
	bne left
	txa
	ora #wall_down
	tax
	
left
	mva coord_y check_y
	lda coord_x
	beq right
	sta check_x
	dec check_x
	setup_position_from_y_Coord check_position, check_y
	ldy check_x
	lda (check_position),y
	cmp #walled_in
	bne right
	txa
	ora #wall_left
	tax
	
right
	lda coord_x
	cmp #[num_cols - 1]
	beq done
	sta check_x
	inc check_x
	setup_position_from_y_coord check_position, check_y
	ldy check_x
	lda (check_position),y
	cmp #walled_in
	bne done
	txa
	ora #wall_right
	tax
	
done
	rts	
.ENDP

.ENDL

.ENDIF