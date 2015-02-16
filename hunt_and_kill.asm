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

	write_cell #walled_in
	
	rts	
.ENDP

;
; Takes one step of maze generation.
; Either continues from current cell, or scans
; for new cell to continue.
;
wall_mask	.BY %00000001

.PROC step	
	check_neighbours
	txa
	bne carve_new_cell
	
	; Start hunt from upper-left corner
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
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_left]
	sta (position),y
	dec coord_x
	dey
	lda #[$F - wall_right]
	sta (position),y
	jmp done
	
right
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_right]
	sta (position),y
	inc coord_x
	iny
	lda #[$F - wall_left]
	sta (position),y
	jmp done

up
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_up]
	sta (position),y
	dec coord_y
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda #[$F - wall_down]
	sta (position),y
	jmp done
	
down
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda (position),y
	and #[$F - wall_down]
	sta (position),y
	inc coord_y
	setup_position_from_y_coord position, coord_y
	ldy coord_x
	lda #[$F - wall_up]
	sta (position),y
	
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
	bne down				; if != 0, cell is occupied already
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
	bne done
	txa
	ora #wall_right
	tax
	
done
	rts	
.ENDP

.ENDL

.ENDIF