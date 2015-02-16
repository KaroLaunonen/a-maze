; Maze
;
; Maze cell data; 4 bits per cell. 0 - no wall, 1 - wall
; 1 left
; 2 right
; 4 up
; 8 down

sm_ptr	= $58		; Screen memory pointer
ch		= $2f4		; Pointer to high byte of character set
chr		= $2400		; International character set

; Zero page variables
position 		= $cb	; Screen Position, word, address to screen memory
coord_x			= $cd	; x coordinate of current cell, 0-39
coord_y			= $ce	; y coordinate, 0-23
maze_ready		= $cf	; is maze ready; 0 - false, otherwise true

	ORG $2000

	.DEF ALG_HUNT_AND_KILL		; maze_alg points to Hunt-and-Kill

	ICL "definitions"
	ICL "hunt_and_kill"

	.PROC init
		mva #>chr ch					; set character set

		; Clear screen memory, 40*24 bytes = 3 pages and change
		mwa sm_ptr position
		mwa sm_ptr position+2		; use zero page locations without worries at this point
		inc position+3				; as they are uninitialized anyway. I setup pointers
		mwa position+2 position+4	; to 3 consecutive pages for simpler clearing loop 
		inc position+5
		
		lda #0
     	ldy #0							; Offset
clr_loop	
		sta (position),y
		sta (position+2),y
		sta (position+4),y
     	iny
     	bne clr_loop
     	
     	; Still 192 bytes to go on the fourth page
		ldy	#192
		inc position+5				; increase hi byte for next page
clr_loop2	
		dey
		sta	(position+4),y
		bne clr_loop2

		maze_alg.init
		jmp main
	.ENDP
	
	.PROC main
main_loop
	maze_alg.step
	lda #0
	cmp maze_ready
	beq main_loop

	jmp *							; stop	
	.ENDP
	
	; Custom character set
	ORG chr
	INS "maze.chr"

	RUN init