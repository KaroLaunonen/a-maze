; Player control routines

.LOCAL player_control

joystick_0	= $278

joystick_up		= %0001
joystick_down	= %0010
joystick_left	= %0100
joystick_right	= %1000

min_y			= 30
max_y			= 160

.PROC move_player
up
	lda #joystick_up
	bit joystick_0
	bne down
	move_up
	jmp left

down
	lda #joystick_down
	bit joystick_0
	bne left
	move_down
	
left
	lda #joystick_left
	bit joystick_0
	bne right
	dec player_x
	lda player_x
	sta p0_hpos
	jmp done

right
	lda #joystick_right
	bit joystick_0
	bne done
	inc player_x
	lda player_x
	sta p0_hpos

done
	rts
.ENDP

.PROC move_up
	lda player_y
	cmp min_y
	beq done

	dec player_y
	sta p0_addr
	ldx #8
	
move_loop
	ldy #1				; stupid logic, think again
	lda (p0_addr),y
	dey
	sta (p0_addr),y
	inc p0_addr
	dex
	bne move_loop

done
	rts
.ENDP

.PROC move_down
	lda player_y
	cmp max_y
	beq done
	
	lda player_y
	clc
	adc #8
	sta p0_addr
	
move_loop
	ldy #0
	dec p0_addr
	lda (p0_addr),y
	iny
	sta (p0_addr),y
	lda player_y
	cmp p0_addr
	bne move_loop

	inc player_y
	
done	
	rts
.ENDP

.ENDL