; Definitions

.IFNDEF DEFINITIONS
.DEF DEFINITIONS

wall_left  		= %0001
wall_right 		= %0010
wall_up			= %0100
wall_down		= %1000
walled_in		= wall_left + wall_right + wall_up + wall_down
no_walls		= 0
bytes_per_row	= 40
bytes_per_col	= 1
num_rows		= 24
num_cols		= 40

.ENDIF