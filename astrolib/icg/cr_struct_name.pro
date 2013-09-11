; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	cr_struct_name
;
; PURPOSE:
;	This function returns a unique structure name based on current
;	date and time information and an additional random number.
;
; CATEGORY:
;	PROG_TOOLS/STRUCTURES
;
; CALLING SEQUENCE:
;	structure_name = cr_struct_name()
;
; COMMON BLOCKS:
;	STORE_SEED: A Long Array[36] which stores the state of the random number generator
;
; EXAMPLE:
;	PRINT, cr_struct_name()
;		'Mon Mar 16 20:55:13 1998      425995'
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 17. Mar. 98.
;	16.03.1999	Removed debug statement, Header edited
;-

FUNCTION cr_struct_name
	COMMON STORE_SEED, _sv__randomu ; Store state of the random number generator
	snm = SYSTIME(0) + STRING(LONG(1.0D6*RANDOMU(_sv__randomu)))
	RETURN, snm
END


