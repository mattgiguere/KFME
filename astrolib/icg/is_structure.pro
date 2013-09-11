; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	is_structure
;
; PURPOSE:
;	Test if variable is of type structure
;
; CATEGORY:
;	PROG_TOOLS
;
; CALLING SEQUENCE:
;	Result = is_structure(var)
;
; INPUTS:
;	var: The variable to be tested.
;
; OUTPUTS:
;	This function returns 1 if var is of type structure else it returns 0
;
; EXAMPLE:
;	A = {q:0, w:'d'}
;	PRINT, is_structure(A)
;	-> 1
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 19.01.98
;	27.01.98: debug statement auskommentiert
;	07.09.98: Return statement vereinfacht
;-

FUNCTION is_structure, var
;	debug,'V1.0 FH 1998-01-19'
	a = SIZE(var)
	n = N_ELEMENTS(a)
	RETURN, a[n - 2] EQ 8
END
