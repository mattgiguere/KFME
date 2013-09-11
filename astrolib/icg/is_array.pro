; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	is_array
;
; PURPOSE:
;	Test if variable is of type array
;
; CATEGORY:
;	PROG_TOOLS
;
; CALLING SEQUENCE:
;	Result = is_array(var)
;
; INPUTS:
;	var: The variable to be tested.
;
; OUTPUTS:
;	This function returns 1 if var is of type array else it returns 0
;
; EXAMPLE:
;	A = [1, 2, 3]
;	PRINT, is_array(A)
;	-> 1
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 19.01.98
;	27.01.98: debug statement auskommentiert
;	07.09.98: Return statement vereinfacht
;-

FUNCTION is_array, var
;	debug,'V1.0 FH 1998-01-19'
	RETURN, (SIZE(var))[0] GT 0
END
