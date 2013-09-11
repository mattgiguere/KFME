; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; NAME:
;	is_defined
;
; PURPOSE:
;	Test if variable is defined
;
; CATEGORY:
;	PROG_TOOLS
;
; CALLING SEQUENCE:
;	Result = is_defined(var)
;
; INPUTS:
;	var: The variable to be tested.
;
; OUTPUTS:
;	This function returns 1 if var is defined else it returns 0
;
; EXAMPLE:
;	A = 3
;	PRINT, is_defined(A)
;	-> 1
;
; MODIFICATION HISTORY:
; 	Written by:	Frank Holland, 20.01.98
;	27.01.98: debug statement auskommentiert
;	07.09.98: Return statement vereinfacht
;-

FUNCTION is_defined, var
;	debug,'V1.0 FH 1998-01-20'
	a = SIZE(var)
	n = N_ELEMENTS(a)
	RETURN, a[n - 2] NE 0
END
