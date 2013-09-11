; $Id: build_vector.pro,v 1. 1997/06/13 13:31:13 RwB ICG-1 $
;
; Copyright (c) 1997, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;+
; NAME:
;	build_vector
;
; PURPOSE:
;	This routine accumulates values into a vector
;
; CATEGORY:
;   PROG_TOOLS
;
; CALLING SEQUENCE:
;   build_vector,vector,value
;
; INPUTS:
;   vector : if it's present the vector which will be accumulated to
;   value  : the value which will be added to the vector
;
; OUTPUTS:
;	vector: A vector holding value
;
; EXAMPLE:
;   build_vector,test,5
;   print,test
;   5
;
; MODIFICATION HISTORY:
; 	Written by:	R.Bauer (ICG-1), 1998-Jun-27
;-

PRO build_vector,vector,value

   IF n_params() lt 2 THEN BEGIN
      MESSAGE,call_help(),/cont
      RETURN
   ENDIF


   IF N_ELEMENTS(vector) EQ 0 THEN vector=value ELSE vector=[vector,value]
END
