;
; Copyright (c) 1999, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; NAME:
;	call_help
;
; PURPOSE:
;	The result of this function is the text below CALLING SEQUENCE from a called routine
;
; CATEGORY:
;   CASE_TOOLS
;
; CALLING SEQUENCE:
;   result=call_help()
;
; EXAMPLE:
;   message,call_help(),/info
;
; MODIFICATION HISTORY:
; 	Written by	R.Bauer (ICG-1), 1999-Nov-06
;   1999-Nov-06 help added
;
;-

FUNCTION call_help
   HELP,call=call

   IF N_ELEMENTS(call) GT 2 THEN $
   file=(STR_SEP((STR_SEP(call[1],'<'))[1],'('))[0] $
   ELSE $
   file=(STR_SEP((STR_SEP(call[0],'<'))[1],'('))[0]

   RETURN,get_template_one_value(file,'CALLING SEQUENCE:')
END

