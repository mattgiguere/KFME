;
; Copyright (c) 2000, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; NAME:
;   strmatch2
;
; PURPOSE:
;   The strmatch2 function compares its search string, which can contain wildcard characters,
;   against the input string expression. The result of this function is an index vector of indices that match
;   to the input string
;
; CATEGORY:
;   PROG_TOOLS/STRINGS
;
; CALLING SEQUENCE:
;   Result=strmatch2(text,searchstring,[count=count],[_extra=e],[/complement])
;
; INPUTS:
;   text:     The String to be matched.
;   searchstring:  The search string, which can contain wildcard characters as discussed above.
;
; KEYWORD PARAMETERS:
;   _extra: to submit keywords to STRMATCH()
;   complement: if arg_present then  complement result is returned to
; OUTPUTS:
;  The result of this function is an index vector of indices that match to the input string
;
; EXAMPLE:
;   text=['Anton:','Anton2:','Anton3:']
;   result=strmatch2(text,'Anton')
;   print,result
;   -1
;
;   result=strmatch2(text,'Anton:')
;   print,result
;   0
;
;   result=strmatch2(text,'Anton?:')
;   print,result
;   1           2
;
;   result=strmatch2(text,'A*')
;   print,result
;   0           1           2
;
; MODIFICATION HISTORY:
; 	Written by:	R.Bauer (ICG-1),2000-Mar-14
;  2002-04-14:  argument complement added
;-

FUNCTION strmatch2,text,searchstring,complement=complement,count=count,_extra=e
   count=0
   n=N_ELEMENTS(searchstring)
 
   FOR i=0,n-1 DO BEGIN
      idx=WHERE(strmatch(text,searchstring[i],_extra=e) EQ 1,count_idx)
      IF count_idx GT 0 THEN build_vector,result,idx
   ENDFOR
   if arg_present(complement) then complement=a_not_b(lindgen(n_elements(text)),result)



   count=N_ELEMENTS(result)
   IF count EQ 0 THEN return,-1

   result=result[uniq(result,sort(result))]
   count=N_ELEMENTS(result)

   RETURN,result
END
