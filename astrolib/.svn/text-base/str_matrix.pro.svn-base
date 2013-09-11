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
;   str_matrix
;
; PURPOSE:
;  The result of this function is a string vector of added string arrays
;
; CATEGORY:
;   PROG_TOOLS/STRINGS
;
; CALLING SEQUENCE:
;   Result=str_matrix(string1,string2,[/uniq])
;
; INPUTS:
;   string1:  first string vector
;   string2:  second string vector
;
; KEYWORD PARAMETERS:
;   uniq:  result is uniq
;
; OUTPUTS:
;  This function returns a vector where each element of string2 is added to each element of string1
;
; EXAMPLE:
;   string1=['A:','B:','C:']
;   string2=['1','2']
;   result=str_matrix(string1,string2)
;   print,result
;   A:1 B:1 C:1 A:2 B:2 C:2
;
;
; MODIFICATION HISTORY:
; 	Written by:	R.Bauer (ICG-1),2000-Mar-14
;-

FUNCTION str_matrix,string1,string2,UNIQ=uniq
   
   n=N_ELEMENTS(string2)
   FOR i=0,n-1 DO build_vector,result,string1+string2[i]
   
   IF KEYWORD_SET(UNIQ) THEN result=result[uniq(result,SORT(result))]
   RETURN,result
END
