; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; USERLEVEL:
;   TOPLEVEL
;
; NAME:
;   arr2string
;
; PURPOSE:
;   This function will combine elements of a stringarray; e.g. [' A ',' B ']->'A,B'
;
; CATEGORY:
;   PROG_TOOLS/STRINGS
;
; CALLING SEQUENCE:
;   result=arr2string(array)
;
; INPUT PARAMETERS:
;   strarray
;
; KEYWORD PARAMETERS:
;   sep  : separator, default:' '
;   limit: string length limit (default 32767); longer strings will be put into a stringvector
;
; OUTPUTS:
;   result: str-vector; combination of elements of the array separated by a separator
;           undefined array will return result=''
;
; EXAMPLE:
;   result=arr2string(['A','BCD','DCB'],sep=':')
;   result:'A:BCD:DCB'
;
;   result=arr2string()
;   result:''
;
; MODIFICATION HISTORY:
;   Written by Reimar Bauer, Franz Rohrer Aug 1998
;   Modified 10.03.2000 Franz Rohrer : limit string length to 32765
;-
function arr2string,str,sep=sep,limit=lim
maximum=2.^15-1
if n_elements(sep) eq 0 then separator=' '  else separator=sep
if n_elements(lim) eq 0 then limit =maximum else limit    =lim
if limit eq 0 or limit gt maximum then limit=maximum

result=['']
n=n_elements(str)
  indx=0L
  s=''
  for i=0L,n-1 do begin
    newstr    =strtrim(str[i],2)

    while strlen(newstr) gt 0 do begin
    e=limit-strlen(result[indx])-strlen(s)
    case 1 of
    strlen(result[indx])+strlen(s) ge limit:begin
      s=''
      indx  =indx+1
      result=[result,'']
      end
    e-strlen(newstr) lt 0:begin
      result[indx]=result[indx]+s+strmid(newstr,0,e)
      newstr=strmid(newstr,e)
      s=''
      indx  =indx+1
      result=[result,'']
      end
    else:begin
      result[indx]=result[indx]+s+newstr
      newstr=''
      end
    endcase
    endwhile
    s     =separator
  endfor

if n_elements(result) eq 1 then result=result[0]
return,result
end


