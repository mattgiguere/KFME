function makearr,n,inmin,inmax,fan=fan,transpose=transpose,double=double
;+
; NAME: 
;       MAKEARR 
;
;
; PURPOSE:
;       This procedure will generate an array of lenght N which
;       runs from values MIN to MAX
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;       f = makearr(n, min, max [,fan=, transfan=, /double])
;
;
; INPUTS:
;
;       N:    The number of desired array elements in F
;       MIN:  The value of the first array element in F
;       MAX:  The value of the last array element in F
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
;       FAN:        Number of times the array is to be repeated.
;                   The final dimensions of F  will be N columns 
;                   by FAN rows.
;       /TRANSPOSE  Final dimensions of F wil be FAN columns by N 
;                   rows if FAN is specified. 
;
; OUTPUTS:
;
;       F:    Final array
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;      If you want a 5 element array which runs from 2 to 4:
;
;         IDL> f = makearr(10,2,4)
;         IDL> print, f
;             2.00000      2.50000      3.00000      3.50000      4.00000
;         
; MODIFICATION HISTORY:
;
;-

if n_params() lt 3 then begin 
  if n_params() eq 2 then if n_elements(inmin) eq 2 then begin
    max = inmin[1]
    min = inmin[0]
  endif else begin
    message, 'Syntax: f = makearr(nelements,min,max)', /info
    retall
  endelse
endif else begin
  max = inmax
  min = inmin
endelse

if KEYWORD_SET(double) then begin
    min = double(min)
    max = double(max)
endif else begin
    min = float(min)
    max = float(max)
endelse

if KEYWORD_SET(double) then begin
    a = dindgen(n)*(max-min)/(n-1)+min
endif else begin
    a = findgen(n)*(max-min)/(n-1)+min
endelse

if KEYWORD_SET(fan) then begin
    p = fltarr(fan)+1.
    if KEYWORD_SET(transpose) then begin
       a = a##p
    endif else begin
       a = p##a
    endelse
endif


return,a
end
