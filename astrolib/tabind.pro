function tabind, xtab, xnew
;Associate each element in a new vector with an interval in an old vector.
;Inputs:
; xtab (vector(nt)) dependent variable for an unspecified table of values.
;   Sequence of values in "xtab" must be increasing.
; xnew (vector(nn)) new points to be associated with intervals in "xtab".
;Outputs:
; itab (vector(nn)) indicies of bracketting intervals for each point in xnew.
;   itab(j) =  i, where xtab(i) le xnew(j) lt xtab(i+1), for i lt nt-2
;   itab(j) =  i, where xtab(i) le xnew(j) le xtab(i+1), for i eq nt-2
;   itab(j) = -1, where            xnew(j) lt xtab(0)
;   itab(j) = -2, where            xnew(j) gt xtab(nt-1)
;History:
; 24-Feb-95 Valenti  Initial coding.
; 11-Mar-95 Valenti  use for loops, instead of arrays - less memory, faster.

if n_params() lt 2 then begin
  print,'syntax: itab = tabind(xtab, xnew)'
  retall
endif

;Sizes of table and new vectors.
  nt = n_elements(xtab)					;size of table
  nn = n_elements(xnew)					;number of new points
  itab = replicate(-3, nn)				;init w/ bogus value

;Sort elements of old and new vectors together.
  isort = sort([xtab, xnew])				;sort together

;Loop thru sort indicies, making desired associations.
  it = -1						;before table flag
  for i = 0, nt + nn - 1 do begin			;loop thru all x
    if isort(i) lt nt then begin			;true: table value
      it = isort(i)					;current table index
    endif else begin					;else: new x value
      itab(i - it - 1) = it				;associate x with table
    endelse
  endfor

;Mark points after final table value.
  iwhr = where(xnew gt xtab(nt-1), nwhr)		;find after final
  if nwhr gt 0 then itab(iwhr) = -2			;true: mark off end

;Fix last endpoint.
  iwhr = where(xnew eq xtab(nt-1), nwhr)		;find final points
  if nwhr gt 0 then itab(iwhr) = nt - 2			;true: mark final

;Return interval indicies/flags.
  return, itab

end
