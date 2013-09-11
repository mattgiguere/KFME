function sign, in

  n = n_elements(in)
  if 1-is_number(in) then return, (n lt 2) ? 0 : replicate(0, n)  ;are these numbers?  Return 0 if not.
  d = datatype(in)
  if d eq 'COM' or d eq 'DCO' then begin ;return complex numbers with real and imaginary parts -1,0,or 1
    r = real_part(in)
    i = imaginary(in)
    return, complex(sign(r), sign(i))
  endif

  out = in*0               ;return 0 unless...
  n = where(in lt 0, nn)   ;things are lt 0
  p = where(in gt 0, np)   ;or gt 0
  nan = where(is_nan(in), nnan) ;if these are checked, we'll return nan

  if nn gt 0 then out[n] = -1   ;negatives get -1
  if np gt 0 then out[p] = 1    ;positives get 1
  if nnan gt 0 then begin        
    if d eq 'FLO' then out[nan] = !values.f_nan
    if d eq 'DOU' then out[nan] = !values.d_nan
  endif

  return, out
end
