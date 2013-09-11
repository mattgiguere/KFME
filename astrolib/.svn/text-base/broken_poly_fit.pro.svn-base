function broken_poly_fit, x, y, sig, ndeg0, ndeg1, xbrk
;Fit a broken polynomial. The two segments are constrained to have equal
; value at the break point. SVD is used to determine weighted least-squares
; fit. Solution depends nonlinearly on break point, so this parameter must
; be specified a priori and remains fixed.
;Returns coefficients of broken polynomial, including polynomial degrees
; and break point: [ndeg0, ndeg1, xbrk, [coef]].
;Weights have not been tested.
;History:
; 27-May-1999 Valenti  Wrote.

if n_params() lt 6 then begin
  print, 'syntax: coef = broken_poly_fit(x, y, sig, ndeg0, ndeg1, xbrk)'
  return, 0
endif

;Sizes.
  nx = n_elements(x)
  if n_elements(y) ne nx then begin
    message, /info, 'x and y have different sizes'
    return, 0
  endif
  nterm = ndeg0 + ndeg1 + 1

;Initializations.
  a = dblarr(nterm, nx)

;Setup on low side of break.
  j0 = where(x le xbrk, n0)
  if n0 gt 0 then begin
    if n_elements(sig) gt 1 then sig0 = sig(j0) else sig0 = sig
    x0 = x(j0)
    for i=0, ndeg0 do begin
      a(i,j0) = (x0/sig0)^i
    endfor
  endif

;Setup on high side of break.
  j1 = where(x gt xbrk, n1)
  if n1 gt 0 then begin
    if n_elements(sig) gt 1 then sig1 = sig(j1) else sig1 = sig
    for i=0, ndeg0 do begin
      a(i,j1) = (xbrk/sig1)^i
    endfor
    x1_xb = x(j1) - xbrk
    for i=1, ndeg1 do begin
      a(ndeg0+i,j1) = (x1_xb/sig1)^i
    endfor
  endif

;Solve least-squares problem using singular value decomposition.
  svdc, a, w, u, v, /double
  ysig = y / sig
  coef = svsol(u, w, v, ysig, /double)

;Return parameters of fit.
  return, [ndeg0, ndeg1, xbrk, coef]

end
