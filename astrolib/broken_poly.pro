function broken_poly, x, par
;Calculate a broken polynomial.
;Input:
; x (vector) independent variable
; par (vector) parameters of broken polynomial (see below)
;Returns:
; Value of broken polynomial at x.

if n_params() lt 2 then begin
  print, 'syntax: y = broken_poly(x, par)'
  return, 0
endif

  nx = n_elements(x)
  y = fltarr(nx)

;Extract parameters.
  deg0 = fix(round(par(0)))		;degree of polynomial for small x
  deg1 = fix(round(par(1)))		;degree of polynomial for large x
  xbrk = par(2)				;value of x where polynomial breaks
  coef0 = par(3:3+deg0)			;polynomial coefficients for small x
  coef1 = [0, par(4+deg0:3+deg0+deg1)]	;polynomial coefficients for large x

;Polynomial segment for small x.
  i0 = where(x le xbrk, n0)
  if n0 gt 0 then y(i0) = poly(x(i0), coef0)

;Polynomial segment for large x.
  i1 = where(x gt xbrk, n1)
  if n1 gt 0 then begin
    coef1(0) = poly(xbrk, coef0)
    y(i1) = poly(x(i1)-xbrk, coef1)
  endif

;Done.
  return, y

end
