function pfit_func, x, coef
;Generates a polynomial from nonlinear coefficients.  The polynomial is given
;  by {coef(0) + Sum(i=1,n) of (coef(i)*x)^i}, rather than the usual linear
;  form {Sum(i=0,n) of coef(i)*x^i}.
;Input:
; x (vector(n)) independent variable.
; coef (vector(ndeg+1)) nonlinear polynomial coefficients.
;Output:
; y (vector(n)) polynomial fit evaluated at x.
;Notes:
; Called by pfit_func.pro via marq.pro.
;History:
; 26-Feb-95 Valenti  Initial coding.

if n_params() lt 2 then begin
  print, 'syntax: y = pfit_func(x, coef)'
  retall
endif

;Generate the polynomial.
  ndeg = n_elements(coef) - 1			;degree of polynomial
  y = replicate(coef(0), n_elements(x))		;init y with constant term
  for i = 1, ndeg do y = y + (coef(i) * x) ^ i	;add remaining terms
  return, y					;return polynomial

end
