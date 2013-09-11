function gpoly, x, coef, terms
;+
; NAME: gpoly
;
; PURPOSE: Generates a generalized polynomial of more than one independent
;   variable. Cross-terms are permitted. Also see gpoly_fit.pro.
;
; CATEGORY: Curve-fitting.
;
; CALLING SEQUENCE: y = gpoly(x, coef, terms)
;
; INPUTS:
;   x: array(ndata,nvar) - contains independent variables. Each column
;      corresponds to a datum in Y. Each row is a different independent
;      variable.
;
;   coef: vector(nterm) - coefficients for terms specified in TERMS.
;
;   terms: array(nvar, nterm) - specification of the polynomial to be fit.
;      Each column contains the exponent that is to be used for the variable
;      in the corresponding column in X. Each row is a different term in the
;      polynomial. See example below for clarification.
;
; OUTPUT:
;   return value: vector(ndata) - polynomial evaluated at X.
;
; EXAMPLE:
;   IDL> x = [[-1, 2,-3, 4,-5], [1, 2, 3, 4, 5]]
;   IDL> y = 2 - 3*x(*,0) + 4*x(*,1) - 0.5*x(*,0)*x(*,1)
;   IDL> terms = [[0,0], [1,0], [0,1], [1,1]]
;   IDL> coef = gpoly_fit(x, y, terms, cond)
;   IDL> if cond lt 1d-10 then message, 'Use fewer terms in polynomial'
;   IDL> print, coef
;   %      2.0000000      -3.0000000       4.0000000     -0.50000000
;   IDL> yfit = gpoly(x, coef, terms)
;   IDL> print, max(abs(yfit/y-1))
;   %      4.2188475e-15
;
; NOTES:
;   Be wary for precision problems.
;
; HISTORY:
;   26-Jul-97 Valenti  Wrote.

if n_params() lt 3 then begin
  print, 'syntax: y = gpoly_fit(x, coef, terms)'
  retall
endif

;Get sizes. Check for bad argument sizes.
  sx = size(x)
  sc = size(coef)
  st = size(terms)
  if sx(0) ne 2 then message, 'X must be two-dimensional array'
  if sc(0) ne 1 then message, 'COEF must be one-dimensional vector'
  if st(0) ne 2 then message, 'TERMS must be two-dimensional array'
  ndata = sx(1)
  nvar = sx(2)
  nterm = sc(1)
  if st(1) ne nvar then message, 'sizes of X and COEF do not agree'
  if st(2) ne nterm then message, 'sizes of X and TERMS do not agree'

;Evaluate polynomial.
  y = dblarr(ndata)
  for i=0, nterm-1 do begin
    prod = replicate(1d0, ndata)
    for j=0, nvar-1 do prod = prod * x(*,j)^terms(j,i)
    y = y + coef(i) * prod
  endfor

;Return polynomial.
  return, y

end
