function gpoly_fit, x, y, terms, cond, thresh=thresh
;+
; NAME: gpoly_fit
;
; PURPOSE: Fits a generalized polynomial to data that is a function of more
;   than one independent variable. Cross-terms are permitted.
;
; CATEGORY: Curve-fitting.
;
; CALLING SEQUENCE: coef = gpoly_fit(x, y, terms)
;
; INPUTS:
;   x: array(ndata,nvar) - contains independent variables. Each column
;      corresponds to a datum in Y. Each row is a different independent
;      variable.
;
;   y: vector(ndata) - dependent data that are to be fit.
;
;   terms: array(nvar, nterm) - specification of the polynomial to be fit.
;      Each column contains the exponent that is to be used for the variable
;      in the corresponding column in X. Each row is a different term in the
;      polynomial. See example below for clarification.
;
;   thresh: (scalar) - fractional threshold below which pivots in SVD will
;      be considered noise. The default is 1d-12, which is appropriate for
;      analytic data. For experimental data, this number may be set to the
;      typical fractional measurement uncertainty. If the condition number
;      returned in COND is significantly less than THRESH, the fit is not
;      unique and hence of dubious value.
;
; OUTPUT:
;   cond (scalar) - "condition number" of the linear system. For analytic
;      data COND should be well above 1d-12 (or 1e-6 if code is changed to
;      single precision). Otherwise, the linear system is poorly conditioned
;      (nearly singular) and the fit is *not* unique. This means both the
;      coefficients and the interpolating polynomial are basically USELESS.
;      For experimental data, it is probably wise to insist on a condition
;      number greater than the fractional uncertainty in the measurements.
;      If the condition number is too low, you should reduce the freedom in
;      the fitting function by including fewer polynomial TERMS.
;
;   return value: vector(nterm) - fit coefficients ordered as in TERMS.
;
; EXAMPLE:
;   x = [[-1, 2,-3, 4,-5], [1, 2, 3, 4, 5]]
;   y = 2 - 3*x(*,0) + 4*x(*,1) - 0.5*x(*,0)*x(*,1)
;   terms = [[0,0], [1,0], [0,1], [1,1]]
;   coef = gpoly_fit(x, y, terms, cond)
;   if cond lt 1d-10 then message, 'Use fewer terms in polynomial'
;   print, cond
;   % 0.024915679
;   print, coef
;   % 2.0000000      -3.0000000       4.0000000     -0.50000000
;
; NOTES:
;   Singular Value Decomposition (SVD) is used to solve linear system.
;   For poorly constrained problems, coefficients are NOT unique. In this
;     case, the resulting polynomial should NOT be used for interpolation.
;   Double precision is used internally.
;
; HISTORY:
;   26-Jul-97 Valenti  Wrote.

if n_params() lt 3 then begin
  print, 'syntax: coef = gpoly_fit(x, y, terms)'
  retall
endif

;Set threshold to default, if not supplied as an argument.
  if not keyword_set(thresh) then thresh = 1d-10

;Get sizes. Check for bad argument sizes.
  sx = size(x)
  sy = size(y)
  st = size(terms)
  if sx(0) ne 2 then message, 'X must be two-dimensional array'
  if sy(0) ne 1 then message, 'Y must be one-dimensional vector'
  if st(0) ne 2 then message, 'TERMS must be two-dimensional array'
  ndata = sx(1)
  if sy(1) ne ndata then message, 'sizes of X and Y do not agree'
  nvar = sx(2)
  if st(1) ne nvar then message, 'sizes of X and TERMS do not agree'
  nterm = st(2)

;Build "design" matrix for SVD solver.
  array = dblarr(nterm, ndata)
  for i=0, nterm-1 do begin
    prod = 1d0
    for j=0, nvar-1 do prod = prod * x(*,j)^terms(j,i)
    array(i,*) = prod
  endfor

;Call SVD solver (always use double precision).
  svdc, array, w, u, v, /double
  cond = min(w, max=wmax) / wmax
  ismall = where(w/wmax lt thresh, nsmall)
  if nsmall gt 0 then w(ismall) = 0.0
  coef = svsol(u, w, v, y, /double)

;Return generalized polynomial coefficients.
  return, coef

end
