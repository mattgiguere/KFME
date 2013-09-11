function poly_marq, x, y, ndeg, ysig, yfit=yfit
;Fits a polynomial using a nonlinear least squares technique that is much 
;  better conditioned (but about 1000 times slower) than the direct solution
;  of the linear system.  This routine is for those times when one correct
;  answer is worth more than 1000 wrong answers.  The coefficients and the
;  poly() function should generate polynomial values that are accurate to
;  the precision level of the machine.  As always, however, the polynomial
;  coefficients themselves may have large errors and should not be trusted.
;To see what I mean about erros in poly_fit, try the following:
;  IDL> x = [4.5, 5.0, 5.5]
;  IDL> y = [0.3371, 0.3427, 0.3469]
;  IDL> c = poly_fit(x, y, 2)
;  IDL> plot, x, y, ps=2, xsty=3, yr=[0.335, 0.355]
;  IDL> u = 4.5 + findgen(101)/100
;  IDL> oplot, u, poly(u, c)
;  The "fit" is horrible, even though we are only fitting a quadratic to
;  3 points. This example can be fixed by using the /double keyword in the
;  call to poly_fit, but the same type of behavior can also occur even in
;  double precision.
;Input:
; x (vector(n)) independent variable.
; y (vector(n)) dependent variable at x.
; ndeg (scalar) degree of polynomial to fit to y(x).
; ysig (vector(n)) optional uncertainties for points in y.  Uncertainties may
;   be set to zero to mark points to be ignored in the fit.  If uncertainties
;   are not provided, all points are given equal wieght.
;Output:
; Returns coef (vector(ndeg+1)) polynomial coefficients.
; yfit (vector(n)) polynomial fit evaluated at x.
;Notes:
; Calls marc.pro.
;History:
; 26-Feb-95 Valenti  Initial coding.

if n_params() lt 3 then begin
  print, 'syntax: coef = poly_marq(x, y, ndeg [,ysig ,yfit=])'
  retall
endif

;Return to calling routine and stop on error.
; on_error, 2					;error stops one level up

;Get sizes of input vectors.
  n  = n_elements(x)				;# data points to fit
  ny = n_elements(y)				;# of y points
  ns = n_elements(ysig)				;# of ysig points

;Set equal weights, if ysig was not passed.
  if ns eq 0 then begin				;true: no sigma passed
    ysig = replicate(1, n)			;use equal weights
    ns = n					;update # of ysig points
  endif

;Check that argument lengths all agree. Give informative error message.
  if ny ne n then $
    stop, 'poly_marq: length of y [' + strtrim(ny, 2) $
      + '] does not agree with length of x [' $
      + strtrim(n, 2) + '].'
  if ns ne n then $
    stop, 'poly_marq: length of ysig [' + strtrim(ns, 2) $
      + '] does not agree with length of x and y [' $
      + strtrim(n, 2) + '].'

;Subtract mean of y: improves accuracy, simplifies initial coefficient guess.
  mny = total(y) / n				;mean of y
  yy  = y - mny					;shifted y values

;Set coefficient step sizes based on range of x and yy.
  ymin = min(y, max=ymax)			;extrema of y
  yran = (ymax - ymin)				;range of y values
  dcoef = replicate(1e-4 * yran, ndeg + 1)	;coefficient step sizes

;Set initial (double precision) parameter values as follows:
;  coef(0) = mean:  set to zero to match mean of yy
;  coef(1) = slope: set to slope of line passing through endpoints of y
;  Remaining coefficients (if any) set to zero, so initial guess is linear.
  coef = dblarr(ndeg + 1)			;coeff guess, all zeroes
  if ndeg gt 0 then begin			;true: have linear term
    coef(1) = (y(n-1) - y(0)) / (x(n-1) - x(0))	;guess for slope
  endif

;Call the nonlinear least squares fitting routine.
  yfit = marq('poly',x, yy, ysig, coef, dcoef)

;Adjust coefficients and fit for offset in yy.
  coef(0) = coef(0) + mny			;restore mean to coeffs
  yfit = yfit + mny				;restore mean to fit

;Return coefficients.
  return, coef

end
