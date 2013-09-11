function nr_brent, ax, bx, cx, f, tol, xmin
;Given a function F, and given a bracketing triplet of abscissas AX, BX, CX
; (such that BX is between AX and CX, and F(BX) is less than both F(AX) and
; F(CX)), this routine isolates the minimum to a fractional precision of about
; TOL using Brent's method. The abscissa of the minimum is returned as XMIN,
; and the function value is returned as the function value of nr_brent().
;Note that the sequence AX, BX, CX may be increasing or decreasing.
;09-Dec-95 Valenti  Adapted from Numerical Recipes.

if n_params() lt 6 then begin
  print, 'syntax: fmin = nr_brent, ax, bx, cx, func, tol, xmin'
  retall
endif

;Internal program parameters.
  itmax = 100			;maximum number of iterations
  cgold = 0.3819660		;golden ration
  zeps = 1e-10			;number near zero

;Initializations.
  a = ax < cx			;force ascending order: a < b
  b = ax > cx
  v = bx
  w = v
  x = v
  e = 0.0			;distance moved on step before last
  fx = call_function(f, x)
  fv = fx
  fw = fx

;Loop until done or maximum iteration count is reached.
  for iter=1, itmax do begin
    xm = 0.5 * (a + b)
    tol1 = tol * abs(x) + zeps
    tol2 = 2.0 * tol1
    if abs(x-xm) le (tol2 - 0.5*(b-a)) then begin
      xmin = x
      return, fx
    endif

;Construct a trial parabolic fit.
    badfit = (0 eq 0)				;assume bad parabolic fit
    if abs(e) gt tol1 then begin
      r = (x - w) * (fx - fv)
      q = (x - v) * (fx - fw)
      p = (x - v)*q - (x - w)*r
      q = 2.0 * (q - r)
      if q gt 0 then p = -p
      q = abs(q)
      etemp = e
      e = d
      if abs(p) ge abs(0.5 * q * etemp) $
          or p le q*(a - x) $
          or p ge q*(b - x) then begin
        badfit = (0 eq 0)			;bad parabolic fit
      endif else begin
        badfit = (0 eq 1)			;good parabolic fit
        d = p / q
        u = x + d
        if (u - a) lt tol2 or (b - u) lt tol2 then begin
          if xm gt x then d = tol1 else d = -tol1
        endif
      endelse
    endif

;Take a golden section step, if the parabolic fit was bad.
    if badfit then begin
      if x ge xm then e = a - x else e = b - x
      d = cgold * e
    endif

;Arrive here with D either from parabolic fit or from golden section.
    if abs(d) ge tol1 then begin
      u = x + d
    endif else begin
      if d gt 0 then u = x + tol1 else u = x - tol1
    endelse
    fu = call_function(f, u)
    if fu le fx then begin
      if u ge x then a = x else b = x
      v = w
      fv = fw
      w = x
      fw = fx
      x = u
      fx = fu
    endif else begin
      if u lt x then a = u else b = u
      if fu le fw or w eq x then begin
        v = w
        fv = fw
        w = u
        fw = fu
      endif else begin
        if fu le fv or v eq x or v eq w then begin
          v = u
          fv = fu
        endif
      endelse
    endelse
  endfor

;Did not converge.
  print, 'nr_brent: no convergence after ' + strtrim(itmax,2) + ' iterations.'
  xmin = x
  return, fx

end
