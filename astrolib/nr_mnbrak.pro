pro nr_mnbrak, ax, bx, cx, fa, fb, fc, func
;Given a function FUNC, and given distinct initial points AX and BX, this
; routine searches in the downhill direction (defined by the function as
; evaluated at the initial points) and returns new points AX, BX, and CX
; which bracket a minimum of the function. Also returned are the function
; values at the three points, FA, FB, and FC.
;Note that BX is between AX and CX and FB is less than FA and FC, but the
; sequence AX, BX, CX may be increasing or decreasing.
;08-Dec-95 Valenti  Adapted from Numerical Recipes.

if n_params() lt 7 then begin
  print, 'syntax: nr_mnbrak, ax, bx, cx, fa, fb, fc, func'
  retall
endif

;Internal program parameters.
  gold = 1.618034		;ratio magnification each step
  glimit = 100			;maximum parabolic magnification
  tiny = 1e-20

;Initial function evaluations.
  fa = call_function(func, ax)
  fb = call_function(func, bx)

;Find downhill direction. Shuffle variables as needed.
  if fb gt fa then begin
    dum = ax				;swap AX and BX
    ax = bx
    bx = dum
    dum = fb				;swap FA and FB
    fb = fa
    fa = dum
  endif

;Make first guess for CX and evaluate function.
  cx = bx + gold * (bx-ax)
  fc = call_function(func, cx)

;Loop until minimum is bracketted.
  while fb ge fc do begin
    r = (bx-ax) * (fb-fc)
    q = (bx-cx) * (fb-fa)
    if q gt r then begin
      u = bx - ((bx-cx)*q - (bx-ax)*r) / (2.0 * ((q-r) > tiny))
    endif else begin
      u = bx - ((bx-cx)*q - (bx-ax)*r) / (2.0 * ((q-r) < (-tiny)))
    endelse
    ulim = bx + glimit * (cx-bx)

    if (bx-u)*(u-cx) gt 0 then begin

;Parabolic minimum is between BX and CX.
      fu = call_function(func, u)
      if fu lt fc then begin		;true: found min between BX and CX
        ax = bx
        fa = fb
        bx = u
        fb = fu
        return				;done
      endif else begin
        if fu gt fb then begin		;true: found min between AX and U
          cx = u
          fc = fu
          return			;done
        endif
      endelse
      u = cx + gold * (cx-bx)		;parabolic fit no good, use GOLD
      fu = call_function(func, u)

    endif else begin
      if (cx-u) * (u-ulim) gt 0 then begin

;Minimum of extrapolated parabola is within ULIM.
        fu = call_function(func, u)
        if fu lt fc then begin
          bx = cx
          cx = u
          u = cx + gold * (cx-bx)
          fb = fc
          fc = fu
          fu = call_function(func, u)
        endif
      endif else begin

        if (u-ulim)*(ulim-cx) ge 0 then begin
;Minimum of extrapolated parabola is too far, so use limit.

          u = ulim
          fu = call_function(func, u)
        endif else begin

;Parabolic stuff was all useless. Just use default magnification in GOLD.
          u = cx + gold*(cx-bx)
          fu = call_function(func, u)
        endelse
      endelse
    endelse

;Eliminate oldest point and continue.
    ax = bx
    bx = cx
    cx = u
    fa = fb
    fb = fc
    fc = fu
  endwhile
end
