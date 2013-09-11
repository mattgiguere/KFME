pro debin,xold,yold,xnew,ynew
;Finds values of a function, YNEW, at finer sampling, preserving integral.
;Iteratively computes the values of a function, YNEW, sampled at XNEW,
;such that its integral, in bins centered at XOLD, yields YOLD.
;Use the Function, REBIN, to compute integral.
;Note: DEBIN should be used when finer sampling is desired.
;      REBIN should be used when coarser sampling is desired.
;INPUT:  
;      XOLD = original abscissa array
;      YOLD = underlying function binned per XOLD
;      XNEW = new abscissa array - more finely spaced than XOLD
;OUTPUT:
;      YNEW = function values such that binning it (with REBIN)
;             yields the original YOLD.
;
;Create: GWM,JAV Mar.1993
;
if n_params() lt 3 then begin
  print,'syntax: yold = debin(xold,yold,xnew)'
  retall
endif

  niter=5                              ;Number of iterations
  fudg = 0.95                          ;fudge factor to slow convergence
  nnew = n_elements(xnew)
  nold = n_elements(xold)

  if xnew(0) lt xold(0) or xnew(nnew-1) gt xold(nold-1) then $        
    message,'New wavelength scale extends beyond old wavelength scale.'

;Trim input spectrum to just encapsulate xnew
  ind  = where(xold ge xnew(0) and xold le xnew(nnew-1),num)  ;indices
  ind  = [ind(0)-1,ind,ind(num-1)+1]           ;indices encapsulating xnew  
  y    = yold(ind)                             ;trimmed y
  x    = xold(ind)                             ;trimmed x
  len  = n_elements(y)                         ;length of y

;Extend xnew scale (xnewbig) artificially to enable rebin to xold
  dxlo = xnew(1)-xnew(0)                      ;delta x per pixel
  dxhi = xnew(nnew-1) - xnew(nnew-2)          ;same at right end
  nlo  = fix(2. * (x(1)-x(0)) / dxlo)         ;# pixels needed lo end
  nhi  = fix(2. * (x(len-1)-x(len-2)) / dxhi)     ;# at hi end
  extlo = fltarr(nlo) * 0.                    ;blank extension array
  exthi = fltarr(nhi) * 0.                    ;blank extension array
  ;extend xnew to beyond the ends of input x scale
  xnewlo = xnew(0) - dxlo * reverse(indgen(nlo)+1)
  xnewhi = xnew(nnew-1) + dxhi * (indgen(nhi)+1)
  xnewbig=[xnewlo, xnew , xnewhi]
  ynew = fspline(x,y,xnew)            ;0th guess of ynew (finely sampled)

;Iterate on a finely sampled guess for YNEW
  FOR j=0,niter-1 do begin
    ynewbig = [ynew(0)+extlo, ynew , ynew(nnew-1)+exthi]  ;extend ynew 
;   rebin,xnewbig,ynewbig,x,ybin         ;bin 0th guess
    ybin=resamp(double(xnewbig) $
               ,double(ynewbig) $
               ,double(x))               ;bin 0th guess
    dif  = float(ybin) - y               ;dif (binned guess - truth)
    sdif = fspline(x,dif,xnew)           ;dif splined to fine sampling
    ynew = ynew - fudg * sdif            ;Apply corrections to 0th guess
  END
return
end
