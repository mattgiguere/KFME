function blaze_func, abcissa, par
;+
;			blaze_func
;
; Generates a theoretical blaze function.
;
; CALLING SEQUENCE:
;	blaze = blaze_func(abcissa, par)
;
; INPUTS:
;       abcissa - vector(npix) input mlam converted to pixels 
;       par - vector(7) parameters controlling blaze function:
;		par(0) - foclen, focal length (same units as pixsiz)
;		par(1) - pixsiz, pixel size (same units as foclen)
;		par(2) - echelle order number
;		par(3) - alpha, incident angle onto echelle grating
;		par(4) - beta0, exit angle from grating along principle ray
;		par(5) - theta, echelle blaze angle
;		par(6) - norm, normalization factor
;               par(7) - blaze center, pix
;               par(8) - blaze width, pix
;
; NOTES:
;	All input angle are in DEGREES!
;
; HISTORY:
;	19-Mar-96 Valenti  Wrote.
;-

if n_params() lt 2 then begin
  print, 'syntax: blaze = blaze_func(abcissa, par)'
  print, '  par(0) - foclen, focal length (same units as pixsiz)
  print, '  par(1) - pixsiz, pixel size (same units as foclen)
  print, '  par(2) - echelle order number
  print, '  par(3) - alpha, incident angle onto echelle grating
  print, '  par(4) - beta0, exit angle from grating along principle ray
  print, '  par(5) - theta, echelle blaze angle
  print, '  par(6) - normalization factor
  print, '  par(7) - blaze center, pix'
  print, '  par(8) - blaze width, pix'
  retall
endif

;Useful quantities.
  npix = n_elements(abcissa)

;  the spectrum is assumed to be flat (i.e. all variation is taken out
;    in determining the raw blaze function)
  spec=fltarr(npix)+1.


;Calculate dbeta (in radians) from focal length and pixel size.
  foclen = par(0)
  pixsiz = par(1)
  dbeta = abcissa * pixsiz / foclen
;Convert angles to radians and pull apart parameters.
  order = par(2)
  alpha = par(3) * !dtor
  beta0 = par(4) * !dtor
  theta = par(5) * !dtor
  center = par(7)
  width  = par(8)

;Calculate beta.
  beta = beta0 + dbeta
;
; print,par(2),par(0),par(1),par(3),par(4),par(5),par(7),par(8),$
;   form='(1x,i3,i5,f5.2,3f6.2,2f11.7)'
;
;Calculate efficiency.
  x = 0.5 * (alpha + beta)
  y = order * !pi * cos(alpha) * sin(x - theta) / sin(x)
  yprime = center + width * y
    list=where(abs(yprime) lt 1.e-5)
      if list(0) gt -1 then yprime(list)=1.e-5
  blaze = cos(alpha) / cos(beta) * (sin(yprime) / yprime)^2
     mblaze=max(blaze)
     if mblaze lt 1.e-5 then mblaze=1.e-5
  blaze = blaze / mblaze			;normalize peak

;Return result.
  norm=par(6)
  return, blaze*spec*norm 

end


