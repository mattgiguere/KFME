function airy, epsilon, v
;Calculate an airy profile for a circular aperture with central obscuration.
;
;Inputs:
; epsilon (scalar) radius of the central obscuration divided by radius of
;   the main aperture. Both are assumed to be circular.
; v (vector) dimensionless independent variable: (2*pi/lambda)*(a/R)*r,
;   where
;             lambda = wavelength of light (same units as r)
;             a      = radius of main aperture (same units as R)
;             R      = distance from aperture center to image plane center
;             r      = distance from optical axis in image plane
;
;Returns:
; (vector) Slice through Airy function at specified values of v.
;
;Example:
; IDL> v=dindgen(2001)/50
; IDL> plot,v/!pi,alog10(airy(0.0,v)),min=-4,xr=[0,7],/xsty $
; IDL>     ,xtit='v/!mp!x', ytit='log PSF'
; IDL> oplot,v/!pi,alog10(airy(0.33,v)),li=2,th=2
;
;History:
; 1999-Aug-02 JAV  Adpated from Chapter 10 of Astronomical Optics,
;		     Schroeder, D. J. 1987, Academic Press Inc, London.

if n_params() lt 2 then begin
  print, 'psf = airy(epsilon, v)'
  retall
endif

;Force double precision. Prevent divide by zero. Don't alter input argument.
  vv = double(v)
  iz = where(v eq 0, nz)
  if nz gt 0 then vv(iz) = 1.0

;Calculate slice through Airy function.
  psf = (2.0 / vv / (1-epsilon^2))^2.0 $
      * (beselj(vv,1) - epsilon*beselj(epsilon*vv,1))^2.0

;Fix point at center of Airy pattern.
  if nz gt 0 then psf(iz) = 1.0

;Return the result.
  return, psf

end
