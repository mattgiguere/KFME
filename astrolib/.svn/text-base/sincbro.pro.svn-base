function sincbro,w,s,hwhm
;Smooths a spectrum by convolution with a sinc function of specified hwhm.
; w (input vector) wavelength scale of spectrum to be smoothed
; s (input vector) spectrum to be smoothed
; hwhm (input scalar) half width at half maximum of smoothing gaussian.
;Returns a vector containing the gaussian-smoothed spectrum.
;Edit History:
;  -Dec-90 GB,GM Rewrote with fourier convolution algorithm.
;  -Jul-91 AL	Translated from ANA to IDL.
;22-Sep-91 JAV	Relaxed constant dispersion check; vectorized, 50% faster.
;05-Jul-92 JAV	Converted to function, handle nonpositive hwhm.
;14-Nov-93 JAV	Adapted from macbro.pro
;23-Apr-93 JAV	Verified that convolution kernel has specified hwhm. For IR FTS
;		 spectra: hwhm=0.0759 Angstroms, max change in profile is 0.4%
;		 of continuum.

if n_params() lt 3 then begin
  print,'syntax: snew=sincbro(w,sold,hwhm)'
  retall
endif

;Warn user if hwhm is negative.
  if hwhm lt 0.0 then $
    message,/info,'Warning! Forcing negative smoothing width to zero.'

;Return input argument if half-width is nonpositive.
  if hwhm le 0.0 then return,s			;true: no broadening

;Calculate (uniform) dispersion.
  nw = n_elements(w)				;# points in spectrum
  dw = (w(nw-1) - w(0)) / (nw-1)		;wavelength change per pixel

;Make sinc function out to 20th zero-crossing on either side. Error due to
; ignoring additional lobes is less than 0.2% of continuum. Reducing extent
; to 10th zero-crossing doubles maximum error.
  fwhm = 2.0 * hwhm				;full width at half maximum
  rperfw = 0.26525				;radians per fwhm of sinc
  xrange = 20*!pi				;20th zero of sinc (radians)
  wrange = xrange * fwhm * rperfw		;20th zero of sinc (wavelength)
  nhalf = fix(wrange/dw+0.999)			;# points in half sinc
  nsinc = 2*nhalf + 1				;# points in sinc (odd!)
  wsinc = (findgen(nsinc) - nhalf) * dw		;absissca (wavelength)
  xsinc = wsinc / (fwhm * rperfw)		;absissca (radians)
  xsinc(nhalf) = 1.0				;avoid divide by zero
  sinc = sin(xsinc) / xsinc			;calculate sinc
  sinc(nhalf) = 1.0				;insert midpoint
  xsinc(nhalf) = 0.0				;fix xsinc
  sinc = sinc / total(sinc)			;normalize sinc

;Pad spectrum ends to minimize impact of Fourier ringing.
  npad = nhalf + 2				;# pad pixels on each end
  spad = [replicate(s(0),npad),s,replicate(s(nw-1),npad)]

;Convolve and trim.
  sout = convol(spad,sinc)			;convolve with gaussian
  sout = sout(npad:npad+nw-1)			;trim to original data/length
  return,sout					;return broadened spectrum.

End
