function diski,inten,deltav,vsini,vturb,osamp=osamp
;+
;NAME:
;	DISKI
;
;PURPOSE:
;	Integrates intensity profiles from 
;
;CALLING SEQUENCE:
;	flux = DISKI(inten,pixdsp,vsini,vturb)
;
;INPUTS:
;	INTEN: (array(# pixels,# angles)) Intensity profiles at various
;	  viewing (mu) angles. The specific intensities must be specified
;	  at mu angles that divide the star into equal area annuli, ordered
;	  from disk center (mu=1) to the limb (mu=0). Reversing this ordering
;	  leads to an inobvious failure mode (slight reversal in line core).
;	DELTAV: (scalar) Velocity separation between adjacent pixels in INTEN.
;	VSINI (scalar) Maximum projected rotational velocity.
;	VTURB (scalar) Standard deviation of an assumed gaussian distribution
;	  of turbulent velocities.
;
;KEYWORD PARAMETERS:
;	OSAMP: (scalar) oversampling factor to be used in the convolutions
;	  in this routine. Omitting the OSAMP keyword or specifying a value
;	  less than 0.5 forces use of the default value of 1. Nonintegral
;	  values are rounded to the nearest integer prior to use.
;
;OUTPUTS:
;	Function Value: (vector(# pixels)) Disk integrated flux profile.
;
;RESTRICTIONS:
;	DELTAV, VSINI, and VTURB must all be in the same units (e.g. km/s).
;	If specified, OSAMP should be a positive integer.
;	Distribute freely, but if you publish results based on this routine,
;	  please cite Valenti & Anderson 1995, PASP, which is currently in
;	  preparation.
;
;MODIFICATION HISTORY:
;	   Feb-88  GM	Created ANA version.
;	13-Oct-92 JAV	Adapted from G. Marcy's ANA routine of the same name.
;	03-Nov-93 JAV	Switched to annular convolution technique.
;	12-Nov-93 JAV	Fixed bug. Intensity components not added when vsini=0.
;	14-Jun-94 JAV	Reformatted for "public" release. Heavily commented.
;			Pass deltav instead of 2.998d5/deltav. Added osamp
;			keyword. Added rebinning logic at end of routine.
;			Changed default osamp from 3 to 1.
;-

if n_params() lt 4 then begin
  print,'syntax: flux = diski(inten,deltav,vsini,vturb [,osamp=])'
  retall
endif

;Ensure vsini and vturb are real.
  vsini = float(vsini)
  vturb = float(vturb)

;Set program parameter(s) (i.e. internal pixel oversampling factor).
  if n_elements(osamp) eq 0 then osamp = 0	;make sure variable is defined
  os = round(osamp)				;round, put in local variable
  if os le 0 then os = 1			;ignore bogus, use default

;Recompute "equal area" quadrature angles that were used to compute the
;  intensities. Different mu angles could probably be implemented by inserting
;  quadrature weights. In the following, "phi" is the angle (in radians)
;  between the line-of-sight towards the observer and the normal to the stellar
;  surface at the boundaries of the annuli (including the center and limb).
  vinfo = size(inten)			;variable info block
  if vinfo(0) eq 1 then begin		;true: only 1 mu angle
    nmu = 1				;# of mu points (1)
  endif else begin			;else: multiple mu angles
    nmu = vinfo(2)			;# of mu angles
  endelse
  phi = asin(sqrt(findgen(nmu+1)/nmu))	;angles to line-of-sight (in radians)

;Generate index vectors for input and oversampled pixels. Note that the
;  oversampled indicies are carfefully chosen such that every "os" finely
;  sampled pixels fit exactly into one input pixel. This makes it simple to
;  "integrate" the finely sampled pixels at the end of the routine.
  npix = long(vinfo(1))			;# of pixels
  xpix = dindgen(npix)			;pixel indices
  nfine = long(os*npix)			;# of oversampled pixels
  xfine = (0.5 / os) $
	* (2*dindgen(nfine)-os+1) 	;oversampled pixel indices

;Loop through annuli, constructing and convolving with rotation kernels.
  dummy = 0L				;init call_ext() return value
  yfine = dblarr(nfine,/nozero)		;init oversampled intensities
  flux = dblarr(nfine)			;init flux vector
  if vsini gt 0 then begin		;true: nontrivial case
    for imu=0,nmu-1 do begin		;loop thru integration annuli

;Use external cubic spline routine (adapted from Numerical Recipes) to make
;  an oversampled version of the intensity profile for the current annulus.
;  IDL (tensed) spline is nice, but *VERY* slow. Note that the spline extends
;  (i.e. extrapolates) a fraction of a pixel beyond the original endpoints.
      sinphi1 = sin(phi(imu))		;inner edge of annulus
      sinphi2 = sin(phi(imu+1))		;outer edge of annulus
      ypix = double(inten(*,imu))	;extract intensity profile
      if os eq 1 then begin		;true: no oversampling
	yfine = ypix			;just copy (use) original profile
      endif else begin			;else: must oversample
        dummy = call_external('/luna/valenti/lib/spline.so','spline' $
	        ,npix,xpix,ypix,nfine,xfine,yfine)
      endelse

;Construct the convolution kernel which describes the distribution of
;  rotational velocities present in the current annulus. The distribution has
;  been derived analytically for annuli of arbitrary thickness in a rigidly
;  rotating star. The kernel is constructed in two pieces: one piece for
;  radial velocities less than the maximum velocity along the inner edge of
;  the annulus, and one piece for velocities greater than this limit.
      dv = deltav / os			;velocity spacing oversampled pixels
      maxv = vsini * sinphi2			;maximum velocity in annulus
      nrk = 2*long(maxv/dv) + 3			;# oversampled kernel pixels
      v = dv * (dindgen(nrk)-((nrk-1)/2))	;velocity scale for kernel
      rkern = dblarr(nrk)		;init rotational kernel
      j1 = where(abs(v) lt vsini*sinphi1,n1)	;low velocity points
      if n1 gt 0 then begin
        rkern(j1) $
	  = sqrt((vsini*sinphi2)^2 - v(j1)^2) $
	  - sqrt((vsini*sinphi1)^2 - v(j1)^2)	;generate distribution
      endif
      j2 = where(abs(v) ge vsini*sinphi1 $	;higher velocity points
             and abs(v) le vsini*sinphi2,n2)
      if n2 gt 0 then begin
        rkern(j2) $
	  = sqrt((vsini*sinphi2)^2 - v(j2)^2)	;generate distribution
      endif
      rkern = rkern / total(rkern)		;normalize kernel

;Convolve the intensity profile with the rotational velocity kernel for this
;  annulus. Pad each end of the profile with as many pixels as are in the
;  convolution kernel. This reduces Fourier ringing. The convolution may also
;  be done with a routine called "externally" from IDL, which efficiently
;  shifts and adds.
      if nrk gt 3 then begin
        lpad = replicate(yfine(0),nrk)		;padding for the "left" side
        rpad = replicate(yfine(nfine-1),nrk)	;padding for the "right" side
        yfine = convol([lpad,yfine,rpad],rkern)	;add the padding and convolve
        yfine = yfine(nrk:nrk+nfine-1)		;trim away padding
      endif
      flux = flux + yfine			;add profile to running total
    endfor

;Handle the trivial case of no rotation. Just oversample and add the intensity
;  profiles (again assuming equal area annuli, and hence equal weights).
  endif else begin			;else: no rotational broadening
    for imu=0,nmu-1 do begin		;loop thru integration annuli
      ypix = double(inten(*,imu))	;extract intensity profile
      if os eq 1 then begin		;true: no oversampling
	yfine = ypix			;just copy (use) original profile
      endif else begin			;else: must oversample
        dummy = call_external('/luna/valenti/lib/spline.so','spline' $
	        ,npix,xpix,ypix,nfine,xfine,yfine)
      endelse
      flux = flux + yfine		;add in annular contribution
    endfor
  endelse
  flux = flux / nmu			;renormalize contributions from annuli

;For isotropic macroturbulence, we can do the convolution after adding the
;  annuli, since convolutions are distributive with respect to addition.
;To include nonisotropic macrturbulence (i.e. vrad ne vtan), stuff the
;  following code back into the mu loop and make sigma a function of mu.
  if vturb gt 0 then begin			;true: nontrivial case

;Construct a gaussian macroturbulence kernel with a standard deviation given
;  by the vturb argument.
    sigma = os * vturb / deltav			;standard deviation in pixels
    nmk = (fix(10*sigma)) < ((nfine-3)/2)	;extend kernel to 10 sigma
    mkern = exp(-0.5 $
	  * ((dindgen(2*nmk+1)-nmk)/sigma)^2)	;compute the gaussian
    mkern = mkern / total(mkern)		;normalize the profile

;Convolve the total flux profiles, again padding the spectrum on both ends to
;  protect against Fourier ringing.
    lpad = replicate(flux(0),nmk)		;padding for the "left" side
    rpad = replicate(flux(nfine-1),nmk)		;padding for the "right" side
    flux = convol([lpad,flux,rpad],mkern)	;add the padding and convolve
    flux = flux(nmk:nmk+nfine-1)		;trim away padding
  endif

;Finally, bin the oversampled pixels to get the flux profile on the original
;  pixel scale.
  flux = reform(flux,os,npix,/overwrite)	;convert to an array
  return,total(flux,1) / os			;sum, normalize, and return


end
