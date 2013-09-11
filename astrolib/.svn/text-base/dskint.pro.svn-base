function dskint,mu,inten,deltav,vsini_in,vturb_in,osamp=osamp
;+
;NAME:
;	DSKINT
;
;PURPOSE:
;	Produces a flux profile by integrating intensity profiles (sampled
;	  at various mu angles) over the visible stellar surface.
;
;CALLING SEQUENCE:
;	flux = DSKINT(mu, inten, deltav, vsini, vturb)
;
;INPUTS:
;	MU: (vector(nmu)) cosine of the angle between the outward normal and
;	  the line of sight for each intensity spectrum in INTEN.
;	INTEN: (array(npts,nmu)) intensity spectra at specified values of MU.
;	DELTAV: (scalar) velocity spacing between adjacent spectrum points
;	  in INTEN (same units as VSINI and VTURB).
;	VSINI (scalar) maximum radial velocity, due to solid-body rotation.
;	VTURB (scalar) sqrt(2) times the standard deviation of an assumed
;	  isotropic gaussian distribution of macroturbulent velocities. The
;	  sqrt(2) is unfortunately standard.
;
;INPUT KEYWORDS:
;	OSAMP: (scalar) internal oversampling factor for convolutions. By
;	  default convolutions are done using the input points (OSAMP=1),
;	  but when OSAMP is set to higher integer values, the input spectra
;	  are first oversampled by cubic spline interpolation.
;
;OUTPUTS:
;	Function Value: (vector(npts)) Disk integrated flux profile.
;
;RESTRICTIONS:
;	Intensity profiles are weighted by the fraction of the projected
;	  stellar surface they represent, apportioning the area between
;	  adjacent MU points equally. Additional weights (such as those
;	  used in a Gauss-Legendre quadrature) can not meaningfully be
;	  used in this scheme.  About twice as many points are required
;	  with this scheme to achieve the precision of Gauss-Legendre
;	  quadrature.
;	DELTAV, VSINI, and VTURB must all be in the same units (e.g. km/s).
;	If specified, OSAMP should be a positive integer.
;
;AUTHOR'S REQUEST:
;	If you use this algorithm in work that you publish, please cite
;	  Valenti & Anderson 1995, PASP, currently in preparation.
;
;MODIFICATION HISTORY:
;	   Feb-88  GM	Created ANA version.
;	13-Oct-92 JAV	Adapted from G. Marcy's ANA routine of the same name.
;	03-Nov-93 JAV	Switched to annular convolution technique.
;	12-Nov-93 JAV	Fixed bug. Intensity components not added when vsini=0.
;	14-Jun-94 JAV	Reformatted for "public" release. Heavily commented.
;			Pass deltav instead of 2.998d5/deltav. Added osamp
;			  keyword. Added rebinning logic at end of routine.
;			Changed default osamp from 3 to 1.
;	20-Feb-95 JAV	Added mu as an argument to handle arbitrary mu sampling
;			  and remove ambiguity in intensity profile ordering.
;			Interpret VTURB as sqrt(2)*sigma instead of just sigma.
;			Replaced call_external with call to nr_spl{ine|int}.
;	03-Apr-95 JAV	Multiply flux by !pi to give observed flux.
;	24-Oct-95 JAV	Force "nmk" padding to be at least 3 pixels.
;-

if n_params() lt 5 then begin
  print,'syntax: flux = dskint(mu,inten,deltav,vsini,vturb [,osamp=])'
  retall
endif

;Make local copies of various input variables, which will be altered below.
  vsini = float(vsini_in)			;ensure real number
  vturb = float(vturb_in)			;ensure real number

;Determine oversampling factor.
  if n_elements(osamp) eq 0 then osamp = 0	;make sure variable is defined
  os = round(osamp > 1)				;force integral value > 0

;Convert input MU to projected radii, R, of annuli for a star of unit radius
;  (which is just sine, rather than cosine, of the angle between the outward
;  normal and the line of sight).
  rmu = sqrt(1.0 - mu^2)			;use simple trig identity

;Sort the projected radii and corresponding intensity spectra into ascending
;  order (i.e. from disk center to the limb), which is equivalent to sorting
;  MU in descending order.  Make a local copy of the intensity spectra, so
;  as not to molest the input variable.
  isort = sort(rmu)				;sorted indicies
  rmu = rmu(isort)				;reorder projected radii
  sint = inten					;copy inten for sorting
  nmu = n_elements(mu)				;number of radii 
  for imu = 0, nmu - 1 do begin			;loop thru radii
    sint(*, imu) = inten(*, isort(imu))		;sort intensity spectra
  endfor

;Calculate projected radii for boundaries of disk integration annuli.  The n+1
;  boundaries are selected such that r(i+1) exactly bisects the area between
;  rmu(i) and rmu(i+1). The innermost boundary, r(0) is set to 0 (disk center)
;  and the outermost boundary, r(nmu) is set to 1 (limb).
  r = sqrt(0.5 * ( rmu(0:nmu-2) ^ 2 $
                 + rmu(1:nmu-1) ^ 2 ) )		;area midpoints between rmu
  r = [0, temporary(r), 1]			;bookend with center and limb

;Calculate integration weights for each disk integration annulus.  The weight
;  is just given by the relative area of each annulus, normalized such that
;  the sum of all weights is unity.  Weights for limb darkening are included
;  explicitly in the intensity profiles, so they aren't needed here.
  wt = r(1:nmu) ^ 2 - r(0:nmu-1) ^ 2		;weights = relative areas

;Generate index vectors for input and oversampled points. Note that the
;  oversampled indicies are carefully chosen such that every "os" finely
;  sampled points fit exactly into one input bin. This makes it simple to
;  "integrate" the finely sampled points at the end of the routine.
  vinfo = size(sint)			;variable information block
  npts  = long(vinfo(1))		;# of points
  xpix  = dindgen(npts)			;point indices
  nfine = long(os*npts)			;# of oversampled points
  xfine = (0.5 / os) $
	* (2*dindgen(nfine)-os+1) 	;oversampled points indices

;Loop through annuli, constructing and convolving with rotation kernels.
  dummy = 0L				;init call_ext() return value
  yfine = dblarr(nfine,/nozero)		;init oversampled intensities
  flux = dblarr(nfine)			;init flux vector
  if vsini gt 0 then begin		;true: nontrivial case
    for imu=0,nmu-1 do begin		;loop thru integration annuli

;Use external cubic spline routine (adapted from Numerical Recipes) to make
;  an oversampled version of the intensity profile for the current annulus.
;  IDL (tensed) spline is nice, but *VERY* slow. Note that the spline extends
;  (i.e. extrapolates) a fraction of a point beyond the original endpoints.
      r1 = r(imu)			;inner edge of annulus
      r2 = r(imu+1)			;outer edge of annulus
      ypix = double(sint(*,imu))	;extract intensity profile
      if os eq 1 then begin		;true: no oversampling
	yfine = ypix			;just copy (use) original profile
      endif else begin			;else: must oversample
        secder = nr_spline(xpix, ypix)	;find spline coefficients
	yfine = nr_splint(xpix, ypix $
	          ,secder, xfine)	;spline onto fine wavelength scale
      endelse

;Construct the convolution kernel which describes the distribution of
;  rotational velocities present in the current annulus. The distribution has
;  been derived analytically for annuli of arbitrary thickness in a rigidly
;  rotating star. The kernel is constructed in two pieces: one piece for
;  radial velocities less than the maximum velocity along the inner edge of
;  the annulus, and one piece for velocities greater than this limit.
      dv = deltav / os				;oversampled velocity spacing
      maxv = vsini * r2				;maximum velocity in annulus
      nrk = 2*long(maxv/dv) + 3			;# oversampled kernel point
      v = dv * (dindgen(nrk) - ((nrk-1)/2))	;velocity scale for kernel
      rkern = dblarr(nrk)			;init rotational kernel
      j1 = where(abs(v) lt vsini*r1,n1)		;low velocity points
      if n1 gt 0 then begin
        rkern(j1) $
	  = sqrt((vsini*r2)^2 - v(j1)^2) $
	  - sqrt((vsini*r1)^2 - v(j1)^2)	;generate distribution
      endif
      j2 = where(abs(v) ge vsini*r1 $		;higher velocity points
             and abs(v) le vsini*r2,n2)
      if n2 gt 0 then begin
        rkern(j2) $
	  = sqrt((vsini*r2)^2 - v(j2)^2)	;generate distribution
      endif
      rkern = rkern / total(rkern)		;normalize kernel

;Convolve the intensity profile with the rotational velocity kernel for this
;  annulus. Pad each end of the profile with as many points as are in the
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
      ypix = double(sint(*,imu))	;extract intensity profile
      if os eq 1 then begin		;true: no oversampling
	yfine = ypix			;just copy (use) original profile
      endif else begin			;else: must oversample
        secder = nr_spline(xpix, ypix)	;find spline coefficients
	yfine = nr_splint(xpix, ypix $
	          ,secder, xfine)	;spline onto fine wavelength scale
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

;Construct a gaussian macroturbulence kernel with a standard deviation
;  equal to VTURB/sqrt(2).
    sigma = os * vturb/ sqrt(2) / deltav	;standard deviation in points
    nmk = (fix(10*sigma)) < ((nfine-3)/2)	;extend kernel to 10 sigma
    nmk = nmk > 3				;pad with at least 3 pixels
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

;Finally, bin the oversampled points to get the flux profile on the original
;  point scale.
  flux = reform(flux,os,npts,/overwrite)	;convert to an array
  return,!pi * total(flux,1) / os		;sum, normalize, and return

end
