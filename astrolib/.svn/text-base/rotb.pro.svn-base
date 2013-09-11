function rotb, s, vstep, vsini, eps
;+
;NAME:
;       ROTB
;
;PURPOSE:
;       Rotationally broadens a flux spectrum by convolving with a kernel
;       that gives the intensity-weighted distribution of velocities over
;       the stellar surface. The intensity weights are specified by a
;       generalized limb-darkening law of the form:
;         I(mu)/I(0) = 1-total(eps) + eps(0)*mu + eps(1)*mu^2 + ....
;
;CALLING SEQUENCE:
;       sb = ROTB(s, vstep, vsini, eps)
;
;INPUTS:
;       S: (vector) flux spectrum to rotationally broaden. This spectrum
;          should be the integral of the specific intensity over the entire
;          surface. For example, an observed spectrum of a nonrotating star
;          could be used. If model specific intensities are available, disk
;          integration (e.g. using rtint.pro) is the preferred means of
;          modeling the effects of rotation (and macroturbulence). S may be
;          given in any units.
;       VSTEP: (scalar) velocity spacing of spectrum points in S. Because
;          the algorithm involves a convolution, the velocity spacing must
;          be uniform (or at least very close). Must have the same units as
;          VSINI.
;       VSINI: (scalar) the maximum radial velocity shift due to rotation,
;          i.e. the equatorial rotation velocity times the sine of the
;          inclination angle. Must have the same units as VSTEP.
;       EPS (scalar or vector) coefficient(s) specifying the limb-darkening
;          law. The intensity (usually in the continuum) is assumed to
;          follow the relation:
;
;             I(mu)
;            ------- = 1-total(eps) + eps(0)*mu + eps(1)*mu^2 + ....
;             I(0)
;
;          where I(mu) is the intensity at an arbitrary mu value, I(0) is
;          the intensity at disk center, and mu is the cosine of the angle
;          between the line-of-sight and the local normal to the stellar
;          surface. EPS is dimensionless. The routine does not currently
;          support limb-darkening laws with quartic or higher terms, so
;          EPS can have at most three elements.
;
;OUTPUTS:
;       Function Value: (vector) rotationally broadened flux spectrum.
;          The units will be the same as the input spectrum S.
;
;MODIFICATION HISTORY:
;       21-Dec-95 Jeff Valenti  Adapted from rotbro.pro. Generalized the
;          limb-darkening law to higher powers of mu.
;-

if n_params() lt 4 then begin
  print,'syntax: sb = rotb(s, vstep, vsini, eps)'
  retall
endif

;Check for negative VSINI (can happen when solving for VSINI).
  if vsini lt 0.0 then begin
    print, 'rotb: treating negative vsini as zero (no broadening).'
  endif

;Check whether any broadening is needed.
  if vsini le 0.0 then begin
    return, s
  endif

;Check that we can support the requested limb-darkening law.
  nmax = 3					;maximum elements in EPS
  neps = n_elements(eps)
  if neps gt nmax then begin
    print, 'rotb: too many elements in limb-darkening specification.'
    stop
  endif

;Pad epsilon with zeros, if the full cubic expression was not specified.
  e = replicate(0.0, nmax)
  e(0:neps-1) = eps

;Build velocity scale (as a fraction of VSINI) used to construct kernel.
  nhalf = long(float(vsini) / vstep)		;points in half kernel
  nk = 2*nhalf + 1				;point in full kernel
  delta = vstep * (findgen(nk) - nhalf) / vsini	;fractional velocities

;Generate coefficients for rotational broadening kernel.
  alpha = 2 / !pi / (1.0 - e(0)/3.0 - 0.5*e(1) - 0.6*e(2))
  c = alpha * [ $
      total(1.0 - total(e)) $
    , !pi * e(0) / 4.0 $
    , 2.0 * e(1) / 3.0 $
    , 3.0 * !pi * e(2) / 16.0 ]
 
;Build rotational broadening kernel. The funny grouping of terms is to
;   minimize the impact of inaccuracies in the sqrt() function.
  d = 1.0 - delta^2
  kern = sqrt(d) * ( c(0) + d*c(2) ) $
       + d       * ( c(1) + d*c(3) )
  kern = kern * vstep / vsini			;normalize area
  kern = kern / total(kern)			;shouldn't be needed, but...

;Convolve spectrum with kernel.
  return, convol(s, kern, /edge_truncate)	;do convolution

end
