Received: from casa.Colorado.EDU (root@casa.Colorado.EDU [128.138.141.19]) by pandora.Colorado.EDU (8.6.12/8.6.12/CASA) with SMTP id QAA06953 for <jvalenti@pandora.Colorado.EDU>; Sun, 24 Sep 1995 16:26:10 -0600
Received: from nak.Berkeley.EDU by casa.Colorado.EDU with SMTP id AA18809
  (5.65a+/IDA-1.5 for jvalenti@pandora.Colorado.EDU); Sun, 24 Sep 95 16:26:09 -0600
Received: from etoile.Berkeley.EDU (etoile.Berkeley.EDU [128.32.92.247]) by nak.berkeley.edu (8.6.10/8.6.10) with SMTP id PAA09739 for <jvalenti@casa.colorado.edu>; Sun, 24 Sep 1995 15:26:08 -0700
Received: by etoile.Berkeley.EDU (5.0/SMI-SVR4)
	id AA15396; Sun, 24 Sep 1995 15:25:28 -0700
Date: Sun, 24 Sep 1995 15:25:28 -0700
From: gmarcy@etoile.berkeley.edu (geoff marcy 417)
Message-Id: <9509242225.AA15396@etoile.Berkeley.EDU>
To: jvalenti@casa.colorado.edu
Subject: PSF Description
X-Sun-Charset: US-ASCII
Content-Length: 4098
Status: RO

Hi Jeff,

I'm still ga-ga over your Zeeman detections.

A quick note on PSF descriptions, as I haven't
filled you in for a while. 

Paul and I have settled on the definitive PSF description.
It's a two-sided Voigt function (each half getting independent
doppler and damping widths)  added to 6 little Gaussians having
fixed positions and widths, but variable heights.  This description
is the sum of the best two descriptions I know of.  Orthogonality
aside, this parametrization carries both elegant simplicity (DBL Voigt)
with significant flexibility if need be (Gaussians).

Our values for Chi-sq in our Doppler project are consistently
between 1.0 and 1.5 , with known improvements yet to come.

Here is the PSF code:  VOIGTGP.PRO

Best Regards,
Geoff

---------------------------------------------

function voigtgp,x,par
;  Two-Sided Voigt Profile Plus 6 Gaussians
;
common psfstuff,psfsig,psfpix,obpix
;
;  INPUT
;       X    fltarr     abscissa values in pixel units
;       PAR  fltarr(4)  Doppler Width and Damping Parameter (for left, right)
;                       PAR = [dop_left, damp_left, dop_right, damp_right]
;  OUTPUT
;       PSF fltarr      resulting PSF, normalized to unit area
;
;  Operation:  Computes a double-sided Voigt function, each wing
;              having its own Doppler and Damping width.
;
;  Example:    ip = voigtip( findgen(64)/4.-8, [0.8, 0.1, 1.0, 0.3,0.,0.,0.,0.,0.,0.] )

;Set some Constants
  c1 = 0.56419                          ;Far Wing constants  (Gray p229)
  c2 = 0.846
  c3 = -0.56

if par(0) eq 0. then par(0) = 1.e-10*(x(1)-x(0))
if par(2) eq 0. then par(2) = 1.e-10*(x(1)-x(0))

if n_elements(psfpix) lt 1 then begin
   psfpix=[0.0,0.0,0.0,0.0,-2.25,-1.50,-0.75, 0.75, 1.50, 2.25, 0.0]
   psfsig=[0.0,0.0,0.0,0.0, 1.00, 0.75, 0.50, 0.50, 0.75, 1.00, 0.0]
endif

dum = min(abs(x),cen)                   ;cen is index of min
;Negative wing.
  u = x/par(0)                          ;x in Doppler-Width units
  ineg = where(u le u(cen) and u ge -4., nneg)  ;Left wing pts
  a = par(1)                            ;damping width
  leftpsf = voigt(a,u(ineg))            ;Voigt profile on left
  leftpeak = leftpsf(nneg-1)            ;central value, from left
  ifar = where(u lt -4., nfar)          ;far wing (4 Doppler widths)
  if nfar ge 1 then begin               ;far wing
    u = u(ifar)
    far = a*(c1/u^2 + c2/u^4) + a^3 * c3/u^4   ;far wing (Gray p229)
    leftpsf = [far,leftpsf]         ;tack together
  end

;Positive wing.
  u = x/par(2)                          ;x in Doppler-Width units
  ipos = where(u ge u(cen) and u le 4.)      ;Right wing pts
  a = par(3)                            ;damping width
  rightpsf = voigt(a,u(ipos))           ;Voigt profile on right
  ifar = where(u gt 4., nfar)           ;far wing (4 Doppler widths)
  if nfar ge 1 then begin               ;far wing
    u = u(ifar)
    far = a*(c1/u^2 + c2/u^4) + a^3 * c3/u^4   ;far wing (Gray p229)
    rightpsf = [rightpsf,far]            ;tack together
  end

;Register half-profiles at center
  ratio = rightpsf(0)/leftpeak
  nleft = n_elements(leftpsf)
  psf = [leftpsf(0:nleft-2)*ratio, rightpsf]  ;concatinate [lt, rt]

;Overall normalization.
  n = n_elements(x)
  dx = (x(n-1) - x(0))/(n-1.)             ;Delta pixel per element of x
  psf = psf / (dx * total(psf))

;Gaussian buddies
; Add in little buddy gaussians:  Fixed centers and widths
  if n_elements(psfpix) gt 0 and max(abs(par(4:10))) gt 0 then $
     for n=4,10 do begin                                    ;harwired parameter!
       cen = psfpix(n) & wid=psfsig(n)                      ;gaussian pos'n, wi!
;       print,cen,wid,par(n)
       if wid gt 0 and par(n) ne 0 then begin               ;if wid = 0, toss g!
          ind = where(x ge cen-4.*wid and x le cen+4*wid)   ;range of guassian
          psf(ind) = psf(ind) + par(n)*exp(-0.5*((x(ind)-cen)/wid)^2)
       endif
     endfor
;End Little Gaussian Addition
;Overall normalization.
  n = n_elements(x)
  dx = (x(n-1) - x(0))/(n-1.)             ;Delta pixel per element of x
  psf = psf / (dx * total(psf))
  return,psf
end



