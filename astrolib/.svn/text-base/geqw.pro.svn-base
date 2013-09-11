pro geqw,w,s,result,wlo,whi
;Determine equivalent width in interactively specified spectral line.
; W (input vector(# points)) wavelength scale
; S (input vector(# points)) spectrum
; [Result (optional output structure)] various line parameters returned
;   by routine. See code and comments at end of routine for details.
; [Wlo (optional input scalar)] lowest wavelength in line selection plot;
;   defaults to lowest value in w.
; [Whi (optional input scalar)] highest wavelength in line selection plot;
;   defaults to highest value in w.
;05-Mar-91 JAV	Create.
;28-May-92 JAV	Translated from ANA.
;01-Jun-92 JAV	Fixed flux normalization bug. Use 1st, not 2nd order polynomial
;		  under gaussian.

if n_params() lt 2 then begin
  print,'syntax: geqw,w,s [,result [,wlo(0=default),whi(0=default)]]'
  retall
endif
if n_params() lt 4 then wlo = 0			;plot down to minimum W
if n_params() lt 5 then whi = 0			;plot up to maximum W

;Make line selection plot.
  delay = 0.4					;mouse button delay (sec)
  tit = 'Indicate left and right edge of line region with mouse.'
  plot,w,s,xrange=[wlo,whi],xstyle=1,tit=tit,/yno

;Get plot window to use for Weq determination.
  cursor,cx1,cy1,wait=3				;get one edge of window
  wait,delay
  cursor,cx2,cy2,wait=3				;get one edge of window
  wait,delay
  xb = cx1 < cx2				;bottom edge of window
  xt = cx1 > cx2				;top edge of window
; message,/info,'Weq determination window: [' $
;   + strtrim(string(xb,form='(f9.2)'),2) + ', ' $
;   + strtrim(string(xt,form='(f9.2)'),2) + '].'

;Plot Weq determination window.
  iwhr = where(w ge xb and w le xt)		;cut out windowed segment
  x = w(iwhr)
  y = s(iwhr)
  plot,x,y,ps=2,syms=0.85,xrange=[xb,xt] $
    ,xstyle=1,/yno				;make plot
  tit = 'Indicate left and right edge of blue baseline region with mouse.'
  !p.color=4
  xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5

;Get blue baseline window.
  cursor,cx1,cy1,wait=3				;get one edge of blue baseline
  wait,delay
  if cy1 lt !y.crange(0) or $
     cy1 gt !y.crange(1) or $
     cx1 lt !x.crange(0) or $
     cx1 gt !x.crange(1) then begin		;true: point outside plot
    message,/info,'No blue baseline.'		;report no blue baseline
    blo = 0					;flag no blue baseline
    bhi = -1					;flag no blue baseline
  endif else begin				;fall thru: blue baseline
    cursor,cx2,cy2,wait=3			;get one edge of blue baseline
    wait,delay
    blo = cx1 < cx2				;blue edge, blue baseline
    bhi = cx1 > cx2				;red edge, blue baseline
;   message,/info,'Blue baseline: [' $
;   + strtrim(string(blo,form='(f9.2)'),2) + ', ' $
;   + strtrim(string(bhi,form='(f9.2)'),2) + '].'
  endelse
    
;Get red baseline window.
  !p.color=0
  xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
  tit = 'Indicate left and right edge of red baseline region with mouse.'
  !p.color=2
  xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
  !p.color=1
  cursor,cx1,cy1,wait=3				;get one edge of red baseline
  wait,delay
  if cy1 lt !y.crange(0) or $
     cy1 gt !y.crange(1) or $
     cx1 lt !x.crange(0) or $
     cx1 gt !x.crange(1) then begin		;true: point outside plot
    message,/info,'No red baseline.'		;report no red baseline
    rlo = 0					;flag no red baseline
    rhi = -1					;flag no red baseline
  endif else begin				;fall thru: red baseline
    cursor,cx2,cy2,wait=3			;get one edge of red baseline
    wait,delay
    rlo = cx1 < cx2				;blue edge, red baseline
    rhi = cx1 > cx2				;red edge, red baseline
;   message,/info,'Red baseline: [' $
;   + strtrim(string(rlo,form='(f9.2)'),2) + ', ' $
;   + strtrim(string(rhi,form='(f9.2)'),2) + '].'
  endelse
    
;Extract baseline if specified.
  ibase = where((x ge blo and x le bhi) $
    or (x ge rlo and x le rhi),nbase)		;extract baseline indicies 
  if nbase gt 0 then begin			;true: baseline points exist
    xbase = x(ibase)				;extract blue base wavelengths
    ybase = y(ibase)				;extract blue base spectrum
  endif else begin				;else: no baseline - get level
    !p.color=0
    xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
    tit = 'Indicate constant baseline level with mouse.'
    !p.color=5
    xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
    !p.color=1
    cursor,cx1,cy1,wait=3			;get baseline level
    wait,delay
    xbase = cx1					;dummy wavelength
    ybase = cy1					;constant baseline value
  endelse
    
;Get spectral line window.
  !p.color=0
  xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
  tit = 'Indicate left and right edge of spectral line region with mouse.'
  !p.color=6
  xyouts,0.5,0.96,align=0.5,/normal,tit,chars=1.5
  !p.color=1
  cursor,cx1,cy1,wait=3				;get one edge of line region
  wait,delay
  if cy1 lt !y.crange(0) or $
     cy1 gt !y.crange(1) or $
     cx1 lt !x.crange(0) or $
     cx1 gt !x.crange(1) then begin		;true: point outside plot
    message,/info,'Using all points in and bewteen baselines.'
    llo = blo					;red edge line abuts blue base
    lhi = rhi					;blue edge line abuts red base
  endif else begin				;fall thru: get other line edge
    cursor,cx2,cy2,wait=3			;get other edge of line region
    wait,delay
    llo = cx1 < cx2
    lhi = cx1 > cx2
;   message,/info,'Spectral line region: [' $
;     + strtrim(string(llo,form='(f9.2)'),2) + ', ' $
;     + strtrim(string(lhi,form='(f9.2)'),2) + '].'
  endelse

;Fit baseline if specified.
  if blo lt bhi and rlo lt rhi then begin	;true: red and blue baselines
    bcoef = poly_fit(xbase,ybase,1)		;ceofficients of baseline fit
    bfit = poly(x,bcoef)			;baseline under entire spectrum
  endif else begin				;fall thru: 0 or 1 baseline
    if n_elements(xbase) ge 10 then begin	;true: long half baseline
      bcoef = poly_fit(xbase,ybase,1)		;coefficients of baseline fit
      bfit = poly(x,bcoef)			;baseline under entire spectrum
    endif else bfit = 0.0*x $
      + total(ybase) / n_elements(ybase)	;use constant baseline
  endelse

;Divide by baseline fit and replot spectral and baseline points only.
  iwhr = where((x ge blo and x le bhi) $
	    or (x ge llo and x le lhi) $
	    or (x ge rlo and x le rhi), nwhr)
  u = x(iwhr)					;extract valid analysis points
  bv = bfit(iwhr)				;baseline at analysis points
  v = y(iwhr) / bv				;extract residual intensity
print,'Amp =' + strcompress(max(y(iwhr)))
  vnorm = total(bfit(iwhr) /nwhr)
  ub = min(u)
  ut = max(u)
  plot,u,v,ps=2,xrange=[ub,ut],xstyle=1,/yno
  oplot,u,0.0*u+1.0,line=2			;overplot baseline

;Extract data to be used in gaussian fit.
  ilin = where(u ge llo and u le lhi,nk)	;extract line segment
  k = u(ilin)					;spectral line waves only
  l = v(ilin)					;spectral line only
  disp = abs(k(nk-1) - k(0)) / (nk-1)		;wavelength/pixel dispersion

;Compute best fit gaussian equivalent width.
  mnu = total(u) / n_elements(u)
  dummy = gfit(u-mnu,v,par)			;fit gaussian
  amp = par(0)					;amplitude
  cen = par(1)+mnu				;central wavelength
  wid = sqrt(2) * par(2)			;1 sigma width
  geqw = sqrt(!pi) * amp * wid			;Area under Gaussian
  message,/info,'Weq under best fitting gaussian (Angstroms): ' $
    + strtrim(string(geqw,form='(f8.3)'),2)
  t = ub + (ut - ub) * findgen(201) / 200.0	;grid in plot window
  base = poly(t-mnu,par(3:4))			;baseline vector
  g = amp* exp(-(t-cen)^2/(wid^2)) + base	;compute extended gaussian
  oplot,t,g					;overplot gaussian fit

;Compute line flux and full width at half maximum intesity (FWHM).
  iwhr = where(t ge llo and t le lhi, nwhr)	;line region
  mnb = total(base(iwhr)*bv) / nwhr		;mean base under line
  gflux = geqw * mnb				;flux in gaussian line fit
  fwhm = 1.6651 * wid				;1.6651 = sqrt(log(16))

;Compute integral of interpolated line profile between 3 sigma limits of
;  gaussian fit above.
  k = (cen-2.12132*wid) + (4.24264*wid) * findgen(101)/100.0
  l = spline(u,v,k)				;cubic spline interpolation
  nk = n_elements(k)				;number of line points
  disp = abs(k(nk-1) - k(0)) / (nk-1)		;wavelength/pixel dispersion
  ieqw = total(l-1) * disp			;integrate spectrum-baseline
  message,/info,'Integrated Weq (Angstroms): ' $
    + strtrim(string(ieqw,form='(f8.3)'),2)

;Load return vector
  result = {w:0.0,fw:0.0,bg:0.0,flx:0.0,gew:0.0,iew:0.0}
  result.w = cen
  result.fw = fwhm
  result.bg = mnb
  result.flx = gflux
  result.gew = geqw
  result.iew = ieqw

end
