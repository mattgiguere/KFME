function resamph,wold,sold,wnew
;Interpolates OR integrates a spectrum onto a new wavelength scale, depending
;  on whether number of pixels per angstrom increases or decreases. Integration
;  is effectively done analytically under a cubic spline fit to old spectrum. 
; wold (input vector) old wavelngth scale.
; sold (input vector) old spectrum to be binned.
; wnew (input vector) new wavelength spectrum.
; snew (output vector) newly binned spectrum.
;10-Oct-90 JAV	Create.
;22-Sep-91 JAV	Translated from IDL to ANA.
;05-Aug-92 JAV	Changed f/ procedure to function. Renamed f/ rebin to resamp.
;		 Switched f/ intrinsic rebin() to total() - faster.
;15-Mar-95 JAV	Replaced call_external with calls to nr_spl{ine|int}.

if n_params() lt 3 then begin
  print,'syntax: snew=resamph(wold,sold,wnew)'
  retall
endif

;Program flags.
  trace = 0					;(0)1: (don't) print trace info
  dummy = 0L					;init call_external return

;Verify that wold, sold, and wnew are real*8
  error = (0 eq 1)				;set error flag false
  sz = size(wold)				;get info block
  if sz(sz(0)+1) ne 5 then error = (1 eq 1)	;set error flag true
  sz = size(sold)				;get info block
  if sz(sz(0)+1) ne 5 then error = (1 eq 1)	;set error flag true
  sz = size(wnew)				;get info block
  if sz(sz(0)+1) ne 5 then error = (1 eq 1)	;set error flag true
  if error then $
    message,'All arguments must be double precision vectors.'

;Determine spectrum attributes.
  nold = long(n_elements(wold))			;number of old points
  nnew = long(n_elements(wnew))			;number of new points
  psold = (wold(nold-1) - wold(0)) / (nold-1)	;old pixel scale
  psnew = (wnew(nnew-1) - wnew(0)) / (nnew-1)	;new pixel scale

;Verify that new wavelength scale is a subset of old wavelength scale.
  If wnew(0) - 0.5*(wnew(1)-wnew(0)) lt $
     wold(0) - 0.5*(wold(1)-wold(0)) then begin
    if wnew(nnew-1) + 0.5*(wnew(nnew-1)-wnew(nnew-2)) gt $
       wold(nold-1) + 0.5*(wold(nold-1)-wold(nold-2)) then begin
      message,'New wavelength scale not subset of old.'
    end
  end

;Select integration or interpolation depending on change in dispersion.
  if psnew le psold then begin			;pixel scale decreased

;Interpolation by cubic spline.
    if trace then message,/info,'Interpolating onto new wavelength scale.'
;   secder = nr_spline(wold,sold)		;find spline coefficients
;   snew = nr_splint(wold,sold,secder,wnew)	;spline interpolate
    snew = hermite(wold,sold,wnew)
  end else begin				;pixel scale increased

;  Integration under cubic spline.
    if trace then message,/info,'Integrating onto new wavelength scale.'
    xfac = fix(psnew/psold + 0.5)		;pixel scale expansion factor
    if trace then message,/info,'Pixel scale expansion factor: ' $
      + strtrim(string(xfac),2)

;  Construct another wavelength scale (w) with a pixel scale close to that of 
;    the old wavelength scale (wold), but with the additional constraint that
;    every xfac pixels in w will exactly fill a pixel in the new wavelength
;    scale (wnew). Optimized for xfac < nnew.
    dw = 0.5 * (wnew(2:nnew-1) - wnew(0:nnew-3));local pixel scale
    dw = [dw,2*dw(nnew-3) - dw(nnew-4)]		;add trailing endpoint first
    dw = [2*dw(0) - dw(1),dw]			;add leading endpoint last
    w = dblarr(nnew,xfac)			;initialize W as array
    for i=0,xfac-1 do begin			;loop thru subpixels
      w(*,i) = wnew + dw*(double(2*i+1)/(2*xfac) - 0.5)	;pixel centers in W
    endfor
    w = transpose(w)				;transpose W before Merging
    nig = nnew * xfac				;elements in interpolation grid
    w = reform(w,nig,/overwrite)		;make W into 1-dim vector
;  Interpolate old spectrum (sold) onto wavelength scale w to make s. Then
;    sum every xfac pixels in s to make a single pixel in the new spectrum
;    (snew). Equivalent to integrating under cubic spline through sold. 
;   secder = nr_spline(wold,sold)		;find spline coefficients
;   s = nr_splint(wold,sold,secder,w)		;spline interpolate
    s = hermite(wold,sold,w)
    s = s / xfac				;take average in each pixel
    s = reform(s,xfac,nnew,/overwrite)		;initialize sdummy as array
    snew = total(s,1)				;most efficient pixel sum
  end
  return,snew					;return resampled spectrum

end
