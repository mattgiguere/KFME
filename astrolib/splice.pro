pro splice, w1, f1, e1, w2, f2, e2, w, f, e, ramp=ramp
;Splice together two overlapping GHRS spectra.
;Inputs:
; w1 (vector) wavelengths for first segment.
; f1 (vector) fluxes for first segment.
; e1 (vector) uncertainties for first segment.
; w2 (vector) wavelengths for second segment.
; f2 (vector) fluxes for second segment.
; e2 (vector) uncertainties for second segment.
; /ramp (switch) ramps fluxes, rather than averaging.
;Outputs:
; w (vectors) spliced wavelengths
; f (vectors) spliced fluxes
; e (vectors) spliced uncertainties
;History:
; 19-Feb-99 Valenti  Added error propagation for e2 in overlap region.

if n_params() lt 9 then begin
  print, 'syntax: splice, w1, f1, e1, w2, f2, e2, w, f, e [,/ramp]'
  retall
endif

;Determine wavelength limits.
  n1 = n_elements(w1)
  n2 = n_elements(w2)
  w1min = min(w1, max=w1max)
  w2min = min(w2, max=w2max)
  dw1 = w1(1:n1-1) - w1(0:n1-2)
  dw2 = w2(1:n2-1) - w2(0:n2-2)

;Check wavelength ordering.
  if w2min lt w1min then begin
    message, /info, "w1 must begin at shorter wavelengths than w2."
  endif

;Make sure we have overlap.
  if w1max lt w2min then begin
    message, /info, "no overlap - concatenating."
    w = [w1, w2]
    f = [f1, f2]
    e = [e1, e2]
    return
  endif

;Find overlap region.
  iol1 = where(w1 ge w2min, nol1)
  iol2 = where(w2 le w1max, nol2)

;Make new wavelength scale.
  i2 = where(w2 gt w1max, n2)
  if n2 gt 0 then begin
    w = [ w1, w2(i2) ] 
  endif else  begin
    w = w1
  endelse
  n = n_elements(w)

;Load flux vector and uncertainties, but overlap region still wrong.
  f = fltarr(n)
  f(0:n1-1) = f1
  if n2 gt 0 then f(n1:n-1) = f2(i2)
  e = fltarr(n)
  e(0:n1-1) = e1
  if n2 gt 0 then e(n1:n-1) = e2(i2)

;Linearly interpolate onto wavelength scale of bluer order in overlap region.
  wol21 = w1(iol1)
  fol21 = interpol(f2, w2, wol21)
  eol21 = fltarr(nol1)
  xlo = w2(0:nol2-1)
  if nol2 lt n2 then begin
    xhi = w2(1:nol2)
  endif else begin
    xhi = [w2(1:nol2-1), 2*w2(nol2-1) - w2(nol2-2)]
  endelse
  elo = e2(0:nol2-1)
  if nol2 lt n2 then begin
    ehi = e2(1:nol2)
  endif else begin
    ehi = [e2(1:nol2-1), 2*e2(nol2-1) - e2(nol2-2)]
  endelse
  for i=0L, nol1-1 do begin
    iwhr = (where(wol21(i) ge xlo and wol21(i) lt xhi, nwhr))(0)
    if nwhr eq 0 then message, 'outside interpolation constraints'
    if nwhr eq 2 then message, 'multiple intervals'
    frac = (wol21(i) - xlo(iwhr)) / (xhi(iwhr) - xlo(iwhr))
    el = elo(iwhr)
    eh = ehi(iwhr)
    if el eq 0 and eh eq 0 then begin
      eol21(i) = 0
    endif else begin
      if el eq 0 then frac = 1
      if eh eq 0 then frac = 0
      eol21(i) = sqrt(((1-frac)*elo(iwhr))^2 + (frac*ehi(iwhr))^2)
    endelse
  endfor

;Fix overlap region.
  if keyword_set(ramp) then begin
    wt = findgen(nol1) / (nol1 - 1)
    fol = ((1.0 - wt) * f1(iol1)) + (wt * fol21)
    eol = sqrt( ((1.0 - wt) * e1(iol1))^2 + (wt * eol21)^2 )
  endif else begin
    inz = where(e1(iol1) ne 0, nnz)
    wt1 = dblarr(nol1)
    if nnz gt 0 then wt1(inz) = 1.0 / e1(iol1(inz))^2
    inz = where(eol21 ne 0, nnz)
    wt2 = dblarr(nol1)
    if nnz gt 0 then wt2(inz) = 1.0 / eol21(inz)^2
    inz = where(wt1+wt2 ne 0, nnz)
    fol = dblarr(nol1)
    eol = dblarr(nol1)
    if nnz gt 0 then begin
      fol(inz) = (f1(iol1(inz))*wt1(inz) + fol21(inz)*wt2(inz)) $
               / (wt1(inz) + wt2(inz))
      eol(inz) = 1.0 / sqrt(wt1(inz) + wt2(inz))
    endif

  endelse
  f(iol1) = fol
  e(iol1) = eol

if min(finite(f)) eq 0 then stop

end
