pro bgcheck, obsid, icol, data=data, back=back
;Check the background determination.
;Input:
; obsid (string) root naem of the observation of interest.
; icol (scalar) number of column to check.
;Output:
; Screen plot.
;Keywords:
; data - the data values from the requested image column
; back - the linearly interpolated background
;History:
; 1998-Dec-01 Valenti  Wrote.
; 1998-Dec-01 CMJ Added output keywords
; 1998-Dec-07 JAV Added scaled order density (orders/pixel) to plot to test
;                  whether overlap is bringing up background. It looks like
;                  the humps in the background are not due to order overlap.

if n_params() lt 2 then begin
  print, 'syntax: bgcheck, obsid, icol, data=data, back=back'
  retall
endif

;Read data.
  rdsk, orc, obsid+'.ord'
  rdsk, bkg, obsid+'.bkg'
  im = readfits(obsid+'.fits', head, /silent)

;Extract desired column.
  col = reform(im(icol,*))

;If the image has more than 2048 columns, assume we have to subtract bias.
  sz = size(im)
  ncol = sz(1)					;number of columns
  nrow = sz(2)					;number of rows
  biasvec = fltarr(nrow)			;init bias vector
  if ncol gt 2048 then begin
    for irow=0, nrow-1 do begin
      biasvec(irow) = median(im(2048:*,irow))	;use median of bias region
    endfor
    biasvec(0) = biasvec(1)			;first point seems bogus
    medbias = median(biasvec)			;scalar bias estimate
    x = dindgen(nrow) / nrow			;normalize for accuracy
    y = biasvec - medbias			;normalize for accuracy
    biascoef = poly_fit(x, y, 4, biasfit, /double)
    biasfit = biasfit + medbias
    col = col - biasfit				;subtract bias fit
;   col = col - medbias				;this is what package does
  endif

;Calculate row numbers for each order at specified column.
  sz = size(orc)
  ncoef = sz(1)				;number of orc coefficients
  nord = sz(2)				;number of orders in spectrum
  dcol = double(icol)			;double precision equivalent
  row = reform(orc(0,*))		;constant term
  for i=1, ncoef-1 do begin		;loop thru remaining terms
    row = row + reform(orc(i,*))*dcol^i	;add next term
  endfor

;Calculate the order density
  drow = row(1:nord-2) - row(0:nord-2)
  oden = 1.0 / drow

;Calculate background beneath each order at specified column.
  sz = size(bkg)
  ncoef = sz(1)				;number of orc coefficients
  bg = reform(bkg(0,*))			;constant term
  for i=1, ncoef-1 do begin		;loop thru remaining terms
    bg = bg + reform(bkg(i,*))*dcol^i	;add next term
  endfor

;Determine good plot limit.
  ncol = n_elements(col)
  nbin = fix(round(nord / 4.0))
  binsiz = float(ncol) / nbin
  bgmax = -1e5
  for ibin=0, nbin-1 do begin
    i0 = fix(ibin*binsiz)
    i1 = fix((ibin+1)*binsiz-1)
    bin = col(i0:i1)
    bgmax = bgmax > abs(min(bin))
  endfor 

;Make plot
  plot, col, max=3*bgmax, xsty=3, ps=10 $
      , xtit='Row Number at Column ' + strtrim(icol, 2) $
      , ytit='Spectrum and Background (ADU)' $
      , tit=obsid $
      , chars=1.3
  oplot, row, bg, th=2, co=2
  oplot, col / max(col) * max(bg), co=3
  oplot, row, 0.75 * oden / max(oden) * max(bg), ps=1, co=4

;Load return variables if requested
  data = col
  back = interpol(bg,row,findgen(nrow))

end
