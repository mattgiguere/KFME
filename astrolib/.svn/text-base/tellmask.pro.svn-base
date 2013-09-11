pro tellmask,w,mask,res,ston
;Make a telluric line mask for the specified wavelength scale.
; w (input vector or array) wavelength scale (in Angstroms). Only lines with
;   equivalent widths greater than or equal to 10 mA between 4930 and 8750
;   Angstroms are included. Data from Moore, Minnaert, and Houtgast (1961).
; mask (output with same size as w) 0 for points contaminated by telluric
;   lines, 1 for uncontaminated points.
; res (input scalar) spectral resolution. Default is 45,000 (appropriate for
;   the Hamilton). Used with ston to determine range of wavelengths that are
;   contaminated by each telluric line.
; ston (input scalar) signal to noise ratio. Default is 100. Used with res to
;   determine range of wavelengths contaminated by each telluric line.
;20-Jun-94 JAV	Create. Needs to be tested at low ston and low resolution.
;14-Oct-94 JAV	Ported to CASA.

if n_params() lt 2 then begin
  print,'syntax: tellmask,w,mask [,res,ston]'
  retall
endif

;Assign default values to optional parameters, if unspecified.
  if n_elements(res) eq 0 then res = 0
  if res le 0 then res = 45000.0			;true: use default
  if n_elements(ston) eq 0 then ston = 0
  if ston le 0 then ston = 100.0			;true: use default

;Define common block to inhibit rereading data.
  common tell_common,tell_lc,tell_eqw

;Read telluric data from disk.
  if n_elements(tell_lc) le 0 then begin
    file = '/users1/casa/jvalenti/idl/telluric.dsk'	;name of data file
    rdsk,tell_lc,file,1				;line centers (air)
    rdsk,tell_eqw,file,2			;equivalent widths (mA)
  endif

;Get information about wavelength scale.
  sz = size(w)					;variable info block
  ndim = sz(0)					;# dimensions
  ncol = sz(1)					;# of columns
  if ndim eq 2 then nord = sz(2) else nord = 1	;# of orders

;Handle multiple order case.
  mask = replicate(1.0,ncol,nord)		;init mask
  for iord=0,nord-1 do begin			;loop thru orders
    ww = w(*,iord)				;extract order
    minww = min(ww,max=maxww)			;get extreme values
    disp = (maxww - minww) / float(ncol-1)	;pixel dispersion
    iwhr = where(tell_lc gt minww-1 $
	     and tell_lc lt maxww+1,nwhr)	;nearby telluric lines
    if nwhr gt 0 then begin
      lc =  tell_lc(iwhr)			;extract lines centers
      eqw = tell_eqw(iwhr)			;extract equivalent widths
      sig = lc / res / 2.3548			;gaussian 1-sigma
      amp = eqw / sig / sqrt(!pi) / 1000.0 ;gaussian amplitude (res int)
      dw = sig * sqrt(2.0*alog(amp*ston)) $
	 + 0.5 * disp				;offset where gaussian = noise
      dw = 1.5*dw + eqw/500.0			;empirically tuned fudge
      for i=0,nwhr-1 do begin			;loop thru lines
	ibad = where(ww ge lc(i) - dw(i) $
		 and ww le lc(i) + dw(i),nbad)  ;find affected pixels
	if nbad gt 0 then begin			;true: some pixels are affected
	  mask(ibad,iord) = 0.0			;flag pixels aas bad
	endif
      endfor
    endif
  endfor

  if ndim eq 1 then mask = reform(mask,ncol,/overw)

end
