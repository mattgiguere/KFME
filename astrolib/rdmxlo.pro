pro rdmxlo, infile, mxlo
;Reads IUE low dispersion spectrum from IUE final archive FITS file.
; infile (input string) filename root (.mxlo extension is assumed)
; mxlo (output structure) structure containing data from Final Archive file.
;Edit History:
;06-Dec-96 Valenti  Wrote.

if n_params() lt 2 then begin
  print, 'rdmxlo, file, mxlo'
  retall
endif

;Append ".mxlo" extension to file name, if needed.
  file = infile					;make local variable
  if strpos(file, '.mxlo') lt 0 then begin
    file = file + '.mxlo'			;add extension, if needed
  endif

;Read the primary FITS header.
  head = headfits(file)

;Make sure this is an IUE Final Archive file.
  if not keyword_set(sxpar(head, 'TELESCOP')) then begin
    message, /info, file + ' is not an IUE Final Archive file.'
    return
  endif

;Read header from first FITS extension.
  fxbopen, unit, file, 1, xhead			;open disk file, get header
  nspec = fxpar(xhead, 'NAXIS2')		;number of apertures
  types = strtrim(fxpar(xhead, 'TTYPE*'), 2)	;column types
  units = strtrim(fxpar(xhead, 'TUNIT*'), 2)	;units for data in columns
  ncols = n_elements(types)			;number of data columns

;Add wavelength to the units.
  iwhr = where(types eq 'WAVELENGTH', nwhr)	;look for wavelengths
  if nwhr eq 0 then begin
    message, /info, 'No starting wavelength in ' + file
    return
  endif
  units = [units, units(iwhr)]			;wavelength

;Start building output structure.
  mxlo = { $
    file:  file,  $
    head:  head,  $
    xhead: xhead, $
    nspec: nspec, $
    units: units  }

;Loop through columns, reading data.
  irow = [1, nspec]				;range of rows
  for icol=1, ncols do begin			;loop thru columns
    typ = types(icol-1)				;current column type
    fxbread, unit, data, icol, irow		;get data
    mxlo = create_struct(mxlo, typ, data)	;insert data
  endfor

;Close data file.
  fxbclose, unit				;close disk file

;Build wavelength scale.
  if nspec eq 1 then begin			;true: 1 spectrum
    wave = mxlo.wavelength $
         + mxlo.deltaw * dindgen(mxlo.npoints)	;build wavelength scale
  endif else begin				;else: multiple spectra
    wave = mxlo.flux				;initial array
    npts = max(mxlo.npoints)			;maximum number of points
    for ispec=0, nspec-1 do begin		;loop thru spectra
      wave(*,ispec) = mxlo.wavelength(ispec) $
         + mxlo.deltaw(ispec) * dindgen(npts)	;build wavelength scales
    endfor
  endelse

;Insert wavelength scale(s) into structure.
  mxlo = create_struct(mxlo, 'WAVE', wave)	;insert wavelength scale(s)

end
