pro rdxte, infile, time, rate, error
;Reads XTE data file.
; infile (input string) filename root (.mxlo extension is assumed)
; time
;Edit History:
;06-Jun-97 Valenti  Adapted from rdmxlo.

if n_params() lt 2 then begin
  print, 'rdxte, infile, time, rate, error'
  retall
endif

;Append ".mxlo" extension to file name, if needed.
  file = infile					;make local variable
  if strpos(file, '.lc') lt 0 then begin
    file = file + '.lc'			;add extension, if needed
  endif

;Read the primary FITS header.
  head = headfits(file)

;Make sure this is an XTE file (should implement).
; if not keyword_set(sxpar(head, '???????')) then begin
;   message, /info, file + ' is not an XTE file.'
;   return
; endif

;Read header from first FITS extension.
  fxbopen, unit, file, 1, xhead			;open disk file, get header
  nsamp = fxpar(xhead, 'NAXIS2')		;number of apertures
  types = strtrim(fxpar(xhead, 'TTYPE*'), 2)	;column types
  units = strtrim(fxpar(xhead, 'TUNIT*'), 2)	;units for data in columns
  ncols = n_elements(units)			;number of data columns

;Loop through columns, reading data.
  irow = [1, nsamp]				;range of rows
  fxbread, unit, time, 1, irow			;get data
  fxbread, unit, rate, 2, irow			;get data
  fxbread, unit, error, 3, irow			;get data

;Close data file.
  fxbclose, unit				;close disk file

end
