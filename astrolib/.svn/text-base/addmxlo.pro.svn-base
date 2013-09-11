pro addmxlo, filelist, w, f, sigmaf
;Reads IUE low dispersion spectrum from IUE final archive FITS file.
; filelist (input string) list of files (.mxlo extension is assumed)
; w (output vector) wavelength (in Angstroms) scale for spectrum in s
; f (output vector) IUE low dispersion, final archive spectrum 
; sigmaf (output vector) uncertainty in s
;Edit History:
;18-Jan-96 Valenti  Adapted from rdmxlo.pro.

if n_params() lt 3 then begin
  print, 'addmxlo, filelist, w, f [,sigmaf]'
  retall
endif

  flist = filelist
  if strpos(flist, '.mxlo') lt 0 then $
    flist = flist + '.mxlo'			;append extension
  flist = findfile(flist)			;expand wildcards
  nfile = n_elements(flist)

  for i=0, nfile-1 do begin
    rdmxlo, flist(i), x, y, z
    if i eq 0 then begin
      w = x
      sumy = y / z^2
      sumz = 1.0 / z^2
    endif else begin
      sumy = sumy + y / z^2
      sumz = sumz + 1.0 / z^2
    endelse
  endfor

  f = sumy / sumz
  sigmaf = sqrt(1.0 / sumz)

  iwhr = where(f gt 0)
  w = w(iwhr)
  f = f(iwhr)
  sigmaf = sigmaf(iwhr)

end
