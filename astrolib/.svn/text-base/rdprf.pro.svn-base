pro rdprf, file, w, s, eps
;Reads output of synth2.
;Inputs:
; file (string) name of .prf file created by synth2.
;Outputs:
; w (vector) irregularly spaced grid of wavelengths.
; s (vector) integrated fluxes from entire star at wavelengths w.
; eps (scalar) limb darkening coefficient returned by synth2.

if n_params() lt 3 then begin
  print, 'syntax: rdprf, file, w, s [,eps]'
  retall
endif

;Read number of spectral lines.
  openr, unit, file, /get
  readf, unit, n, form='(i5)'

;Read and flush spectral line information.
  buff = ''
  for i=1,n do readf, unit, buff

;Read number of spectrum points.
  readf, unit, n, eps

;Read spectrum.
  array = dblarr(2, n)
  readf, unit, array
  w = reform(array(0, *))
  s = reform(array(1, *))

;Close file.
  free_lun, unit

end
