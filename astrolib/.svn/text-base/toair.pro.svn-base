function toair, vacwav
;Converts vacuum wavelength(s) in Angstroms to air wavelength equivalent.
; vacwav (input scalar or vector) vacuum wavelengths (in Angstroms) to convert.
; toair() (output scalar or vector) air wavelength (in Angstroms) equivalent.
;Vacuum wavelengths below 2000 Angstroms are not altered.
;Claimed accuracy is 0.005 Angstroms (see IUE Image Processing Manual, p. 6).
;1982      Lindler   Create.
;1989      Landsman  Document.
;19-Feb-95 Valenti   Adapt from vactoair procedure.  New function does not
;                     alter input argument.  Maintains input precision, though
;                     as in original version, constants are single precision.

on_error, 2
if n_params() lt 1 then begin
   print,'syntax: airwav = toair(vacwav)'
   retall
endif

;Wavelength not below 2000 Angstroms. Compute ratio of vacuum/air wavelengths.
  ratio = 1.0 + 2.735182e-4 $
              + (11.46377774e0 / vacwav) ^ 2 $
              + (128.9214492e0 / vacwav) ^ 4

;Set correction factor to unity (no correction) below 2000 Angstroms.
  ratio = ratio * (vacwav ge 2000) + (vacwav lt 2000)

;Convert and return air wavelengths.
  return, vacwav / ratio

end                        
