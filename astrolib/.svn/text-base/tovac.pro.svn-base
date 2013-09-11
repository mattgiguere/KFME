function tovac, airwav
;Converts air wavelength(s) in Angstroms to vacuum wavelength equivalent.
; airwav (input scalar or vector) air wavelengths (in Angstroms) to convert.
; tovac() (output scalar or vector) vacuum wavelength (in Angstroms) equivalent.
;Air wavelengths below 1999.3529 Angstroms are not altered. This limit is
; equivalent to the 2000 Angstrom vacuum wavelength in toair().
;IAU conversion standard (Morton 1991, ApJS, 77, 119).
;1991      Landsman  Create.
;19-Feb-95 Valenti   Adapt from airtovac procedure.  New function does not
;                     alter input argument.  Maintains input precision, though
;                     as in original version, constants are single precision.

on_error, 2
if n_params() lt 1 then begin
   print,'syntax: vacwav = tovac(airwav)'
   retall
endif

;Wavelength not below 2000 Angstroms. Compute ratio of vacuum/air wavelengths.
  wsq   = (1e4 / airwav) ^ 2			;wavenumber(??) squared?
  ratio = 1.0 + 6.4328e-5 $
              + 2.94981e-2 / (146.0 - wsq) $
              + 2.5540e-4  / ( 41.0 - wsq)

;Set correction factor to unity (no correction) below 2000 Angstroms.
  ratio = ratio * (airwav ge 1999.3529) + (airwav lt 1999.3529)

;Convert and return vacuum wavelengths.
  return, airwav * ratio

end                        
