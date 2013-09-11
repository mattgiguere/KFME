pro stis_ripple, wave, ripple, opt_elem, cenwave, sporder, out=out
;Extract echelle ripple from SC2D reference file.
;
;Inputs:
; opt_elem (string) optical element ('e140m', 'e140h', 'e230m', 'e230h')
; cenwave (integer) official central wavelength of setting
; sporder (integer) echelle order number
; [out=] (string) name of optional output file
;
;Outputs:
; wave (vector(500)) vector of wavelengths for ripple function
; ripple (vector(500)) ripple function for requested mode and order
;
;History:
; 2000-Oct-20 Valenti  Initial coding.

if n_params() lt 5 then begin
  print,'syntax: stis_ripple, wave, ripple, opt_elem, cenwave, sporder [,out=]'
  return
endif

;Internal parameters.
  oref = '/data/garnet3/oref/'			;reference file directory

;Put optical element in canonical form.
  oe = strlowcase(strtrim(opt_elem, 2))

;Get name of SC2D reference file containing desired ripple function.
  case oe of
    'e140m' : rfile = 'k8m09588o_rip.fits'
    'e140h' : rfile = 'k8m09588o_rip.fits'
    'e230m' : rfile = 'k8m0958fo_rip.fits'
    'e230h' : rfile = 'k8m0958fo_rip.fits'
    else: begin
      message, /info, 'unknown optical element: ' + opt_elem
      message, /info, "allowed values are: 'e140m', 'e140h', 'e230m', 'e230h'"
      return
      end
  endcase

;Read reference file.
  file = oref + rfile
  rip = mrdfits(file, 1, /silent)
  if not keyword_set(rip) then begin
    message, /info, 'could not read ' + file
    return
  endif

;Make sure requested optical element is in reference data.
  ioe = where(strlowcase(strtrim(rip.opt_elem, 2)) eq oe, noe)
  if noe eq 0 then begin
    message, /info, 'data for ' + oe + ' not in ' + rfile
    return
  endif
  rip = rip(ioe)				;trim to requested opt_elem

;Look for requested central wavelength.
  icw = where(rip.cenwave eq cenwave, ncw)
  if ncw eq 0 then begin
    message, /info, 'invalid requested cenwave: ' + strtrim(cenwave, 2)
    message, /info, 'allowed central wavelengths for ' + opt_elem + ' are:'
    valid = rip.cenwave
    valid = valid(uniq(valid, sort(valid)))
    print, form='(12i6)', valid
    return
  endif
  rip = rip(icw)				;trim to requested cenwave

;Look for requested echelle order.
  ior = where(rip.sporder eq sporder, nor)
  if nor eq 0 then begin
    message, /info, 'invalid echelle order: ' + strtrim(sporder, 2)
    message, /info, 'allowed spectral orders for ' + opt_elem + ' are:'
    print, form='(14i5)', rip.sporder
    return
  endif

;Extract requested ripple function.
  wave = rip(ior).wavelength			;wavelengths
  ripple = rip(ior).ripple			;ripple function

;If requested, write optional output file.
  if keyword_set(out) then begin
    openw, unit, out, /get_lun
    printf, unit, ' Vac_Wave    Ripple'
    for i=0, n_elements(wave)-1 do begin	;loop thru ripple points
      printf, unit, form='(f9.4,f7.3)' $
            , wave(i), ripple(i)
    endfor
    free_lun, unit
  endif

end
