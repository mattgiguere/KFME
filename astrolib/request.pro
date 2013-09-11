pro request,template,wlo,whi,dlim,vmic,teff,logg,long=long
;Make a textfile that can be sent as a VALD "stellar" request.
; template (input string) template for constructing filenames. Should include
;   a "#" which will be replaced by midpoint wavelength of each spectral
;   segment, rounded to the nearest Angstrom. Example: 'mag#.lin'
; wlo (input vector) list of initial wavelengths (in Angstroms) for segments
; whi (input vector) list of final wavelengths (in Angstroms) for segments
;01-Dec-94 JAV	Create.

if n_params() lt 7 then begin
  print,'syntax: request,template,wlo,whi,dlim,vmic,teff,logg [,/long]'
  retall
endif

;Parse filename template.
  smark = '#'					;template substitution mark
  smpos = strpos(template,smark)		;position of substitution mark
  if smpos eq -1 then begin			;true: no substitution mark
    print,'request: filename template has no substitution marker (' $
      + smark + ').'
    retall
  endif
  fbeg = strmid(template,0,smpos)		;leading portion of filename
  fend = strmid(template,smpos+1 $
        ,strlen(template)-smpos-1)		;trailing portion

;Determine wavelngth midpoints for name construction.
  wmid = 0.5*(long(wlo)+whi)			;mean wavelength(s)

  nw = n_elements(wlo)
  for iw=0,nw-1 do begin
    wstr = strtrim(string(long(wmid(iw)+0.5)),2)  ;rounded wavelength
    file = fbeg + wstr + fend			;build filename
    openw,unit,file,/get_lun			;open file
    printf,unit,'begin request'			;write request header
    printf,unit,'extract stellar'		;write request type
    if keyword_set(long) then begin
      printf,unit,'long format'			;write format type
    endif
    printf,unit,strtrim(string(wlo(iw) $
      ,form='(f10.2)'),2) $
      + "," + strtrim(string(whi(iw) $
      ,form='(f10.2)'),2) 			;write wavelength range
    printf,unit,strtrim(string(dlim $
      ,form='(f10.4)'),2) $
      + "," + strtrim(string(vmic $
      ,form='(f10.3)'),2) 			;write limit and vmicro
    printf,unit,strtrim(string(teff $
      ,form='(f10.1)'),2) $
      + "," + strtrim(string(logg $
      ,form='(f10.3)'),2) 			;write Teff and gravity
    printf,unit,'end request'			;write request trailer
    free_lun,unit
  endfor

end
