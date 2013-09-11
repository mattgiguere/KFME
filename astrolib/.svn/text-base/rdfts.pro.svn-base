pro rdfts, file, w, s, xr=xr
;Read NSO FTS spectrum.

if n_params() lt 3 then begin
  print, 'syntax: rdfts, file, w, s [,xr=]'
  return
endif

  s = readfits(file, head)
  n = n_elements(s)
  w0 = sxpar(head, 'CRVAL1')
  disp = sxpar(head, 'CDELT1')
  w = w0 + disp*dindgen(n)

  if keyword_set(xr) then begin
    itrim = where(w ge xr(0) and w le xr(1), ntrim)
    if ntrim eq 0 then begin
      message, /info, 'xr= constraint leaves no data - ignoring'
    endif else begin
      w = w(itrim)
      s = s(itrim)
      n = ntrim
    endelse
  endif

end

