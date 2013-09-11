pro bgcheck_all, icol
;Call bgcheck for all obsid implied by *.bkg files in the current directory.
;Input:
; [icol] (scalar) column for all background checks - default is 1000.
;History:
; 1998-Dec-01 Valenti  Wrote.

;Set column, if not specified.
  if n_elements(icol) eq 0 then icol = 1000		;default column

;Get *.bkg files from current directory. Strip extension.
  bkgf = findfile('*.bkg')
  if not keyword_set(bkgf) then message, 'no *.bkg files!'
  nbkg = n_elements(bkgf)

;Convert to obsid by stripping ".bkg" extension.
  obsid = strarr(nbkg)
  for i=0, nbkg-1 do begin
    obsid(i) = strmid(bkgf(i), 0, strpos(bkgf(i),'.bkg'))
  endfor

;Loop through obsid, checking background.
  for i=0, nbkg-1 do begin
    if keyword_set(findfile(obsid(i)+'.ord')) then begin
      haveord = 1
    endif else begin
      print, 'no ' + obsid(i) + '.fits in current directory'
      haveord = 0
    endelse
    if keyword_set(findfile(obsid(i)+'.fits')) then begin
      havefits = 1
    endif else begin
      print, 'no ' + obsid(i) + '.fits in current directory'
      havefits = 0
    endelse
    if havefits and haveord then begin
      print, form='(a,$)', obsid(i) + '...'
      bgcheck, obsid(i), icol
      junk = get_kbrd(1)
      print, ''
    endif
  endfor

end



