pro zpatt, trans, glo=glo, gup=gup

if n_params() lt 1 then begin
  print, 'syntax: zpatt, trans [,glo ,gup=]'
  return
endif

  f_glo = n_elements(glo)
  f_gup = n_elements(gup)


  theta = 90
  if f_glo then begin
    if f_gup then begin
      zeeman, trans, theta, comps, gf=glo, gi=gup, geff=geff
    endif else begin
      zeeman, trans, theta, comps, gf=glo, geff=geff
    endelse
  endif else begin
    if f_glo then begin
      zeeman, trans, theta, comps, gi=gup, geff=geff
    endif else begin
      zeeman, trans, theta, comps, geff=geff
    endelse
  endelse
  print, '  geff=' + string(geff, form='(f5.3)')
  print

  split = reform(comps(0,*))
  wts   = reform(comps(1,*))
  state = reform(comps(2,*))

  ikeep = where(wts ne 0)
  split = split(ikeep)
  wts   = wts(ikeep)
  state = state(ikeep)

  isig = where(state eq -1, nsig)
  ipi  = where(state eq  0, npi)

  isort = sort(split(ipi))
  if npi le 7 then begin
    print, form='(i3,",",7(f9.5,","))', npi, split(ipi(isort))
    print, form='(4x,7(f9.5,","))', wts(ipi(isort))
  endif else begin
    print, form='(i3,",",8(f9.5,","))', npi, split(ipi(isort(0:6)))
    print, form='(4x,7(f9.5,","))', split(ipi(isort(7:*)))
    print, form='(4x,7(f9.5,","))', wts(ipi(isort(0:6)))
    print, form='(4x,7(f9.5,","))', wts(ipi(isort(7:*)))
  endelse

  isort = sort(split(isig))
  if nsig le 7 then begin
    print, form='(i3,",",7(f9.5,","))', nsig, split(isig(isort))
    print, form='(4x,7(f9.5,","))', wts(isig(isort))
  endif else begin
    print, form='(i3,",",7(f9.5,","))', nsig, split(isig(isort(0:6)))
    print, form='(4x,7(f9.5,","))', split(isig(isort(7:*)))
    print, form='(4x,7(f9.5,","))', wts(isig(isort(0:6)))
    print, form='(4x,7(f9.5,","))', wts(isig(isort(7:*)))
  endelse

end
