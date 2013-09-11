pro rdhrs, dataset, x, y, plot=plot

if n_params() lt 3 then begin
  print, 'syntax: rdhrs, dataset, x, y [,/plot]'
  retall
endif

  w = readfits(dataset+'_c0f.fits', /silent)
  s = readfits(dataset+'_c1f.fits', /silent)
  sz = size(w)
  npix = sz(1)
  if sz(0) eq 1 then nsp = 1 else nsp = sz(2)

  os = 4
  xmin = max(w(1,*))
  xmax = min(w(npix-2,*))
  x = xmin + (xmax-xmin) * findgen(os*npix) / (os*npix-1)
  y = fspline(w(*,0), s(*,0), x)
  if keyword_set(plot) then plot, x, y, ps=3, xsty=3, /yno

  disp = (x(os*npix-1) - x(0)) / (os*npix-1)
  t = fltarr(os*npix, nsp)
  for i=0, nsp-1 do begin
    ss = fspline(w(*,i), s(*,i), x)
    if i eq 0 then s0 = ss
    xcorl, s0, ss, 50, shft
    ss = fspline(w(*,i)+shft*disp, s(*,i), x)
    t(*,i) = ss
    if keyword_set(plot) then oplot, x, ss, ps=3
  endfor

  y = fltarr(os*npix)
  for i=0, os*npix-1 do begin
    tvec = t(i,*)
    tsort = tvec(sort(tvec))
;   y(i) = total(tsort(1:nsp-2)) / (nsp-2)
    y(i) = total(tsort) / nsp
  endfor

  if keyword_set(plot) then begin
    colors
    oplot, x, y, ps=10, co=2
  endif

end
