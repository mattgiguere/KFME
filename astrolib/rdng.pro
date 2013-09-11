pro rdng,file,atmo,teff,logg,feh,type,ndep,wlstd
;Unzips output structure from Allrad "NextGen" models and reads atmosphere.
;17-Nov-95 JAV Create.

if n_params() lt 2 then begin
  print, 'syntax: rdng, file,atmo [,teff,logg,feh,type,ndep,wlstd]'
  retall
endif

  wlstd = 12000.0			;reference wavelength (A)
  type  = 'TAU'

  tmpf = 'rdng.tmp'
  spawn, '\cp ' + file + ' ' + tmpf + '.gz ; gunzip -fn ' + tmpf + '.gz'

  openr, unit, tmpf, /get_lun
  buff = ''
  repeat readf, unit, buff $
    until strmid(buff, 0, 28) eq "   l        tstd temperature"
  readf, unit, buff			;skip first grid point

  ndep  = 49 
  tau   = fltarr(ndep)
  temp  = fltarr(ndep)
  pgas  = fltarr(ndep)
  pelec = fltarr(ndep)
  rho   = fltarr(ndep)
  buff = fltarr(6)
  for i=0, ndep-1 do begin
    readf, unit, buff
    tau(i)   = buff(1)
    temp(i)  = buff(2)
    pgas(i)  = buff(3)
    pelec(i) = buff(4)
    rho(i)   = buff(5)
  endfor

  free_lun, unit

  kboltz = 1.38066e-16
  xne = pelec / temp / kboltz
  xna = (pgas-pelec) / temp / kboltz

  atmo = fltarr(5, ndep)
  atmo(0,*) = tau
  atmo(1,*) = temp
  atmo(2,*) = pgas
  atmo(3,*) = pelec
  atmo(4,*) = rho

  flen = strlen(file)
; print,strmid(file, flen-25, 2) $
;   + '|' + strmid(file, flen-22, 3) $
;   + '|' + strmid(file, flen-19, 4)
  teff = 100 * float(strmid(file, flen-25, 2))
  logg = float(strmid(file, flen-22, 3))
  feh  = float(strmid(file, flen-19, 4))

  if abs(logg) eq 0 then logg = 0.0		;get rid of minus in -0.0

end
