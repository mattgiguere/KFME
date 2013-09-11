pro obsinfo, obs, epoch, coord, year, month, day, limit=limit
;Tool to help plan observing runs.
;Inputs:
; obs (string) observatory designation: "ctio", "irtf", "kpno", "lick", "mcd"
; epoch (scalar) epoch of coordinates
;Example:
; obsinfo, 'kpno', 2000.0, '12 30 26 -03 10 08', 1995, 'feb', 25
;
;UNTESTED! STILL NEEDS COORDINATE PRECESSION! HANDLE SPLIT VISIBILITIES!

if n_params() lt 6 then begin
  print, 'syntax: obsinfo, obs, epoch, coord, year, month, day [,limit=]'
  retall
endif

;Find observatory.
  case strlowcase(obs) of
    'ctio': begin
       lon = +( 70 + 48.9/60.0)
       lat = -( 30 + 09.9/60.0)
       end
    'irtf': begin
       lon = +(155 + 28.3/60.0)
       lat = +( 19 + 49.7/60.0)
       end
    'kpno': begin
       lon = +(111 + 36.0/60.0)
       lat = +( 31 + 57.8/60.0)
       end
    'lick': begin
       lon = +(121 + 38.2/60.0)
       lat = +( 37 + 20.6/60.0)
       end
    'mcd': begin
       lon = +(104 + 01.3/60.0)
       lat = +( 30 + 40.3/60.0)
       end
    'not': begin
       lon = +( 17 + 52/60.0 + 59.7/3600.0)
       lat = +( 28 + 45/60.0 + 20.5/3600.0)
       end
    else: begin
       print, "valid observatories: ctio, irtf, kpno, lick, mcd, not"
       retall
       end
  endcase

;Define some internal variables.
  units = 0						;radians
  stringad, coord, starra, stardec
  zendec = lat						;degrees

;Parse the date.
  sz = size(month)
  if sz(sz(0)+1) eq 7 then begin
    case strlowcase(month) of
      'jan': imon =  1
      'feb': imon =  2
      'mar': imon =  3
      'apr': imon =  4
      'may': imon =  5
      'jun': imon =  6
      'jul': imon =  7
      'aug': imon =  8
      'sep': imon =  9
      'oct': imon = 10
      'nov': imon = 11
      'dec': imon = 12
      else: begin
        print, "valid months: jan, feb, mar, apr, may, jun, jul," $
             + " aug, sep, oct, nov, dec"
        retall
        end
    endcase
    datestr = strtrim(day,2) + '-' $
            + strupcase(month) + '-' $
            + strtrim(year,2)
  endif else begin
    imon = month
    monlist = [ 'jan','feb','mar','apr','may','jun' $
              , 'jul','aug','sep','oct','nov','dec' ]
    datestr = strtrim(day,2) + '-' $
            + strupcase(monlist(imon-1)) + '-' $
            + strtrim(year,2)
  endelse
  
  juldate, [year, imon, day, 0.0, 0.0], mjd
  print, '  Julian Date: ' + strtrim(mjd, 2)

  ndut    = 10 * 24 + 1
  dut     = 24d0 * dindgen(ndut) / (ndut-1d0)
  sunrav  = dblarr(ndut)
  sundecv = dblarr(ndut)
  jdv     = dblarr(ndut)
  gmstv   = dblarr(ndut)
  lmstv   = dblarr(ndut)
  sunzv   = dblarr(ndut)
  starzv  = dblarr(ndut)
  for idut=0, ndut-1 do begin

;Formulae from the 1995 Astronomical Almanac, page C24.
;Coordinates good to 0.01 degree between 1950 and 2050.
    jd = mjd + 2400000.0d0 + dut(idut) / 24d0		;no 0.5 in IDL routine
    nday2000 = jd - 2451545d0
    mnlonsun = 280.466d0 + 0.9856474d0 * nday2000
    mnlonsun = (mnlonsun + 360d3) mod 360d0
    mnanom   = 357.528d0 + 0.9856003d0 * nday2000
    mnanom   = (mnanom + 360d3) mod 360d0
    ecliplon = mnlonsun + 1.915d0 * sin(!dtor*mnanom) $
                        + 0.020d0 * sin(2d0*!dtor*mnanom)
    obliqecl = 23.440d0 - 0.0000004d0 * nday2000
    sunra = atan(cos(!dtor*obliqecl) * tan(!dtor*ecliplon)) / !dtor
    sunra = (sunra + 90d2) mod 90d0
    sunra = sunra + 90d0 * floor(ecliplon / 90d0)
    sundec = asin(sin(!dtor*obliqecl) * sin(!dtor*ecliplon)) / !dtor

;Formulae from the 1995 Astronomical Almanac, page C24.
;Greenwich mean sidereal time.
    tu = (jd - 2451545d0) / 36525d0
    gmstat0h = 24110.54841d0 + tu*(8640184.812866d0 $
                             + tu*(      0.093104d0 $
                             - tu*(      6.2d-6     )))
    gmstat0h = (gmstat0h / 3600d0 + 24d3) mod 24d0

;In 1995 there were 1.0027379093d0 mean sidereal days per mean solar day.
;Local mean sidereal time.
    lmst = gmstat0h $
         + dut(idut) * 1.0027379093d0 $
         + lon / 15d0

;Get solar and stellar zenith angles.
    zenra  = 15d0 * ((lmst+24d2) mod 24d0)		;degrees
    gcirc, units, !dtor*sunra, !dtor*sundec $
                , !dtor*zenra, !dtor*zendec $
                , dist
    sunz = dist / !dtor
    gcirc, units, !dtor*starra, !dtor*stardec $
                , !dtor*zenra, !dtor*zendec $
                , dist
    starz = dist / !dtor

;Save results.
    sunrav(idut)  = sunra
    sundecv(idut) = sundec
    jdv(idut)     = jd
    gmstv(idut)   = gmstat0h
    lmstv(idut)   = lmst
    sunzv(idut)   = sunz
    starzv(idut)  = starz
  endfor
  sunra  = total(sunrav)  / ndut
  sundec = total(sundecv) / ndut
  print, form='("  Solar RA:   ",i3.2,2i3.2)', round(sixty(sunra/15.0))
  print, form='("  Solar Dec:  ",i3.2,2i3.2)', round(sixty(sundec))
  print, form='("  Zenith Dec: ",f6.2," degrees")', lat

;Calculate derived quantities.
  zenrav  = 15d0 * ((lmstv+24d2) mod 24d0)		;degrees
  starhav = (zenrav - starra) / 15d0			;hours
  starhav  = ((starhav+24d2) mod 24d0)			;degrees
  iwhr = where(starhav gt 12, nwhr)
  if nwhr gt 0 then starhav(iwhr) = starhav(iwhr) - 24d0

;Define observing limits.
  if n_elements(limit) eq 0 then limit = 108d0		;18 degree twilight
  itwi = where(sunzv(0:ndut-2) lt limit $
            and sunzv(1:ndut-1) ge limit, ntwi)
  if ntwi lt 1 then begin
    print, 'No twlilight.'
    return
  endif else begin
    itwi = itwi(0)
    uttwi = dut(itwi) + (dut(itwi+1) - dut(itwi)) $
                      * (limit         - sunzv(itwi)) $
                      / (sunzv(itwi+1) - sunzv(itwi))
    print, form='("  Twilight:   ",f6.2," UT")', uttwi
  endelse
  idawn = where(sunzv(0:ndut-2) gt limit $
            and sunzv(1:ndut-1) le limit, ndawn)
  if ndawn lt 1 then begin
    print, 'No twlilight.'
    return
  endif else begin
    idawn = idawn(0)
    utdawn = dut(idawn) + (dut(idawn+1) - dut(idawn)) $
                      * (limit         - sunzv(idawn)) $
                      / (sunzv(idawn+1) - sunzv(idawn))
    print, form='("  Dawn:       ",f6.2," UT")', utdawn
  endelse
  duration = utdawn - uttwi
  print, form='("  Duration:   ",f6.2," hours")', duration

;Calculate stellar information.
  print, ' '
  amasslim = 3.0
  iwhr = where(sunzv gt limit $
    and (abs(starhav) le 6 and starzv le acos(1/amasslim)/!dtor), nwhr)
  if nwhr le 0 then begin
    print, '  Star is never accessible.'
  endif else begin
    starbeg = min(dut(iwhr))
    starend = max(dut(iwhr))
    stardur = starend - starbeg
    print, form='("  Star Begin: ",f5.1," UT")', starbeg
    print, form='("  Star End:   ",f5.1," UT")', starend
    print, form='("  Available:  ",f5.1," hours")', stardur
  endelse

  if 0 eq 0 then begin
    plot, dut, sunzv, xr=[uttwi,utdawn], yr=[0,180] $
        , /xsty, /ysty, li=1 $
        , xtit='UT hours' $
        , ytit='Solar/Stellar Zenith Angle' $
        , chars=1.4, tit=datestr+' UT' $
        , xmarg=[7,2] $
        , yticks=6, yminor=3,  ymarg=[4,2]
    oplot, dut, starzv
    amasslim = 3.0
    iwhr = where(abs(starhav) gt 6 $
              or starzv gt acos(1/amasslim)/!dtor, nwhr)
    dummy = starzv
    if nwhr gt 0 then dummy(iwhr) = 1001
    oplot, dut, dummy, max=1000, th=5
  endif

end
