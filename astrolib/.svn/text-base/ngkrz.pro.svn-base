pro ngkrz, infile, outfile, tau=tau, nozeta=nozeta $
         , opflag=opflag, duplist=duplist,pgas=pgas
;Extracts atmosphere from Pheonix output file and writes into .krz file.
; infile (input string) name of Pheonix output file.
; outfile (input string) name of .krz format file to create.
; /tau (switch) selects reference tau as independent variable for output file.
; /nozeta (switch) suppresses zeta processing.
; opflag (input vector(20)) opacity flags for output file (overrides default).
; duplist (output vector) zeta id strings that were duplicated in file. If
;   no duplicates weer found, a null string is returned.
;Edit History:
; 21-Jan-96 Valenti  Adapted from ngzeta.pro.
; 30-Jan-96 Valenti  Added zeta logic.

if n_params() lt 1 then begin
  print, 'syntax: ngkrz, infile [, outfile, /tau, /nozeta, opflag=] '
  retall
endif

;Set default values for quantities not passed.
  if n_elements(tau) eq 0 then tau = 0			;true: use mass column
  if not keyword_set(opflag) then begin			;true: use defaults
    opflag = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1 $
             ,1, 1, 1, 0, 1, 0, 0, 0, 0, 0 ]
  endif 
  if tau then modtyp = 1 else modtyp = 0		;set code for type
  numbfrac = replicate(1e-20, 99)			;abundance fraction

;Inernal program parameters.
  maxzeta = 500						;maximum species
  zthresh = 0						;zeta/natom limit

;Open input file. Check for various errors.
  file = findfile(infile + ' ' + infile+'.gz')		;expand filename
  if not keyword_set(file) then begin			;true: no such file
    print,'ngkrz: no such input file: ' + infile
    retall
  endif
  if n_elements(file) gt 1 then begin			;true: multiple files
    print,'ngkrz: wildcard in input filename must expand to single file.'
    retall
  endif
  file = file(0)					;force scalar
  if strpos(file, '.gz') gt 0 then begin
    tmpf = 'web_krz.tmp'
    spawn, '\cp ' + file + ' ' + tmpf + '.gz ; gunzip -fn ' + tmpf + '.gz'
    openr, unit, tmpf, /get				;open temp file
  endif else begin
    openr, unit, file, /get				;open file
  endelse

;Read number of depth points.
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'no. of depth points:') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for number of depths.'
    free_lun, unit
    return
  endif
  ipos = strpos(buff, '=')				;find the = sign
  ndep = fix(strmid(buff, ipos+1, 999))			;read beyond = sign

;Find and read effective temperature.
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'model:   teff :') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for effective temperature.'
    free_lun, unit
    return
  endif
  teff = float(strmid(buff, 20, 7))			;read Teff

;Find and read logarithm of surface gravity [cm/s^2].
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'log(g):') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for gravity.'
    free_lun, unit
    return
  endif
  grav = float(strmid(buff, 24, 6))			;read log(g)

;Read reference wavelength used for optical depth scale.
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'tau depth scale is at') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for reference wavelength.'
    free_lun, unit
    return
  endif
  refwav = float(strmid(buff, 25, 9))			;read log(g)

;Find abundance data.
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'Element abundances :') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for abundances.'
    free_lun, unit
    return
  endif

;Read abundances.
;Assumes number fractions follow two lines after element code.
  repeat begin
    readf, unit, buff
    if strpos(buff, ' code:') eq 0 then begin		;found element codes
      nfields = (strlen(buff) - 11) / 12		;number of fields
      codes = intarr(nfields)				;init data vector
      reads, strmid(buff, 11, 999), codes		;extract codes
      indx = codes / 100 - 1				;convert to index
      readf, unit, buff					;flush one line
      readf, unit, buff					;read number fracs
      if strpos(buff, ' numb-frc.:') ne 0 then begin	;true: wrong line type
        print, 'ngkrz: missing number fractions in abundance data.'
        free_lun, unit
        return
      endif
      frac = fltarr(nfields)				;init data vector
      reads, strmid(buff, 11, 999), frac		;extract fractions
      iwhr = where(indx ge 0, nwhr)			;ignore elec fraction
      if nwhr gt 0 then begin				;true: atomic fracs
        numbfrac(indx(iwhr)) = frac(iwhr)		;save fractions
      endif
    endif
  endrep until eof(unit) $
            or strpos(buff, 'Element abundances relative') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF while reading abundances.'
    free_lun, unit
    return
  endif

;Find atmosphere.
  buff = ''
  repeat begin
    readf, unit, buff					;read a line
  endrep until eof(unit) $
            or strpos(buff, 'tstd temperature') ge 0
  if eof(unit) then begin				;true: unexpected EOF
    print, 'ngkrz: unexpected EOF looking for temperature structure.'
    free_lun, unit
    return
  endif

;Read atmosphere.
  dbuff = fltarr(10)					;row buffer
  data = fltarr(10, ndep)				;input storage
  for i=0, ndep-1 do begin
    readf, unit, dbuff
    data(*, i) = dbuff
    if eof(unit) then begin
      print, 'ngkrz: unexpected EOF while reading atmospheric structure.'
      free_lun, unit
      return
    endif
  endfor

;Extract all available data from table.
  idepth  = reform(data(0,*))				;depth index
  tauref  = reform(data(1,*))				;ref optical depth
  temp    = reform(data(2,*))				;temperature (K)
  pgas    = reform(data(3,*))				;gas pressure
  pelec   = reform(data(4,*))				;electron pressure
  density = reform(data(5,*))				;mass density
  molwt   = reform(data(6,*))				;mean molec. weight
  radius  = reform(data(7,*))				;radius (cm)
  depth   = reform(data(8,*))				;depth below radius
  kappaq  = reform(data(9,*))				;mass opacity

;Note: zeta = number density / partition function
;Look for optional zeta data.
  if not keyword_set(nozeta) then begin			;true: want zeta
    repeat begin
      readf, unit, buff					;read text line
    endrep until eof(unit) $
              or strpos(buff, 'atomic zeta') ge 0 $
              or strpos(buff, 'molecular zeta') ge 0
  endif

;Read zeta data.
  if not keyword_set(nozeta) and $
     not eof(unit) then begin				;true: found zeta
    repeat begin

;Look for table header.
      repeat begin
        readf, unit, buff				;read a line
      endrep until eof(unit) $
              or strpos(buff, '    l         tk') eq 0
      if not eof(unit) then begin			;true: found header

;Init arrays, if this is the first table to be processed.
        if not keyword_set(izeta) then begin		;need to init var
          izeta = 0					;zeta counter
          zetaid = strarr(maxzeta)			;all good zetaid
          zeta = dblarr(ndep, maxzeta)			;all good zeta
        endif

;Parse header line.
        len = strlen(buff)				;line length
        if (len - 16) mod 11 ne 0 then begin		;true: bad header
          print, 'ngkrz: strange line length in zeta table header.'
          print, 'ngkrz: ' + strmid(buff, 0, 50) + '....'
          free_lun, unit
          return
        endif
        ncol = (strlen(buff) - 16) / 11			;number of columns
        idbuff = strarr(ncol)				;zetaid for table
        for icol=0, ncol-1 do begin			;loop thru columns
          id = strmid(buff, 16+icol*11, 11)		;extract one header
          idbuff(icol) = strtrim(id, 2)			;trim all spaces
        endfor

;Read contents of table.
        zrow = dblarr(ncol)				;row of zeta values
        zbuff = dblarr(ndep, ncol)			;data in zeta table
        for idep=0, ndep-1 do begin			;loop thru depths
          readf, unit, buff				;read a line
          if eof(unit) then begin			;true: table too short
            print, 'ngkrz: end of file while reading table of zeta values.'
            free_lun, unit
            return
          endif
          reads, strmid(buff, 16, 999), zrow		;read row of zeta
          zbuff(idep, *) = zrow				;insert into table
        endfor

;Keep all species with nonzero zeta values.
        for icol=0, ncol-1 do begin			;loop thru cols
          if max(zbuff(*, icol)) gt 0 then begin	;true: nonzero zeta
            zetaid(izeta) = idbuff(icol)		;save zetaid
            zeta(*, izeta) = zbuff(*, icol)		;save zeta data
            izeta = izeta + 1				;increment counter
          endif
        endfor
      endif

;Loop back to look for more tables, until end of file is reached.
    endrep until eof(unit)				;until end of file
  endif

;Close input file.
  free_lun, unit					;close input file

;Calculate new atmospheric variables.
  kboltz = 1.380658e-16					;erg K
  nelec = pelec / kboltz / temp				;elecron density (1/cc)
  natom = (pgas - pelec) / kboltz / temp		;atomic density (1/cc)
  if not keyword_set(tau) then begin			;true: need masscol
    xold = depth(1:ndep-1)				;trim surface point
    yold = alog10(density(1:ndep-1))			;trim and take log
    osamp = 20						;oversampling factor
    nx = (ndep-1) * osamp + 1				;number of points
    x = max(depth) * dindgen(nx) / (nx - 1)		;fine depth scale
    dx = (x(nx-1) - x(0)) / (nx - 1)			;depth step size
    secder = nr_spline(xold, yold, /double)		;spline fit
    y = nr_splint(xold, yold, secder, x, /double)	;log(den) on fine grid
    yav = 10d0 ^ (0.5d0*(y(0:nx-2) + y(1:nx-1)))	;avg density over step
    m = dblarr(nx)					;init m on fine scale
    m(0) = 0d0						;no mass at top
    for i=0,nx-2 do m(i+1) = m(i) + yav(i)		;cumulative integral
    m = m * dx						;include constant step
    xnew = x(1:nx-1)					;exclude first point
    ynew = alog10(m(1:nx-1))
    masscol = fltarr(ndep)				;init mass column
    secder = nr_spline(xnew, ynew, /double)		;spline fit
    masscol(1:ndep-1) = 10.0^nr_splint(xnew, ynew $
      , secder, xold, /double)				;sample on old depths
    masscol(0) = 0.0     				;no mass at top
  endif

;Process the abundance data.
  abund = alog10(numbfrac)				;relative abundance
  abund(0) = 10^abund(0)				;no log for H
  abund(1) = 10^abund(1)				;no log for He
  feh = abund(25) + 4.3711				;determine [Fe/H]

;Trim zeta arrays.
  duplist = ''						;null list by default
  if keyword_set(nozeta) then begin
    nzeta = 0
  endif else begin
    nzeta = izeta					;number of species
  endelse
  if nzeta gt 0 then begin				;true: have zeta data
    zetaid = zetaid(0:nzeta-1)				;trim vector
    zeta = zeta(*, 0:nzeta-1)				;trim array

;Look for duplicate zeta id. Only keep the last data.
    dup = replicate(0, nzeta)				;clear flags
    for izeta=0, nzeta-2 do begin			;loop thru species
      iwhr = where(zetaid(izeta) $
        eq zetaid(izeta+1: nzeta-1), nwhr)		;look for duplicate
      if nwhr gt 0 then dup(izeta) = 1			;set flag
    endfor
    idup = where(dup eq 1, ndup)			;duplicate indices
;   if ndup gt 0 then begin				;true: found dups
;     duplist = zetaid(idup)				;return argument
;     inodup = where(dup eq 0, nzeta)			;nonduplicates
;     zetaid = zetaid(inodup)				;keep unique data
;     zeta = zeta(*, inodup)				;keep unique data
;   endif

;Remove species with zeta less than zthresh*natom at every depth
    if zthresh gt 0 then begin
      small = replicate(0, nzeta)			;clear flags
      for izeta=0, nzeta-1 do begin			;loop thru species
        if max(zeta(*, izeta) / natom) lt zthresh $
          then small(izeta) = 1
      endfor
      ismall = where(small eq 1, nsmall)		;duplicate indices
      if nsmall gt 0 then begin				;true: found dups
        ibig = where(small eq 0, nzeta)			;nonduplicates
        zetaid = zetaid(ibig)				;keep unique data
        zeta = zeta(*, ibig)				;keep unique data
      endif
    endif
  endif

;Generate output file name, if none was specified.
  if not keyword_set(outfile) then begin
    outfile = 'ng' + strtrim(round(teff/100),2) $
      + '-' + string(round(grav*10),form='(i2.2)') $
      + '-' + string(round(feh*10),form='(i2.2)') $
      + '.krz'
  endif

;Open output file and write comment line.
  openw, unit, outfile, /get				;open output file
  printf, unit, 'Data from file: ' + file

;Write model specification to output file.
  printf, unit, form='("T EFF=", f6.0, " GRAV=", ' $
    + 'f4.1, "  MODEL TYPE=", i2, " WLSTD=", f6.0)' $
    , teff, grav, modtyp, refwav

;Write opacity switches to output file.
  printf, unit, form='(20(i2), " - OPACITY SWITCHES")', opflag

;Write abundances and number of depths in atmosphere.
  printf, unit, form='(2f7.3, 8f7.2)', abund(0:9)
  printf, unit, form='(10f7.2)', abund(10:89)
  printf, unit, form='(9f7.2, i3)', abund(90:98), ndep

;Special screen dump of abundances with many significant figures.
; print, form='(2(f9.6,","), 4(f9.5,","))', abund(0:5)
; print, form='(6(f9.5,","))', abund(6:98)

;Write atmosphere to output file.
  for i=0, ndep-1 do begin
    if tau then begin
      printf, unit, form='(E16.9, ",", f9.1, ",", 3(E12.5, ","))' $
        , tauref(i), temp(i), nelec(i), natom(i), density(i)
    endif else begin
      printf, unit, form='(E16.9, ",", f9.1, ",", 3(E12.5, ","))' $
        , masscol(i), temp(i), nelec(i), natom(i), density(i)
    endelse
  endfor

;Close output file and exit.
  free_lun, unit
  return

end
