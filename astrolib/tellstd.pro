pro tellstd, obs, lst, file=file
;For a given LST, calculate the airmass towards a list of telluric standards.
;Inputs:
; obs (string) observatory designaton chosen from: 'cfht', 'irtf', 'kpno',
;   'lick', or 'mcd'.
; lst (string) local sidereal time to use in the calculation. Format must
;   be either 'HH MM SS' or 'HH:MM:SS'.
; file= (string) name of file containing data about telluric standards. If
;   not specified, then $HOME/idl/tellstd.dat is used.
;Outputs:
; Sorted list of standards sorted by airmass is written to the screen.
;Notes:
; Should precess coordinates to the current date.
; Should estimate JHK magnitudes from V and B-V.
;Edit History:
;13-Apr-98 Valenti  Wrote.

deffile = '/data/starlight1/valenti/idl/tellstd.dat'
if n_params() lt 2 then begin
  print, "syntax: tellstd, obs, lst [,file=]"
  print, "  e.g.: tellstd, 'kpno', '12 54 27'"
  print, "  obs = {'cfht', 'irtf', 'kpno', 'lick', 'mcd'}"
  print, "  default file = '" + deffile + "'"
  retall
endif

;Read telluric standards from disk file.
  if not keyword_set(file) then begin
    file = findfile(deffile)
    if not keyword_set(file) then begin
      message, /info, 'Could not find data file: ' + deffile
    endif
    file = file(0)
  endif
  openr, unit, file, /get_lun
  maxstd = 1000					;maximum number of standards
  hr = replicate(0, maxstd)			;init vectors
  name = replicate('', maxstd)
  ra = replicate('', maxstd)
  dec = replicate('', maxstd)
  epoch = replicate('', maxstd)
  vmag = replicate('', maxstd)
  bv = replicate('', maxstd)
  vsini = replicate('', maxstd)
  type = replicate('', maxstd)
  istd = 0
  name1 = ''					;define variable types
  ra1 = ''
  dec1 = ''
  type1 = ''
  while not eof(unit) do begin
    readf, unit, form='(i4,2x,a7,2x,a10,2x,a9,i6,2f7,i5,2x,a)' $
         , hr1, name1, ra1, dec1, epoch1, vmag1, bv1, vsini1, type1
    hr(istd) = hr1
    name(istd) = name1
    ra(istd) = ra1
    dec(istd) = dec1
    epoch(istd) = epoch1
    vmag(istd) = vmag1
    bv(istd) = bv1
    vsini(istd) = vsini1
    type(istd) = type1
    istd = istd + 1				;increment counter
  endwhile
  free_lun, unit
  nstd = istd					;number of standards read
  hr = hr(0:nstd-1)				;trim vectors
  name = name(0:nstd-1)
  ra = ra(0:nstd-1)
  dec = dec(0:nstd-1)
  epoch = epoch(0:nstd-1)
  vmag = vmag(0:nstd-1)
  bv = bv(0:nstd-1)
  vsini = vsini(0:nstd-1)
  type = type(0:nstd-1)

;Convert RA and Dec strings to radians. Precess to current epoch.
  dra = dblarr(nstd)				;init decimal vectors
  ddec = dblarr(nstd)
  for istd=0, nstd-1 do begin
    stringad, ra(istd)+' '+dec(istd), dra1, ddec1
    dra(istd) = dra1 * !dtor			;in radians
    ddec(istd) = ddec1 * !dtor			;in radians
  endfor

;List of known observatories.
  case strlowcase(obs) of			;force lowercase
    'cfht':  begin				;CFHT
      lat = 0.346030917d0			; +19 49 34
      lon = 2.713492477d0			; 155 28 18
      ht = 4198.
    end
    'irtf':  begin				;IRTF
      lat = 0.346030917d0			; +19 49 34
      lon = 2.713492477d0			; 155 28 18
      ht = 4198.
    end
    'kpno': begin				;Kitt Peak
      lat = 0.557865407d0			; +31 57.8 (1991 Almanac)
      lon = 1.947787445d0			; 111 36.0
      ht = 2120.
    end
    'lick': begin				;Lick 3-m
      lat = 0.651734547d0			; +37 20 29.9
      lon = 2.123019229d0			; 121 38 24.15
      ht = 1283.
    end
    'mcd': begin                  		;McDonald Observatory
      lat = 0.535321576d0			; +30 40 18.0
      lon = 1.815520577d0			; 104 01 18.0
      ht = 2075.
    end
    else: begin
      message, /info, 'Unknown observatory designation.'
      retall
    endelse
  endcase

;Convert local sidereal time string to decimal LST.
  i=0
  lst1 = lst					;local copy to modify
  while (i ne -1) do begin			;replace colons with spaces
    i = strpos(lst1, ':')
    if (i ne -1) then strput, lst1, ' ', i
  endwhile
  dlst = ten(getopt(lst1, 'F'))			;convert to decimal

;Compute angular description of zenith position.
  zenra = dlst * 15.0 * !dtor			;in radians
  zendec = lat					;in radians

;Compute angular distance between zenith and list of standards.
  adist = sphdist(dra, ddec, zenra, zendec)

;Select out those stars which are visible (above limiting airmass).
  limit = acos(1.0 / 10d0)			;above airmass of 10
  ivis = where(adist lt limit, nvis)
  if nvis le 0 then begin
    message, /info, 'No stars above limiting airmass?!'
    retall
  endif

;Sort indicies into descending angular distance.
  isort = sort(adist(ivis))
  ivis = ivis(isort)
  ivis = reverse(ivis)

;Trim vectors to include only stars that are visible.
  hr = hr(ivis)
  name = name(ivis)
  ra = ra(ivis)
  dec = dec(ivis)
  epoch = epoch(ivis)
  vmag = vmag(ivis)
  bv = bv(ivis)
  vsini = vsini(ivis)
  type = type(ivis)
  adist = adist(ivis)

;Calculate airmass.
  amass = 1.0 / cos(adist)

;Print results.
  print, ' Amass   HR    Name      RA          Dec      Epoch' $
       + '  Vmag Vsini SpecType'
  for ivis=0, nvis-1 do begin
    print, form='(f6.3,2x,i4,2x,a7,2x,a10,2x,a9,i6,f7.2,i5,2x,a)' $
         , amass(ivis), hr(ivis), name(ivis), ra(ivis), dec(ivis) $
         , epoch(ivis), vmag(ivis), vsini(ivis), type(ivis)
  endfor

end
