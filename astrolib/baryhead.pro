pro baryhead, head, barycorr, jdmid, addcards=addcards
;Calculates barycentric velocity correction using information in FITS header.
;
;Input/Output:
; HEAD (string vector(ncards)) FITS header as a string array; currently the
;   following card names (or listed synonyms) and typical formats are
;   required:
;
;     DATE-OBS= '20/08/95'
;     UT      = '10:22:10.39'
;     DARKTIME= 3600.0
;     RA      = '22:53:02.30'
;     DEC     = '-14:16:58.00'
;     EPOCH   = 1995.64					; of RA and DEC
;     OBSERVAT= 'MCDONALD'
;
;   HEAD is modified only if the /ADDCARDS switch is specified.
;
;Input:
; /ADDCARDS (switch) setting this keyword causes the calculated values of
;   BARYCORR and JDMID to be inserted into HEAD
;
;Output
; [BARYCORR] (scalar) or (string 'none') barycentric velocity correction
;   (km/s) that is to be applied to an observatory frame wavelength scale;
;   unlike the convention for stellar radial velocities, BARYCORR is
;   **negative** when the motion of the earth around the Sun gives an extra
;   component of radial velocity **away** from the source; this convention
;   is used by bary.pro; in order to shift the observatory frame wavelength
;   scale to the barycentric frame, apply the tranformation:
;
;        BARYWAVE = OBSWAVE * (1 + BARYCORR/2.9979246d5)
;
;   if required information is missing from the header or cannot be located,
;   the return value of BARYCORR will be 'none'
; [JDMID] (scalar) Julian data at the "middle" of the exposure, obtained by
;   adding half the DARKTIME to the Julian date at the beginning of the
;   observation.
;
;Notes:
;  If the header is from a coadded image that was created without updating
;    DARKTIME, then the correction from JDSTART to JDMID will be wrong.
;    This will cause an error in BARYCORR, as well.
;
;History:
; 28-Jul-1998 Valenti  Initial coding for McDonald Observatory headers.

if n_params() lt 1 then begin
  print, 'syntax: baryhead, head [ ,barycorr ,jdmid ,/addcards ]'
  return
end

;Set BARYCORR to error flag value in case we return during processing.
  barycorr = 'none'					;flag error

;Get UT date of observation.
  dateobs = strtrim(sxpar(head, 'DATE-OBS', count=count), 2)
  if count lt 1 then begin
    dateobs = strtrim(sxpar(head, 'DATE_OBS', count=count), 2)
  endif
  if count lt 1 then begin
    message, /cont, 'no DATE-OBS card in header - could not compute BARYCORR'
    return
  endif

;Get UT time of observation.
  ut = strtrim(sxpar(head, 'UT', count=count), 2)
  if count lt 1 then begin
    ut = strtrim(sxpar(head, 'TIME_OBS', count=count), 2)
  endif
  if count lt 1 then begin
    ut = strtrim(sxpar(head, 'TIME', count=count), 2)
  endif
  if count lt 1 then begin
    message, /cont, 'no UT card in header - could not compute BARYCORR'
    return
  endif

;Get duration of observation.
  darktime = strtrim(sxpar(head, 'DARKTIME', count=count), 2)
  if count lt 1 then begin
    darktime = strtrim(sxpar(head, 'ITIME', count=count), 2)
  endif
  if count lt 1 then begin
    darktime = strtrim(sxpar(head, 'INT_S', count=count), 2)
  endif
  if count lt 1 then begin
    darktime = strtrim(sxpar(head, 'EXPOSURE', count=count), 2)
  endif
  if count lt 1 then begin
    message, /cont, 'no exposure time in header - could not compute BARYCORR'
    return
  endif

;Get right ascension. Check for missing quotes in header card.
  ra = sxpar(head, 'RA', count=count)
  if count eq 1 then begin				;true: found something
    if (size(ra))(1) ne 7 then begin			; but not a string?!
      ira = where(strmid(head, 0, 9) eq 'RA      =' $
                 , nra)					;find RA card
      if nra lt 1 then message, 'internal logic error while reparsing RA'
      ra = strmid(head(ira(0)), 9, 21)			;extract string
    endif
    ra = strtrim(ra, 2)				;trim spaces
  endif
  if count lt 1 then begin
    message, /cont, 'no RA card in header - could not compute BARYCORR'
    return
  endif

;Get declination. Check for missing quotes in header card.
  dec = sxpar(head, 'DEC', count=count)
  if count eq 1 then begin				;true: found something
    if (size(dec))(1) ne 7 then begin			; but not a string?!
      idec = where(strmid(head, 0, 9) eq 'DEC     =' $
                 , ndec)				;find DEC card
      if ndec lt 1 then message, 'internal logic error while reparsing DEC'
      dec = strmid(head(idec(0)), 9, 21)		;extract string
    endif
    dec = strtrim(dec, 2)				;trim spaces
  endif
  if count lt 1 then begin
    message, /cont, 'no DEC card in header - could not compute BARYCORR'
    return
  endif

;Get observatory name.
  observat = strtrim(sxpar(head, 'OBSERVAT', count=count), 2)
  if count lt 1 then begin
    observat = strtrim(sxpar(head, 'TELESCOP', count=count), 2)
  endif
  if count lt 1 then begin
    message, /cont, 'no observatory in header - could not compute BARYCORR'
    return
  endif

;Get epoch of RA and DEC.
  epoch = strtrim(sxpar(head, 'EPOCH', count=count), 2)
  if count lt 1 then begin
    dummy = sxpar(head, 'LICK', count=count)
    lick_epoch = 1
  endif
  if count lt 1 then begin
    message, /cont, 'no EPOCH card in header - could not compute BARYCORR'
    return
  endif

;Parse date string. Go to great pains to trap errors.
  count = 0						;reset counter
  while strpos(dateobs, '-') ge 0 do begin
    strput, dateobs, '/', strpos(dateobs, '-')		;replace "-" by "/"
    count = count + 1
  endwhile
  if count ne 0 and count ne 2 then begin		;bad number of "-"
    print, 'unexpected number of "-" in date - could not compute BARYCORR'
    return
  endif
  isl = where(byte(dateobs) eq (byte('/'))(0), nsl)	;find slashes
  if nsl ne 2 then begin				;bad number of "/"
    print, 'did not find two "/" in date - could not compute BARYCORR'
    return
  endif
  utday = fix(strmid(dateobs, 0, isl(0)))		;extract day
  utmon = fix(strmid( dateobs, isl(0)+1 $
                    , isl(1)-isl(0)-1) )		;extract month
  utyr  = fix(strmid(dateobs, isl(1)+1, 99))		;extract year
  if utyr lt 999 then utyr = utyr + 1900		;add full year
  if utyr lt 1930 then utyr = utyr + 100		;year 2000 kludge

;Parse time string. Go to great pains to trap errors.
  ico = where(byte(ut) eq (byte(':'))(0), nco)	;find colons
  if nco ne 2 then begin				;bad number of "/"
    print, 'did not find two ":" in time - could not compute BARYCORR'
    return
  endif
  uthr  = fix(strmid(ut, 0, ico(0)))			;extract hours
  utmin = fix(strmid(ut, ico(0)+1, ico(1)-ico(0)-1))	;extract minutes
  utsec = fix(strmid(ut, ico(1)+1, 99))			;extract seconds
  utmin = utmin + utsec / 60.0				;fractional minutes

;Parse duration of observation.
  darktime = float(darktime)				;catch weird strings

;Calculate Julian date at start and middle of observation.
  juldate, [utyr, utmon, utday, uthr, utmin], jdstart	;calculate julian date
  jdmid = jdstart + 0.5d0 * darktime / (24d0 * 3600d0)	;JD at midpoint of obs
  jdstart = jdstart + 2.4d6				;convert reduced JD
  jdmid = jdmid + 2.4d6					;convert reduced JD

;Convert JDMID back to a calendar date.
  daycnv, jdmid, midyr, midmon, midday, midhr		;convert to calendar
  midmin = 60.0 * (midhr  - floor(midhr))		;extract minutes
  midhr  = floor(midhr)					;keep integer part

;Construct date string expected by bary.pro.
  dstr = strtrim(midyr, 2) $
       + string(midmon, form='(1x,i2.2)') $
       + string(midday, form='(1x,i2.2)') $
       + string(midhr,  form='(1x,i2.2)') $
       + string(midmin, form='(1x,f5.2)')

;Parse RA string.
  count = 0						;reset counter
  while strpos(ra, ':') ge 0 do begin
    strput, ra, ' ', strpos(ra, ':')			;replace ":" by " "
    count = count + 1
  endwhile
  if count ne 0 and count ne 2 then begin		;bad number of "-"
    print, 'unexpected number of ":" in RA - could not compute BARYCORR'
    return
  endif

;Parse DEC string.
  count = 0						;reset counter
  while strpos(dec, ':') ge 0 do begin
    strput, dec, ' ', strpos(dec, ':')		;replace ":" by " "
    count = count + 1
  endwhile
  if count ne 0 and count ne 2 then begin		;bad number of "-"
    print, 'unexpected number of ":" in DEC - could not compute BARYCORR'
    return
  endif

;Construct coordinate string.
  cstr = ra + ' ' + dec
  cstr = strtrim(strcompress(cstr), 2)

;Parse the epoch string. Lick implicitly uses epoch of observation.
  if keyword_set(lick_epoch) then begin			;true: lick is special
    juldate, [utyr, 1, 1, 0, 0], jdboy			;JD at beg of year
    jdboy = jdboy + 2.4d6				;convert reduced JD
    epoch = utyr + (jdmid - jdboy) / 365.25d0		;epoch of observation
  endif
  epoch = float(epoch)					;should by a number

;Validate and convert the observatory name.
  case strupcase(strtrim(observat, 2)) of
    '3M-COUDE':  obs = 'l3'
    'NASA IRTF': obs = 'cfht'
    'KPNO':      obs = 'kp'
    'MCDONALD':  obs = 'mcd'
  else: begin
    print, 'unrecognized observatory - could not compute BARYCORR'
    return
    end
  endcase

;Print diagnostic.
; print, "  bary,'" + dstr + "', '" + cstr + "', " $
;      + strtrim(epoch, 2) + ", obs='" + obs + "'"

;Calculate barycentric correction.
  bary, dstr, cstr, epoch, vvec, obs=obs		;calculate velocities
  barycorr = vvec(0)					;extract radial comp

;Write results into header; replace existing card, if present.
  if keyword_set(addcards) then begin
    sxaddpar, head, 'JDMID', jdmid $
            , ' Julian date at midpoint of observation' $
            , format='f19.7', before='COMMENT'		;add JDMID
    sxaddpar, head, 'BARYCORR', barycorr $
            , ' barycentric velocity correction (km/s)' $
            , form='f19.5', after='JDMID'		;add BARYCORR
  endif

end
