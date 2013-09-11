pro bsc,starid,hr=hrflag,hd=hdflag,norem=norem
;Read and print data from electronic version of 5th Bright Star Catalog.
; starid (input scalar) catalog number of the star of interest. Default
;   catalog is HR, but HD can be specified with the /HD flag.
; /hr (switch) causes starid to be interpreted as an HR number (which is
;   the default in any case).
; /hd (switch) causes starid to be interpreted as an HD number.
; /norem (switch) supresses the (sometimes lengthy) catalog remarks.
;  to be a valid search index.
;08-Feb-95 JAV Create.
;21-Feb-96 JAV Modified code to find by star name - requires new database.
;29-Jul-96 JAV Program stopped working, perhaps because of a change in the
;		database software? Fixed by using element 0 of one-element
;		vectors	returned by dbext in conditionalexpressions.

;Print syntax if no search constraint was specified.
  if n_params() lt 1 then begin
    print,'syntax: bsc,starid [,/hr,/hd,/norem]'
    retall
  endif

;Set system variables.
;  setenv,'ZDBASE=/starlight/data1/valenti/idl/zdbase'
  defsysv,'!TEXTOUT',2
  defsysv,'!TEXTUNIT',0
  defsysv,'!PRIV',0

;Check if starid is a string, implying a search by name of star.
  vinfo = size(starid)
  if vinfo(vinfo(0)+1) eq 7 then nameflag = 1 else nameflag = 0

;Check for a sensible combination of flags.
  nflag = keyword_set(hrflag) + keyword_set(hdflag) $
        + keyword_set(nameflag)
  if nflag gt 1 then begin
    if nameflag then begin
      print,'bsc: when "starid" is a star name, do not use "/hr" or "/hd".'
    endif else begin
      print,'bsc: the flags "/hr" and "/hd" may not be used together.'
    endelse
    retall
  endif

;Open database.
  dbopen,'yale_bs'

;Init search constraint.
  scon = ''

;Use the HD number as the search constraint, if specified.
  if keyword_set(hdflag) gt 0 then begin
    if strlen(scon) gt 0 then scon = scon + ','
    scon = scon + 'hd=' + strtrim(string(starid),2)
  endif

;Use the HR number as the search constraint, if specified.
  if keyword_set(hrflag) or nflag eq 0 then begin
    if strlen(scon) gt 0 then scon = scon + ','
    scon = scon + 'hr=' + strtrim(string(starid),2)
  endif

;Use Bayer/Flamsteed name as the search constraint, if specified.
  if keyword_set(nameflag) gt 0 then begin
    if strlen(scon) gt 0 then scon = scon + ','
    starname = strtrim(starid, 2)			;strip end spaces
    ispc = strpos(starname, ' ')
    if ispc eq -1 then begin				;true: no spaces
      scon = scon + 'name=' + starname
    endif else begin					;else: assume 1 space
      scon = scon + 'name=' + strmid(starname,0,ispc) $
           + ','  + 'name=' + strmid(starname,ispc+1,999)
    endelse
  endif

;Do the search.
  list = dbfind(scon,/silent)

;Issue warning if multiple matches were found.
  if n_elements(list) gt 1 then begin
    dbext, list, 'hr', list2
    print,'bsc: matches for HR', list2, form='(a,11i5,10(/,4x,14i5))'
    return
  endif

;Handle case where no entry was found.
  if list(0) eq 0 then begin

;If we are searching for a name, try the name field in the remarks database.
    if nameflag then begin
      dbclose
      dbopen,'yale_bs_rmks'
      list = dbfind(['category=n', 'remarks='+starid],/silent)
      if list(0) gt 0 then begin
        nlist = n_elements(list)
        if nlist eq 1 then begin
          dbext, list, 'hr', list2
          bsc, list2
          return
        endif else begin
          dbext, list, 'hr', list2
          print,'bsc: matches for HR', list2, form='(a,11i5,10(/,4x,14i5))'
          return
        endelse
      endif
    endif

;No match found.
    print,'bsc: no match found in Bright Star Catalog'
    return
  endif

;Get various catalog identifications.
  dbext,list,'hr, hd, name, dm_no, sao, fk5, dble_name, var_star' $
            , hr, hd, name, dm_no, sao, fk5, dble_name, var_star

;Construct and print ID string.
  idstr = 'HR ' + strtrim(string(hr),2)
  if hd(0) ne 0 then idstr = idstr $
     + ' = HD ' + strtrim(string(hd),2)
  if strtrim(name(0),2) ne '' then idstr = idstr $
     + ' = ' + strtrim(strcompress(name),2)
  if strtrim(var_star(0),2) ne '' then idstr = idstr $
     + ' = ' + strtrim(strcompress(var_star),2)
  if strtrim(dm_no(0),2) ne '' then idstr = idstr $
     + ' = ' + strtrim(strcompress(dm_no),2)
  if sao(0) ne 0 then idstr = idstr $
     + ' = SAO ' + strtrim(string(sao),2)
  print,idstr

;Get various coordinates.
  dbext,list,'ra_1900, dec_1900, ra_2000, dec_2000' $
            , ra_1900, dec_1900, ra_2000, dec_2000

;Construct and print coordinate string.
  coordstr = 'J2000: RA=' + strtrim(strcompress(ra_2000),2) $
         + '  Dec=' + strtrim(strcompress(dec_2000),2) + ', ' $
         + '  B1900: RA=' + strtrim(strcompress(ra_1900),2) $
         + '  Dec=' + strtrim(strcompress(dec_1900),2)
  print,coordstr

;Get photometry and parallax.
  dbext,list,'vmag, vmag_code, bv, bv_code, ub, ub_code, ri, ri_code' $
            , vmag, vmag_code, bv, bv_code, ub, ub_code, ri, ri_code
  dbext,list,'prlax, prlax_code' $
            , prlax, prlax_code

;Construct and print photometry string.
  photstr = 'V=' + strtrim(string(vmag,form='(f10.2)'),2)
  if strtrim(vmag_code(0),2) ne '' then photstr = photstr + vmag_code(0)
  if bv(0) ne 0.0 then photstr = photstr $
    + '   (B-V)=' + strtrim(string(bv,form='(f10.2)'),2)
  if strtrim(bv_code(0),2) ne '' then photstr = photstr + bv_code(0)
  if ub(0) ne 0.0 then photstr = photstr $
    + '   (U-B)=' + strtrim(string(ub,form='(f10.2)'),2)
  if strtrim(ub_code(0),2) ne '' then photstr = photstr + ub_code(0)
  if ri(0) ne 0.0 then photstr = photstr $
    + '   (R-I)=' + strtrim(string(ri,form='(f10.2)'),2)
  if strtrim(ri_code(0),2) ne '' then photstr = photstr + ri_code(0)
  if prlax(0) ne 0.0 then begin
    if strtrim(prlax_code(0),2) eq '' then begin
      photstr = photstr + '   Dist='
    endif else begin
      photstr = photstr + '   Dyn_Dist='
    endelse
    if prlax(0) gt 0.3162 then $
      photstr = photstr + strtrim(string(1.0/prlax,form='(f10.3)'),2)
    if prlax(0) gt 0.1 and prlax(0) le 0.3162 then $
      photstr = photstr + strtrim(string(1.0/prlax,form='(f10.2)'),2)
    if prlax(0) gt 0.03162 and prlax(0) le 0.1 then $
      photstr = photstr + strtrim(string(1.0/prlax,form='(f10.1)'),2)
    if prlax(0) gt 0.01 and prlax(0) le 0.03162 then $
      photstr = photstr + strtrim(string(fix(0.5 + 1.0/prlax)),2)
    if prlax(0) gt 0.003162 and prlax(0) le 0.01 then $
      photstr = photstr + strtrim(string(10*fix(0.5 + 0.1/prlax)),2)
    if prlax(0) gt 0.0 and prlax(0) le 0.003162 then $
      photstr = photstr + strtrim(string(100*fix(0.5 + 0.01/prlax)),2)
    if prlax(0) lt 0.0 then begin
      photstr = photstr + '(parallax<0)'
    endif else begin
      photstr = photstr + ' pc'
    endelse
  endif
  print,photstr

;Get spectral type, radial velocity, and vsini information.
  dbext,list,'spec_type, rad_vel, rad_vel_cm, vsini_limits, vsini, vsini_unc' $
            , spec_type, rad_vel, rad_vel_cm, vsini_limits, vsini, vsini_unc

;dbext,list,'glat,glon',glat,glon

;Construct and print spectral type and velocity information.
  specvstr = strtrim(strcompress(spec_type),2)
  specvstr = specvstr $
    + '   RV=' + strtrim(string(rad_vel),2) + ' km/s'
  if strtrim(rad_vel_cm(0),2) ne '' then specvstr = specvstr $
    + ' (' + strtrim(strcompress(rad_vel_cm),2) + ')'
  if vsini(0) ne 0.0 then begin
    specvstr = specvstr + '   vsini='
    if strtrim(vsini_limits(0),2) ne '' then specvstr = specvstr $
      + strtrim(strcompress(vsini_limits),2)
    specvstr = specvstr + strtrim(string(vsini),2)
    if strtrim(vsini_unc(0),2) ne '' then specvstr = specvstr $
      + strtrim(strcompress(vsini_unc),2)
    specvstr = specvstr + ' km/s'
  endif
  print,specvstr

;Get information about companions.
  dbext,list,'dble_mag_dif, dble_sep, dble_id, dble_no' $
            , dble_mag_dif, dble_sep, dble_id, dble_no

;Preprocess the data for companions.
  mdiff = dble_mag_dif(0)
  sep = dble_sep(0)
  id = strtrim(dble_id(0), 2)
  ncomp = dble_no(0)
  if sep gt 0.0 or mdiff gt 0.0 and (ncomp eq 0 and id eq '') then begin
    ncomp = 2
    id = 'AB'
  endif

;Construct and print string containing information about compainions.
  if mdiff gt 0.0 or sep gt 0.0 or ncomp gt 0 then begin
    compstr = ''
    if ncomp gt 0 then compstr = compstr + 'Ncomp=' $
      + strtrim(string(ncomp),2) + '   '
    if id ne '' then compstr = compstr + id + ': '
    if sep gt 0.0 or mdiff gt 0.0 then begin
      compstr = compstr + 'Sep=' $
        + strtrim(string(sep,form='(f10.1)'),2) + '"'
      compstr = compstr + '  DeltaV=' $
        + strtrim(string(mdiff,form='(f10.1)'),2) + ' mag'
    endif
    print,compstr
  endif 

;Close catalog database.  If no remarks are desired, then exit.
  dbclose
  if keyword_set(norem) then return

;Open remarks database.
  dbopen,'yale_bs_rmks'

;Find the comments associated with our star.
  list = dbfind('hr='+strtrim(hr,2),/silent)

;If no comments were found, then return.
  if list(0) eq -1 then return

;Get remarks for this object.
  dbext,list,'category, remarks' $
            , category, remarks

;Loop through remarks, printing them out.
  nrem = n_elements(category)
  for irem = 0, nrem-1 do begin

;Concatenate multiple string from the same category.
    catlab = strmid(category(irem),1,3)
    rem = strtrim(strcompress(remarks(irem)),2)
    while strmid(category((irem+1)<(nrem-1)),1,3) eq catlab $
      and irem lt nrem-1 do begin
      irem = irem + 1
      rem = rem + ' ' + strtrim(strcompress(remarks(irem)),2)
    endwhile

    cat = '*     '
    strput,cat,strtrim(catlab,2) + ':',1
    while rem ne '' do begin
      remlen = strlen(rem)

;Print final line of remark.
      if remlen le 70 then begin
        print, cat + rem
        ncut = 70

;Can't fit entire remaining portion of remark on one line.  Try to break at
; a period followed by a space, a comma followed by a space, a space, or at
; the 70th character (in decreasing order of preference).
      endif else begin
        iper = where(byte(strmid(rem,0,70)) eq (byte('.'))(0) $
                 and byte(strmid(rem,1,71)) eq (byte(' '))(0),nper)
        if nper gt 0 then begin
          ncut = max(iper)
        endif else begin
          icom = where(byte(strmid(rem,0,70)) eq (byte(','))(0) $
                   and byte(strmid(rem,1,71)) eq (byte(' '))(0),ncom)
          if ncom gt 0 then begin
            ncut = max(icom)
           endif else begin
            ispc = where(byte(strmid(rem,0,70)) eq (byte(' '))(0),nspc)
            if nspc gt 0 then ncut = max(ispc)-1 else ncut = 70
          endelse
        endelse
        print,cat + strmid(rem,0,ncut+1)
      endelse
      cat = '       '
      rem = strtrim(strmid(rem,ncut+2,5000),2)
    endwhile
  endfor

; print,glat,glon

;Close remarks database and exit.
  dbclose

end
