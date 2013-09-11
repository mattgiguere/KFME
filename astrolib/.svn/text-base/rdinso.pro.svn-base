pro rdinso,w,s,w1,w2,atm=atm,wn=wn,thresh=thresh
;procedure to read the IR solar atlas and return the values between
;w1 and w2.  
;INPUTS:
;  w1 - lower limit of wavelength region of interest in microns
;       (wavenumbers if /WN set)
;  w2 - upperlimit of wavelength region of interest in microns
;       (wavenumbers if /WN set)
;OUTPUTS:
;  w - wavelength scale in microns (wavenumbers if /WN set)
;  s - solar irradiance spectrum 
;KEYWORDS:
;  /wn - sets inputs and output wavelength scale to be in wavenumbers
;  atm=atm - earth atmospheric  spectrum
;  thresh=thresh - when the atmospheric component is le THRESH, SOLar
;                  spectrum will not be present (default: THRESH = 0.3)
;BUGS:
;  because the data are stored as wavenumbers, output will always go from
;  from integer wavenumber to integer wavenumber (ie. although the user
;  might specify 2.3 to 2.5 microns the actual output will be from
;  fix(10000/2.5) = 4000 to fix(10000/2.3) + 1 = 4348 in wavenumbers or
;  2.2999 microns to 2.5 microns).
;NOTES:
;  data are stored in two places: 
;           IND_WN - /data/richter/solar/nsoir_sol_ind.bin
;           SC_?? - /data/richter/solar/nsoir_sol_scal.bin
;                                       
;  IND_WN: should be read in using READU.  Its length will be equal
;          to the # of wavenumbers with data * 3 (=# of records) 
;	(i) - wavenumber who's data is in records 3*i,3*i+1,3*i+2
;
;  SC_DATA:  should be read in using ASSOCiated variables.  Each record
;            is 106 words long.  
;     record(3*N) is the scaled values for wavenumber N.
;     record(3*N + 1) is the scaled atmospheric value
;			atm(i)=0.5 + SC_ATM(i) / 5d4
;     record(3*N + 2) is the scaled solar value
;			sol(i)=0.5 + SC_SOL(i) / 5d4
;
;mjr 26-Apr-1993: create
;jav 22-Sep-1993: renamed from rdirsol.pro. Changed syntax and location of
; atlas files.

if n_params() lt 4 then begin
  print,'syntax: rdinso,w,s,w1,w2[,atm=atm][,/wn][,thresh=thresh (def=0.3)'
  retall
endif

  ysol=1					;logical: user wants solar
  if not(keyword_set(thresh)) then thresh=0.3	;default value
  if (w1 gt 1000 and not(keyword_set(wn))) or (w1 lt 1000 and $
          keyword_set(wn))  then begin		;true: user error
    print,'WARNING: if using wavenumbers have /WN set, '
    print,'         if using microns, do NOT have /WN set.'
    retall
  endif

  datfile='/data/starlight1/valenti/idl/nsoir_sol_scal.bin'
  indfile='/data/starlight1/valenti/idl/nsoir_sol_ind.bin'
;  datfile='/scratch/richter/solar/photatl/scal.bin'
;  indfile='/scratch/richter/solar/photatl/ind.bin'

  openr,indlun,indfile,/get_lun
  ind_wn=intarr(6410)			;valid for data stored on 26-Apr
  readu,indlun,ind_wn
  free_lun,indlun

  openr,datlun,datfile,/get_lun
  data=assoc(datlun,intarr(106))

  if not(keyword_set(wn)) then begin	;true: change microns to cm^-1
    w1mic=w1
    w2mic=w2
    wave1=w1mic*1d4			;change to angstroms
    airtovac,wave1			;change to vacuum wavelength
    wave2=w2mic*1d4			;change to angstroms
    airtovac,wave2			;change to vacuum wavelength
    w2=1d8/wave1
    w1=1d8/wave2
  endif

  wn1=fix(w1)		;integer wavenumbers for evaluating IND
  wn2=fix(w2)
;  wn0=fix(ind_wn(0))		;first wavenumber in file

  w=0			;initialize outputs
  atm=0			
  sol=0

  for i=wn1,wn2 do begin	;loop over wavenumbers desired
    temp=where(ind_wn eq i)	;is desired wavenumber there?
    n_rec=temp(0)
    if n_rec ne -1 then begin	;true: wavenumber is there
      wdata=data(n_rec*3)
      adata=data(n_rec*3+1)
      sdata=data(n_rec*3+2)
      gddata=where(wdata lt 30000)
      wdata=wdata(gddata)/5d4+ind_wn(n_rec)+0.5
      adata=adata(gddata)/5d4+0.5
      sdata=sdata(gddata)/5d4+0.5
      w=[w,wdata]
      atm=[atm,adata]
      thickatm=where(adata lt thresh)
      if thickatm(0) ne -1 then sdata(thickatm)=0
      sol=[sol,sdata]
    endif else begin		;else: wavenumber isn't there
      print,'no data for wavenumber '+strtrim(i,2)
      print,'(wavelength '+strtrim(1d4/i,2)+ ' microns'
    endelse
  endfor
  w=w(1:*)				;trim wavenumbers
  atm=atm(1:*)
  sol=sol(1:*)
  if not(keyword_set(wn)) then begin	;true: user wants microns
    w=1d8/w				;change from wavenumbers to Ang.
    vactoair,w				;convert to air wavelength
    w=w/1d4				;change to microns
    w1=w1mic
    w2=w2mic
  endif
  loc=where(w ge w1 and w le w2)	;extract portion wanted by user
  w=w(loc)
  atm=atm(loc)
  if ysol then sol=sol(loc)
  free_lun,datlun
  s = sol
  if not keyword_set(wn) then begin
    w = reverse(w)
    s = reverse(s)
    atm = reverse(atm)
  endif
end
