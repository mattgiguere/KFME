;This file contains all the routines needed to run bary.pro

function bary_jd,dvec
;Calculate Julian date from UT.
; dvec (input vector(5)) Date and time specified by a 5 element vector of
;   the form [year,month,day,hour,minutes], e.g. [1993,12,25,16,24.2].
;   The year must be fully specified. Only the minutes may be fractional.
;Returns Julian date as a double precision scalar.
; B. Grundseth	Created.
;18-Jun-93 JAV	Translated into IDL. Changed argument list. Check arguments.

if n_params() lt 1 then begin
  print,'syntax: jd = bary_jd([year,month,day,hours,minutes])'
  print,'   e.g. jd = bary_jd([1993,   12, 25,   16,   24.2])'
  retall
endif

;Algorithm relies on integer truncation. (Yuck.)
  iyear = fix(dvec(0))
  imonth = fix(dvec(1))
  iday = fix(dvec(2))
  hours = dvec(3) + dvec(4) / 60d0

;Error checking.
  if (iyear gt 0) and (iyear lt 100) then begin
    message,/info,'Year must be fully specified, e.g. 1993'
    retall
  endif
  if ((iyear mod 1) ne 0) or ((imonth mod 1) ne 0) $
    or ((iday mod 1) ne 0) then begin
    message,/info,'Year, month and day MUST be integral.'
    retall
  endif

;Do it.
  icst = fix(1.2001 - 0.1*imonth)	;1(0)=before(after) possible leap day
  ix = iyear - icst			;modified year for leap calculation
  bary_jd = 365d0*iyear + double(ix/4 - ix/100 + ix/400 + 2*icst + iday) $
        + fix(30.57*imonth) + 1721028d0 - 0.5d0 + hours/24d0
  return,bary_jd

end

pro bary_sidrl,jd,xlong,dtim,ih,min,s
;Calculation of local mean sidereal time from input data.  b.grundseth

if n_params() lt 6 then begin
  print,'syntax: bary_sidrl,jd,xlong,dtim,ih,min,s'
  retall
endif

  jdif = jd - 2415020d0			;subtract J1900
  ru1 = 236.55536049d0 * jdif
  ru2 = 0.00696d0 * jdif*jdif * 1d-8
  ts = (ru1 + ru2 + 23925.836d0) / 3600d0
  ts = ts+dtim*24.d0-12.d0*xlong / !dpi
  if ts lt 0 then ts = ts + 24d0
  ts = ts mod 24d0
  ih = fix(ts)
  f = abs(ts-ih)*60.d0
  min = fix(f)
  g = float(min)
  t = (f-double(g))*60.d0
  s = float(t)
end

function bary_premat,deq1,deq2
;Calculation of the matrix of general precession from deq1 to deq2.
;The precession angles are computed from definitions in the astronomical
;  ephemeris (1961,p.30f) [p.stumpff].
;See also Smart 1985, "Spherical Astromony", p. 220.
;deq1 (input scalar) unprecessed (catalog) Julian date.
;deq2 (input scalar) precessed (observation) Julian date.
;Returns prema (output array(3,3)) precession matrix,

if n_params() lt 2 then begin
  print,'syntax: prema = bary_premat(deq1,deq2)'
  retall
endif

  csar = 4.848136812d-6
  dto = (deq1 - 1900d0) / 100d0
  dt = (deq2 - deq1) / 100d0

  zeta = csar * dt*((2304.25d0  + 1.396d0*dto) + dt*(0.302d0 + dt*0.018d0))
  theta = csar * dt*((2004.683d0 - 0.853d0*dto) - dt*(0.426d0 + dt*0.042d0))
  z = zeta + 0.791d0*csar*dt*dt

  szeta = sin(zeta)
  czeta = cos(zeta)
  sz = sin(z)
  cz = cos(z)
  stheta = sin(theta)
  ctheta = cos(theta)

  a = szeta * sz
  b = czeta * sz
  c = szeta * cz
  d = czeta * cz

  prema = [ d*ctheta - a,    b*ctheta + c,    czeta*stheta $
          ,-c*ctheta - b,   -a*ctheta + d,   -szeta*stheta $
          ,-stheta*cz,      -stheta*sz,       ctheta ]
  return,reform(prema,3,3)
end

pro bary_vl,dje,deq,dvelh,dvelb
;Calculation of heliocentric and barycentric velocity components of
; the earth to an accuracy of < 0.42 m/sec.
;dje (input scalar) julian ephemeris date.
;deq (input scalar) epoch of mean equinox of dvelh and dvelb. If deq=0
;	then deq is assumed to be equal to dje.
;dvelh (output vector(3)) heliocentric velocity component.
;dvelb (output vector(3)) barycentric velocity component.
;Edit History.
;P. Stumpff	Created.
;G. Marcy	Modified.
;17-Jun-93 JAV	Translated to IDL.

if n_params() lt 4 then begin
  print,'syntax: bary_vl,dje,deq,dvelh,dvelb'
  retall
endif

;Define constants
  dc2pi = 6.28318530717960044D0		;2*pi
  cc2pi = 6.283185			;2*pi
  dc1 = 1.0D0
  dcto = 2415020.0D0
  dcjul = 36525.0D0			;days in julian year
  dcbes = 0.313D0
  dctrop = 365.24219572D0		;days in tropical year (...572 insig)
  dc1900 = 1900.0D0

;Constants dcfel(i,k) of fast changing elements.
  dcfel = [1.7400353D00, 6.2833195099091D02,  5.2796D-6 $
          ,6.2565836D00, 6.2830194572674D02, -2.6180D-6 $
          ,4.7199666D00, 8.3997091449254D03, -1.9780D-5 $
          ,1.9636505D-1, 8.4334662911720D03, -5.6044D-5 $
          ,4.1547339D00, 5.2993466764997D01,  5.8845D-6 $
          ,4.6524223D00, 2.1354275911213D01,  5.6797D-6 $
          ,4.2620486D00, 7.5025342197656D00,  5.5317D-6 $
          ,1.4740694D00, 3.8377331909193D00,  5.6093D-6 ]
  dcfel = reform(dcfel,3,8)

;constants dceps and ccsel(i,k) of slowly changing elements.
  dceps = [4.093198D-1, -2.271110D-4, -2.860401D-8 ]
  ccsel = [1.675104E-2, -4.179579E-5, -1.260516E-7 $
          ,2.220221E-1,  2.809917E-2,  1.852532E-5 $
          ,1.589963E00,  3.418075E-2,  1.430200E-5 $
          ,2.994089E00,  2.590824E-2,  4.155840E-6 $
          ,8.155457E-1,  2.486352E-2,  6.836840E-6 $
          ,1.735614E00,  1.763719E-2,  6.370440E-6 $
          ,1.968564E00,  1.524020E-2, -2.517152E-6 $
          ,1.282417E00,  8.703393E-3,  2.289292E-5 $
          ,2.280820E00,  1.918010E-2,  4.484520E-6 $
          ,4.833473E-2,  1.641773E-4, -4.654200E-7 $
          ,5.589232E-2, -3.455092E-4, -7.388560E-7 $
          ,4.634443E-2, -2.658234E-5,  7.757000E-8 $
          ,8.997041E-3,  6.329728E-6, -1.939256E-9 $
          ,2.284178E-2, -9.941590E-5,  6.787400E-8 $
          ,4.350267E-2, -6.839749E-5, -2.714956E-7 $
          ,1.348204E-2,  1.091504E-5,  6.903760E-7 $
          ,3.106570E-2, -1.665665E-4, -1.590188E-7 ]
  ccsel = reform(ccsel,3,17)

;Constants of the arguments of the short-period perturbations.
  dcargs = [5.0974222D0, -7.8604195454652D2 $
           ,3.9584962D0, -5.7533848094674D2 $
           ,1.6338070D0, -1.1506769618935D3 $
           ,2.5487111D0, -3.9302097727326D2 $
           ,4.9255514D0, -5.8849265665348D2 $
           ,1.3363463D0, -5.5076098609303D2 $
           ,1.6072053D0, -5.2237501616674D2 $
           ,1.3629480D0, -1.1790629318198D3 $
           ,5.5657014D0, -1.0977134971135D3 $
           ,5.0708205D0, -1.5774000881978D2 $
           ,3.9318944D0,  5.2963464780000D1 $
           ,4.8989497D0,  3.9809289073258D1 $
           ,1.3097446D0,  7.7540959633708D1 $
           ,3.5147141D0,  7.9618578146517D1 $
           ,3.5413158D0, -5.4868336758022D2 ]
  dcargs = reform(dcargs,2,15)

;Amplitudes ccamps(n,k) of the short-period perturbations.
  ccamps = $
    [-2.279594E-5,  1.407414E-5,  8.273188E-6,  1.340565E-5, -2.490817E-7 $
    ,-3.494537E-5,  2.860401E-7,  1.289448E-7,  1.627237E-5, -1.823138E-7 $
    , 6.593466E-7,  1.322572E-5,  9.258695E-6, -4.674248E-7, -3.646275E-7 $
    , 1.140767E-5, -2.049792E-5, -4.747930E-6, -2.638763E-6, -1.245408E-7 $
    , 9.516893E-6, -2.748894E-6, -1.319381E-6, -4.549908E-6, -1.864821E-7 $
    , 7.310990E-6, -1.924710E-6, -8.772849E-7, -3.334143E-6, -1.745256E-7 $
    ,-2.603449E-6,  7.359472E-6,  3.168357E-6,  1.119056E-6, -1.655307E-7 $
    ,-3.228859E-6,  1.308997E-7,  1.013137E-7,  2.403899E-6, -3.736225E-7 $
    , 3.442177E-7,  2.671323E-6,  1.832858E-6, -2.394688E-7, -3.478444E-7 $
    , 8.702406E-6, -8.421214E-6, -1.372341E-6, -1.455234E-6, -4.998479E-8 $
    ,-1.488378E-6, -1.251789E-5,  5.226868E-7, -2.049301E-7,  0.E0 $
    ,-8.043059E-6, -2.991300E-6,  1.473654E-7, -3.154542E-7,  0.E0 $
    , 3.699128E-6, -3.316126E-6,  2.901257E-7,  3.407826E-7,  0.E0 $
    , 2.550120E-6, -1.241123E-6,  9.901116E-8,  2.210482E-7,  0.E0 $
    ,-6.351059E-7,  2.341650E-6,  1.061492E-6,  2.878231E-7,  0.E0 ]
  ccamps = reform(ccamps,5,15)

;Constants csec3 and ccsec(n,k) of the secular perturbations in longitude.
  ccsec3 = -7.757020E-8
  ccsec = [1.289600E-6, 5.550147E-1, 2.076942E00 $
          ,3.102810E-5, 4.035027E00, 3.525565E-1 $
          ,9.124190E-6, 9.990265E-1, 2.622706E00 $
          ,9.793240E-7, 5.508259E00, 1.559103E01 ]
  ccsec = reform(ccsec,3,4)

;Sidereal rates.
  dcsld = 1.990987D-7			;sidereal rate in longitude
  ccsgd = 1.990969E-7			;sidereal rate in mean anomaly

;Constants used in the calculation of the lunar contribution.
  cckm = 3.122140E-5
  ccmld = 2.661699E-6
  ccfdi = 2.399485E-7

;Constants dcargm(i,k) of the arguments of the perturbations of the motion
; of the moon.
  dcargm = [5.1679830D0,  8.3286911095275D3 $
           ,5.4913150D0, -7.2140632838100D3 $
           ,5.9598530D0,  1.5542754389685D4 ]
  dcargm = reform(dcargm,2,3)

;Amplitudes ccampm(n,k) of the perturbations of the moon.
  ccampm = [ 1.097594E-1, 2.896773E-7, 5.450474E-2,  1.438491E-7 $
           ,-2.223581E-2, 5.083103E-8, 1.002548E-2, -2.291823E-8 $
           , 1.148966E-2, 5.658888E-8, 8.249439E-3,  4.063015E-8 ]
  ccampm = reform(ccampm,4,3)

;ccpamv(k)=a*m*dl,dt (planets), dc1mme=1-mass(earth+moon)
  ccpamv = [8.326827E-11, 1.843484E-11, 1.988712E-12, 1.881276E-12]
  dc1mme = 0.99999696D0

;Time arguments.
  dt = (dje - dcto) / dcjul
  tvec = [1d0, dt, dt*dt]

;Values of all elements for the instant(aneous?) dje.
  temp = (tvec # dcfel) mod dc2pi
  dml = temp(0)
  forbel = temp(1:7)
  g = forbel(0)				;old fortran equivalence

  deps = total(tvec*dceps) mod dc2pi
  sorbel = (tvec # ccsel) mod dc2pi
  e = sorbel(0)				;old fortran equivalence

;Secular perturbations in longitude.
dummy=cos(2.0)
  sn = sin((tvec(0:1) # ccsec(1:2,*)) mod cc2pi)

;Periodic perturbations of the emb (earth-moon barycenter).
  pertl = total(ccsec(0,*) * sn) + dt*ccsec3*sn(2)
  pertld = 0.0
  pertr = 0.0
  pertrd = 0.0
  for k=0,14 do begin
    a = (dcargs(0,k)+dt*dcargs(1,k)) mod dc2pi
    cosa = cos(a)
    sina = sin(a)
    pertl = pertl + ccamps(0,k)*cosa + ccamps(1,k)*sina
    pertr = pertr + ccamps(2,k)*cosa + ccamps(3,k)*sina
    if k lt 11 then begin
      pertld = pertld + (ccamps(1,k)*cosa-ccamps(0,k)*sina)*ccamps(4,k)
      pertrd = pertrd + (ccamps(3,k)*cosa-ccamps(2,k)*sina)*ccamps(4,k)
    endif
  endfor

;Elliptic part of the motion of the emb.
  phi = (e*e/4d0)*(((8d0/e)-e)*sin(g) +5*sin(2*g) +(13/3d0)*e*sin(3*g))
  f = g + phi
  sinf = sin(f)
  cosf = cos(f)
  dpsi = (dc1 - e*e) / (dc1 + e*cosf)
  phid = 2*e*ccsgd*((1 + 1.5*e*e)*cosf + e*(1.25 - 0.5*sinf*sinf))
  psid = ccsgd*e*sinf / sqrt(dc1 - e*e)

;Perturbed heliocentric motion of the emb.
  d1pdro = dc1+pertr
  drd = d1pdro * (psid + dpsi*pertrd)
  drld = d1pdro*dpsi * (dcsld+phid+pertld)
  dtl = (dml + phi + pertl) mod dc2pi
  dsinls = sin(dtl)
  dcosls = cos(dtl)
  dxhd = drd*dcosls - drld*dsinls
  dyhd = drd*dsinls + drld*dcosls

;Influence of eccentricity, evection and variation on the geocentric
; motion of the moon.
  pertl = 0.0
  pertld = 0.0
  pertp = 0.0
  pertpd = 0.0
  for k = 0,2 do begin
    a = (dcargm(0,k) + dt*dcargm(1,k)) mod dc2pi
    sina = sin(a)
    cosa = cos(a)
    pertl = pertl + ccampm(0,k)*sina
    pertld = pertld + ccampm(1,k)*cosa
    pertp = pertp + ccampm(2,k)*cosa
    pertpd = pertpd - ccampm(3,k)*sina
  endfor

;Heliocentric motion of the earth.
  tl = forbel(1) + pertl
  sinlm = sin(tl)
  coslm = cos(tl)
  sigma = cckm / (1.0 + pertp)
  a = sigma*(ccmld + pertld)
  b = sigma*pertpd
  dxhd = dxhd + a*sinlm + b*coslm
  dyhd = dyhd - a*coslm + b*sinlm
  dzhd= -sigma*ccfdi*cos(forbel(2))

;Barycentric motion of the earth.
  dxbd = dxhd*dc1mme
  dybd = dyhd*dc1mme
  dzbd = dzhd*dc1mme
  for k=0,3 do begin
    plon = forbel(k+3)
    pomg = sorbel(k+1)
    pecc = sorbel(k+9)
    tl = (plon + 2.0*pecc*sin(plon-pomg)) mod cc2pi
    dxbd = dxbd + ccpamv(k)*(sin(tl) + pecc*sin(pomg))
    dybd = dybd - ccpamv(k)*(cos(tl) + pecc*cos(pomg))
    dzbd = dzbd - ccpamv(k)*sorbel(k+13)*cos(plon - sorbel(k+5))

  endfor

;Transition to mean equator of date.
  dcosep = cos(deps)
  dsinep = sin(deps)
  dyahd = dcosep*dyhd - dsinep*dzhd
  dzahd = dsinep*dyhd + dcosep*dzhd
  dyabd = dcosep*dybd - dsinep*dzbd
  dzabd = dsinep*dybd + dcosep*dzbd

;Epoch of mean equinox (deq) of zero implies that we should use
; Julian ephemeris date (dje) as epoch of mean equinox.
  if deq eq 0 then begin
    dvelh = [dxhd, dyahd, dzahd]
    dvelb = [dxbd, dyabd, dzabd]
    return
  endif

;General precession from epoch dje to deq.
  deqdat = (dje-dcto-dcbes) / dctrop + dc1900
  prema = bary_premat(deqdat,deq)
  dvelh = prema # [dxhd, dyahd, dzahd]
  dvelb = prema # [dxbd, dyabd, dzabd]
  return
  end

pro bary,dstr,cstr,epoch,vvec,obs=obs,ha=ha,jd=jd
;Calculate barycentric velocity for a particular observing site, time, and
; target. Account for the geocentric motion of the observatory, the helio-
; centric motion of the earth, and the barycentric motion of the sun.
;
;dstr (input string) UT date and time of the observation, expressed as a
; string of the form:  'year month date hours minutes', e.g.
; '1992 11 20 1 20.0'. The minutes may be nonintegral, but all other
; arguments MUST be integral (though they may be expressed as a decimal).
; Arguments may be separated by an arbitrary number of spaces and prefixed
; by zeros.
;
;cstr (input string) Coordinates of object being observed, expressed as
; a string of the form:  'hours minutes seconds [sign]degrees minutes seconds',
; where the first 3 terms give the RA and the second 3 terms give the objects
; declination, e.g. '0 6 36.7  +29 1 17'. All terms may be nonintegral,
; expressed as an integeger or a decimal, separated by an arbitrary number of
; spaces, and be prefixed by zeros. If a negative sign appears anywhere in the
; coordinate string, the declination is assumed to be negative.
;
;epoch (input scalar) Julian epoch of the coordinates, usually 2000. Note that
; B1950 ocoordinates have Julian epoch 1949.999789
;
;vvec (output vector(4)) Various projections of the barycentric velocity
; correction, expressed in km/sec. The four elements in the vector are radial,
; tangential, right ascension, and declination projections respectively.
; Add vvec(0) to the observed velocity scale to shift it to the barycenter.
;
;obs= (input string) Observatory designaton. Currently 'L3' is the 3m at Lick,
; 'CFHT' is the CFHT, and 'KP' is Kitt Peak. Lick 3m is assumed if no
; observatory is specified.
;
;ha= (output scalar) the hour angle of the observation expressed in radians.
;
;jd= (output scalar) Julian date of observation.
;
;Known limitations:
; Need to use special relativity when adding velocities (2 m/s).
;
;Edit History:
;MMDR Created.
;1987 Geoff Marcy Modified.
;Spring-90  KJB	Modified to allow passage of data to/from ANA.
;   Jun-90  JB  Unknown modifications?
;22-Feb-92  EW  Added variable 'obtype' to accomodate the designation of 
; of template of observation to each file.
;22-Jun-93 JAV  Translated to IDL. Info passed by argument rather than file.
; Validated translation to 17 (in)significant figures against fortran version
;  for test case: '1992 11 20 1 20.0', '0 6 36.7 +29 1 17', 2000, giving a
;  radial velocity of -18915.78728 m/s.
; Corrected 3 minor bugs: [1] epoch of observation was determined by crude
;  formula. [2] declinations between -1 0 0 and 0 0 0 were treated as positive,
;  [3] seconds of declination were ignored and set to zero. Also improved
;  internal numerical accuracy of code giving a radial velocity of -18915.80110
;  m/s for above test case.
; Compared with pulsar pulse arrival time code, "tempo", which gives a radial
;  velocity of -18914.33900 m/s for the test case.
;23-Nov-93 JAV	Fixed error in RA component of radial velocity (changed
; "dv(1)*cdec" to "dv(1)*cra").
;04-Dec-93 JAV	Added "jd" as a return argument.
;30-Aug-00 JAV	If "dstr" is not a string, it is interpreted as the Julian
;		 date at the midpoint of the observation.

if n_params() lt 4 then begin
  print,'syntax: bary,dstr,cstr,epoch,vvec [,obs=,ha=,jd=]'
  print,'         where vvec is [vrad, vtan, vra, vdec] (in km/s)'
  print,'         and default observing site is Lick 3m.'
  print,"  e.g.: bary, '1992 11 29 16 46.2', '0 6 36.7 +29 1 17', 2000, vvec"
  print,"  e.g.: bary, 2400000.5d0 + 51651.207d0, '0 6 36.7 +29 1 17'" $
       +", 2000, vvec"
  retall
endif

;Constants
  daum = 1.4959787d8
  dlite = 2.99792458d5
  jeq = 0d0
  c1 = 2d0 * !dpi
  c2 = 3.8197187

;Get the observatory longitude and latitude (in radians) and height (in m).
  if n_elements(obs) eq 0 then obs = 'L3'	;default site is Lick 3-m
  ht = -1					;flag: no observatory yet
  obslc = strlowcase(obs)			;force lowercase
  if obslc eq 'l3' then begin			;Lick 3-m
    lat = 0.651734547d0				; +37 20 29.9
    lon = 2.123019229d0				; 121 38 24.15
    ht = 1283.
  endif
  if obslc eq 'cfht' then begin			;CFHT
    lat = 0.346030917d0				; +19 49 34
    lon = 2.713492477d0				; 155 28 18
    ht = 4198.
  endif
  if obslc eq 'kp' then begin			;Kitt Peak
    lat = 0.557865407d0				; +31 57.8 (1991 Almanac)
    lon = 1.947787445d0				; 111 36.0
    ht = 2120.
  endif
  if obslc eq 'mcd' then begin			;McDonald Observatory
    lat = 0.535321576d0				; +30 40 18.0
    lon = 1.815520577d0				; 104 01 18.0
    ht = 2075.
  endif
  if obslc eq 'vlt' then begin			;Very Large Telescope
    lat = -0.429838d0
    lon = -1.22880d0
    ht = 2681.
  endif
  if ht eq -1 then begin			;unknown observatory
    print,'bary: Unknown observatory designation.'
    retall
  endif

;Parse the input coordinate string, returning RA and Dec in radians.
  rav = dblarr(3)			;initialize RA vector
  decv = dblarr(3)			;initialize dec vector
  reads,cstr,rav,decv			;read hour,min,sec,deg,mn,sec into vecs
  ra = !dtor * 15d0 * (rav(0) + rav(1)/60d0 + rav(2)/3600d0)
  dsign = 1d0				;assume positive declination
  if strpos(cstr,'-') gt -1 then dsign = -1d0	;true: negative declination
  decv = abs(decv)			;just worry about magnitude of dec
  dec = dsign * !dtor * (decv(0) + decv(1)/60d0 + decv(2)/3600d0)

;Convert the date vector to a Julian date.
  sz = size(dstr)
  if sz(sz(0)+1) eq 7 then begin	;true: original date string
    datv = fltarr(5)			;initialize date vector
    reads,dstr,datv			;read yr,mn,dy,hr,mn into datv
    jd = bary_jd(datv)			;calculate Julian date
    tim = datv(3)/24d0 + datv(4)/1440d0
  endif else begin			;else: interpret as JD
    jd = dstr
    tim = (jd+0.5d0) mod 1d0		;fractional day
  endelse
  
;Empirical calculation of rho*cos(phi) and vrot.
  tlat = tan(lat)
  rcp = (1.0 + 1.57d-7*ht) / sqrt(1.0 + 0.993305d0 * tlat*tlat)
  vrot = 4.65102d2 * rcp

;Calculate barycentric velocity of earth.
  bary_sidrl,jd,lon,tim,h,m,s
  bary_vl,jd,jeq,velh,velb

;Calculation of geocentric velocity of observatory.
  lmst = double((h+(m+s/60.)/60.)/c2)
  ha = lmst - ra
  velt = vrot * [-sin(lmst), cos(lmst), 0d0]

;Calculation of barycentric velocity components.
  dv = velb * daum + velt*1.d-3
  sra = sin(ra)
  sdec = sin(dec)
  cra = cos(ra)
  cdec = cos(dec)

  if epoch eq 0 then begin		;0 -> use current ra, dec
    ihrap = ihra
    imrap = imra
    srap = sra
    iddp = idd
    idmp = idm
    sdnp = sdn
  endif else begin

;Precession of 'epoch' co-ordinates to 'edeq' co-ordinates.
    epobs = 2000d0 + (jd-2451545d0) / 365.25d0	;"epoch" of observation
;
;   dedeq = double(datv(0)+((datv(1)-1)+datv(2)/30.0)/12.0)
;
    prema = bary_premat(epoch,epobs)
    dcor = [cra*cdec, sra*cdec, sdec]
    dcorp = prema # dcor
    xdecp = asin(float(dcorp(2)))
    dec = double(xdecp)
    xrap = 2.0 * atan(float(dcorp(1)/(dcorp(0) + cos(xdecp)))) + c1
    xrap = xrap mod c1
    ra = double(xrap)
    sra = sin(ra)
    sdec = sin(dec)
    cra = cos(ra)
    cdec = cos(dec)
  endelse

;Calculation of relevant velocities.
  dvr = dv(0)*cra*cdec + dv(1)*sra*cdec + dv(2)*sdec
  dva = -dv(0)*sra + dv(1)*cra
  dvd = -dv(0)*cra*sdec - dv(1)*sra*sdec + dv(2)*cdec
  dvt = sqrt(dva*dva + dvd*dvd)

;"Wavelength shift of standard line" - move outside of routine.
; dratio = sqrt(1.d0 - (dvr*dvr + dvt*dvt) / (dlite*dlite)) $
;	 / (1.d0 + dvr/dlite)

  vvec = [dvr, dvt, dva, dvd]			;return velocities

end
