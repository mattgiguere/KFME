;***********************************************************************
;+
; NAME: TRANSIT	   
;																	   
; PURPOSE: TO PREDICT WHEN A KNOWN EXOPLANET WILL TRANSIT ITS
;		HOST STAR.
;																	   
; CATEGORY: EXOPLANETS							   
;																	   
; CALLING SEQUENCE:													   
;																	   
;																	   
; :PARAMS:															   
;	VST: in, optional, type="string"
;     The velocity structure where the data is restored from
;
;	SPEC: in, optional, type="string"
;     The spectral type of the star used to approximate the
;		mass and radius of the star. This can be found through
;		an online search engine such as SIMBAD.
;
; OPTIONAL INPUTS:													   
;																	   
; :KEYWORDS:	
;
;  BANK: in, optional, type="boolean"
;     Restore the velocity structure (vst) from the '/mir3/vstbank'
;     directory rather than the '/mir3/vel' directory. 
;
;	CURRENT: Use this keyword if you want the ephemeris to be printed
;		out starting at the current date instead of at the one set for
;		that particular object. 
;
;  NOKECK: Use this keyword if NOT using data from keck
;
;	VST: This keyword is used by the procedure to run rv_fits and come
;		up with the necessary orbital elements. There are a handful of
;		stars that have been run so many times that all of the orbital 
;		elements are in a database, and the operation of rv_fits is not
;		necessary. If the star is not already in the database, use of 
;		this keyword is necessary. To access the database, use the STAR
;		keyword. 
;
;	MONTE_P: Use this keyword if you want to do a monte_carlo fit to 
;		find out what the error is in the transit prediction. this 
;		will house the randomized period. 
;
;	MONTE_Tp: This variable will house the Time of periastron that has
;		a random error associated with it. This is used to calculate the
;		error for the prediction of transit. 
;
;	MONTE_NEW: This array will be where all of the time of 
;		central transits generated with the random error from MONTE_
;		TRANSIT will be stored. 
;
;	NO_TRANSIT_PLOT: Use this keyword if you don't want any plots to be
;		produced. This is a good keyword to use when running the monte
;		carlo simulations using TRANSIT_MONTE. 
;
;  RoMac: in, optional, type="boolean"
;     This keyword will calculate the transit, then remove observations
;     taken during transit, refit with rv_fit, recalculate the transit, 
;     phase-fold the data and reinsert the observations to look for 
;     transiting planets using the Rossiter-MacLaughlin Effect.
;
;	SPEC: Use this keyword to tell the procedure the spectral type and 
;		luminosity class of the star. Like, VST, the only time this
;		keyword is optional is if the star's spectral type is already in
;		the STAR database and the STAR keyword is invoked. 
;
;	STAR: Use this keyword if the spectral type and orbital parameters
;		are already known and listed in the STAR database. To use this
;		keyword, type "star = 'HD17156' " or 'GJ' in place of the HD 
;		at the beginning of the star name if the star is listed in that 
;		catalog. 
;
;	TmoTr: The Time of Transit you want to obtain. This program will 
;		add orbital periods to the calculated time from the given data
;		until it obtains the NEXT transit after the date you specify.
;
;	Trend: This keyword is passed on to rv_fit_func. Use it if there is
; 		a trend in the data. 
;
;	CIRC: This keyword is used by the rv_fit_func to constrain the orbit
;		to a circle. This is used when testing the procedure to make 
;		sure it outputs the same thing as rv_fit, /ephem
;
;	FRZ_PER: If there aren't enough data points to find the orbital 
;		parameters with rv_fit_func, this will constrain the period
;		to whatever this is set to. Ex: transit, ...., frz_per = 13.9
;
;	FRZ_K: This keyword can be used just like frz_per to constrain
;		one of the free parameters in rv_fit in case there aren't enough 
;		data points.
;
;	FRZ_ECC: This freezes the eccentricity in the same manner as frz_per
;		and frz_k for their respective parameters. 
;																	   
; OUTPUTS:															   
;																	   
; OPTIONAL OUTPUTS:													   
;																	   
; SIDE EFFECTS:														   
;																	   
; RESTRICTIONS:														   
;																	   
; PROCEDURE:														   
;																	   
; EXAMPLE:			
;	transit, vst = 'vst5319.dat', star = 'HD5319'
;	transit, vst = 'vst17156.dat', star = 'HD17156', /circ
;
;  transit, star='HD179079', vst = 'vst179079.dat', /bank, /RoMac
;
; And using published parameters instead of data:
;  transit, star = '55cncb'
;	 																   
; MODIFICATION HISTORY:												   
;     c. Matt Giguere, Monday, July 20th, 2007						   
;-
;***********************************************************************
;NOTE: 
;MAX (POSITIVE) STELLAR VELOCITY CORRESPONDS TO THE STAR
;	RECEDING.
;MIN (NEGATIVE) STELLAR VELOCITY CORRESPONDS TO THE STAR APPROACHING
;0 IS PRIMARY AND SECONDARY TRANSITS

pro transit, $
a1sini = a1sini, $
arel = arel, $
cf3 = cf3, $
circ = circ, $
current = current, $
debug = debug, $
ephem = ephem, $
frz_ecc = frz_ecc, $
frz_k = frz_k, $
frz_per = frz_per, $
knownstarrad = knownstarrad, $
monte_e = monte_e, $
monte_k = monte_k, $
monte_p = monte_p, $
monte_om = monte_om, $
monte_gam = monte_gam, $
monte_dvdt = monte_dvdt, $
monte_tcen = monte_tcen, $
monte_ting = monte_ting, $
monte_tegr = monte_tegr, $
monte_stcen = monte_stcen, $
monte_sting = monte_sting, $
monte_stegr = monte_stegr, $
monte_Tp = monte_Tp, $
msini = msini, $
mstar = mstar, $
nobank = nobank, $
no_transit_plot = no_transit_plot, $
nokeck = nokeck, $
orbel = orbel, $
plidx = plidx, $
prob_t = prob_t, $
prob_st = prob_st, $
romac = romac, $
spec = spec, $
star = star, $
stddvtrans_no = stddvtrans_no, $
time_stamp = time_stamp, $
TmoTr = TmoTr, $
trend = trend, $
vst = vst, $
vsini = vsini, $
use_monte_ting = use_monte_ting, $
use_monte_tegr = use_monte_tegr, $
uvsini = uvsini

if keyword_set(frz_per) then begin
print, 'now inside of TRANSIT.PRO line 163'
print, 'frz_ecc is: ', frz_ecc
print, 'frz_per is: ', frz_per
print, 'TRANSIT.PRO line 166'
endif

;CONSTANTS:

;Newton's Gravitational Constant:
G = 6.67d*10.d^(-11.d)
;Convert from meters to Astronomical Units:
AU = 1.49598d11		;in meters
;Mass of the Sun:
msun = 1.9891d30	;in kg
;Mass of Jupiter
mjup = 1.8987d27	;in kg
;Mass of Jupiter to Earth Mass:
mjup2earth = 317.816611d
;Radius of the Sun:
rsun = 6.955d8		;in meters

;Initial setting of parameters:
tim2calc = systime(/julian)
TmOIng = 0.d
TmOEgr = 0.d
p = 0.d
inc = 90.d
init_om = 0.d
rsp = 'y'
if ~keyword_set(knownstarrad) then knownStarRad = 0.
if ~keyword_set(circ) then circ = 0.d

!p.charsize = 1.25
!p.charthick = 1.
 charsize=!p.charsize
 !x.charsize = !p.charsize
 ;!x.charsize = 2.0
 !y.charsize = !p.charsize
 ;!y.charsize = 2.
 !p.thick=0.5*!p.charsize
 !x.thick=0.5*!p.charsize
 !y.thick=0.5*!p.charsize


;***********************************************************************
;		GATHER INFORMATION ABOUT THE STAR...
;***********************************************************************
if 0 then begin ;keyword_set(star)
  stardat = stardat(star)
  p = stardat.p
  spec = stardat.spec
  a = stardat.a
  e = stardat.e
  if ~keyword_set(frz_ecc) then frz_ecc = stardat.frz_ecc
  if ~keyword_set(frz_per) then frz_per = stardat.frz_per
  if ~keyword_set(frz_k) then frz_k = stardat.frz_k
  inc = stardat.inc
  K = stardat.K
  msini = stardat.msini
  om = stardat.om
  TmoEgr = stardat.TmoEgr
  TmoIng = stardat.TmoIng
  Tc = stardat.Tc
  Tp = stardat.Tp
  TrKnown = stardat.TrKnown
  knownStarRad = stardat.knownStarRad
  tim2calc = stardat.tim2calc
  vsini = stardat.vsini
  uvsini = stardat.uvsini
endif ;KW:star


if keyword_set(monte_p) then begin
  p = monte_p
  Tp = monte_tp
  e = monte_e
  om = monte_om
  k = monte_k
  gam = monte_gam
  dvdt = monte_dvdt
endif ;KW: monte_p


;begin statements to find mass and radius:
if knownStarRad eq 0 then begin
	StarRad = starRadDat(spec)
endif else StarRad = knownStarRad ;knownStarRad

print, 'The Stellar Radius is: ', StarRad

;dir='/mir3/vel/'
;if keyword_set(nokeck) then dir='/mir1/vel/'

;if keyword_set(vst) then begin
;  if n_elements(cf3) eq 0 then restore, dir+vst
;  ;restore, '/mir3/vstbank/vsthip85977_6obs.dat'
;  print, 'the file you restored was: ', dir+vst
;  ;stop
;  ;x = where( strmid(cf3.obnm, 0, 2) eq 'rj')
;  ;cf3 = cf3[x]
;  if n_elements(cf3) lt 0 then cf3=cfresid
;  pergram,cf3
;  
;  if star eq 'HD179079' then cf3.errvel = sqrt(cf3.errvel^2 + 3.5^2)
;  ;rv_fit fits a keplerian model to the cf3 velocities.  
;  ;  Check out the keywords. You need a good guess the 
;  ;  period - get this from pergram
;  
;  
;  if p eq 0 then read, 'Best guess for period: ', p
;
;  print, 'RV_FIT_FUNC.PRO WILL be run, and the variables that will be used are:'
;  print, 'cf3'
;  print, 'per: ', p
;  print, 'circ: ', circ
;  if keyword_set(frz_per) then print, 'frz_per: ', frz_per
;  if keyword_set(frz_ecc) then print, 'frz_ecc: ', frz_ecc
;
;  print, 'init_om: ', init_om
;
;  if keyword_set(manTp) then print, 'manTp: ', manTp
;  if keyword_set(trend) then print, 'trend: ', trend
;
;  ;read, 'Do you want to change any of these values? (y/n): ', rsp
;  rsp = 'n'
;  
;  
;  ;added 2.2 m/s jitter as requested by Debra for 7924 20080206:
;  ;cf3.errvel = sqrt(cf3.errvel^2 + 2.2^2) 
;  
;  print, 'the minmax of the velocities are: ', minmax(cf3.mnvel)
;  
;  print, 'frz_ecc is: ', frz_ecc
;  print, 'frz_per is: ', frz_per
;  if keyword_set(debug) then stop
;  
;print, 'YOU NEED TO CHANGE THIS TO USE RV_LIN!!!'
;stop
;  rvfits = rv_fit_func(cf3, $
;  period=p, frz_per = frz_per, $
;  star=strmid(vst,3,strlen(vst)-7), $
;  thfit=1,frz_ecc = frz_ecc, $
;  tim2calc = tc, /phase, trend = trend, charsize = 1.5, $
;  nobank = nobank, keck = ~n_elements(nokeck), $
;  debug = debug)
;  
;  if keyword_set(debug) then stop
;  
;  ;This is the parameter array created in rv_fit. For a detailed 
;  ; description of what each element does, see below.
;  pars = rvfits.par
;  
;  print, 'pars are: ', pars
;  
;  
;  ;This is the theoretical velocity curve created in rv_fit with 
;  ; the associated time array:
;  vstar = rvfits.vstar
;  vtimes = rvfits.vtimes
;  
;  ;The number of observations made of that star:
;  nobs = rvfits.nobs
;  loadct, 5
;  
;  window, 2
;  
;  !p.charsize = 1.25
;  !p.charthick = 1.
;	charsize=!p.charsize
;	!x.charsize = !p.charsize
;	;!x.charsize = 2.0
;	!y.charsize = !p.charsize
;	;!y.charsize = 2.
;	!p.thick=0.5*!p.charsize
;	!x.thick=0.5*!p.charsize
;	!y.thick=0.5*!p.charsize
;  
;  plot, vtimes, vstar, background = 255
;  
;  ;This will return:
;  ;p: the period in days
;  p = pars[0]
;  
;  ;Tp: the time of periastron (when the planet crosses the periastron
;  Tp = 40000.d + pars[1]+ 2.4d6
;  print, 'right after rv_fits'
;  print, Tp
;  ;print, jul2cal(Tp)
;  ;stop
;  
;  ;e: the eccentricity of the planet
;  e = pars[2]
;  
;  ;om: Longitude of periastron; This is the angle measured from the line
;  ;of nodes to the periastron in the direction of the planetary oribt:
;  om = pars[3]
;  
;  ;k: the maximum velocity/ velocity amplitude (in meters/sec)
;  k = pars[4]
;  
;  ;gam: the lienar trend or constant velocity of the center of mass 
;  ; system (meters/sec)
;  gam = pars[5]
;  
;  ;dvdt: 
;  dvdt =  pars[6]
;  
;endif; KW: vst
;

;psec: the period in days
psec = p * 24.d * 3600.d

;a1sini: the upper boundary of the semimajor axis of the planet
a1sini = k*psec*sqrt(1.d - e^2.d)/(2.d * !pi)   ;in meters

;The longitude of the ascending node doesn't matter for radial
;	velocity or transit detection methods, only astrometric:
big_om = 0.d


;The mass function. See Carroll & Ostlie's Astrophysics Ch. 7 p. 188,
;Bowers & Deeming p. 317, my transit_procedure.pdf describing this
;procedure, or rv_fit.pro for more information
massfn = 4.d * !pi^2.d * a1sini^3.d/(G*psec^2.d)/1.989d30 ;in Msun

print, 'the mass function is: ', massfn

;read,'Give Mass of the Star (Solar Masses):',mstar
if not keyword_set(mstar) then mstar = 1.0d

print, 'mstar is: ', mstar

;Iterate to find m_2 sin i
;MASSFN defined:  massfn = [m_2/(m_star+m_2)]^2 * m_2 sini^3
;Thus expression for msini depends on m_2:
; msini = [massfn * (mstar + m_2)^2 ]^1/3
;First guess is the standard one, for m_2 << mstar:
msini = (massfn * mstar^2.d)^(1.d / 3.d)  ;in Solar masses, first estimate

;Now iterate twice, using msini for m_2:
msini = (massfn * (mstar+msini)^2.d)^(1.d/3.d) ;in Solar masses
msini = (massfn * (mstar+msini)^2.d)^(1.d/3.d) ;in Solar masses

mstar *= msun
msini *= msun

StarRad *= rsun

;now put it in 

;The standard gravitational parameter
;http://en.wikipedia.org/wiki/Vis-viva_equation
mu = G*(Mstar + msini)

;Kepler's Laws:
;1st (planets orbit in an ellipse):
;num = a*(1.-e^2.)
;den = G*M*(1+e*cos(theta))
;r = num/den
;2nd: (constant area swept out over time)

;Calculating the orbit of the planet about the center of mass:
a2sini = a1sini*( (Mstar + msini)/msini - 1.d)

;calculating the orbit of the planet about the central star at 
;  a focus:
;print, 'a1 is: ', a1sini
;print, 'which in AU is: ', a1sini/AU
;print, 'a2 is: ', a2sini
;print, 'which in AU is: ', a2sini/AU

;calculate the relative semimajor axis as though the star were at the origin:
arel = a2sini + a1sini

;Which is exactly the same as printed in 
; Celestial Mechanics by Laurence Taff 1985 p. 477:
; a = a1sini*(Mstar + msini)/msini

;print, '---------------------------------------------'
;print, 'Factors of interest in TRANSIT.PRO'
;print, '---------------------------------------------'

;print, 'a1 is: ', a1sini
;print, 'which in AU is: ', a1sini/AU
;print, 'a2 is: ', a2sini
;print, 'which in AU is: ', a2sini/AU
;print, 'arel is: ', arel
print, 'arel in AU is: ', arel/AU
print, 'e is: ', e
print, 'mstar is: ', mstar
;print, 'mu is: ', mu
print, 'msini is: ', msini
print, 'which in Jupiter masses is: ', msini/mjup

;***********************************************************************
;		PLOT THEORETICAL VELOCITY CURVES
;***********************************************************************
;Think about a way to check that the Tc that is plotted here is 
;within the range between Ingress and Egress after the orbit is rotated. 

if (keyword_set(Tc) and Tp eq 0) then tte = Tc - .25d*p else tte=Tp
tpplot = Tp
while Tpplot lt tim2calc do tpplot += p
while Tpplot gt tim2calc do tpplot -= p
mypars = [p, tte, e, om, k, gam, dvdt]


;print, 'starting TRANSIT_THVEL IN TRANSIT.PRO @ ', SYSTIME()
transit_thvel, mypars, mytfine, tpplot, tim2calc, $
no_transit_plot = no_transit_plot
;print, 'finished TRANSIT_THVEL IN TRANSIT.PRO @ ', SYSTIME()


;***********************************************************************
;		CALCULATE MEAN ANOMALY TO DETERMINE TING, TCEN AND TEGR
;***********************************************************************

!p.charsize = 1.25
 charsize=!p.charsize
 !x.charsize = !p.charsize
 ;!x.charsize = 2.0
 !y.charsize = !p.charsize
 ;!y.charsize = 2.
 !p.thick=0.5*!p.charsize
 !x.thick=0.5*!p.charsize
 !y.thick=0.5*!p.charsize

print, '---------------------------------------------------------'
print, '  PARS BEFORE TRANSIT_MEAN_ANOM in TRANSIT.PRO:  '
print, '---------------------------------------------------------'
print, 'p is: ', p
print, 'tp is: ', tp
print, 'k is: ', k
print, 'om is: ', om
print, 'e is: ', e
print, 'gam is: ', gam
print, 'dvdt is: ', dvdt
print, '---------------------------------------------------------'


transit_mean_anom, arel, big_om, inc, e, m1sini, m2sini, om, nu, $
xnew, ynew, znew, nel = n_elements(mytfine), no_ellipse2sky = $
no_transit_plot, StarRad = StarRad, $
rcen, ring, regr, mcen, ming, megr, $
srcen, sring, sregr, smcen, sming, smegr, starnm = star, $
time_stamp = time_stamp, plot_transit = romac, $
pscolor=romac, psbw=romac

;if keyword_set(romac) then stop

print, '---------------------------------------------------------'
print, ' PARS AFTER TRANSIT_MEAN_ANOM in TRANSIT.PRO:  '
print, '---------------------------------------------------------'
print, 'p is: ', p
print, 'tp is: ', tp
print, 'k is: ', k
print, 'om is: ', om
print, 'e is: ', e
print, 'gam is: ', gam
print, 'dvdt is: ', dvdt
print, '---------------------------------------------------------'

;***********************************************************************
;		COMPUTE TRANSIT PROBABILITY
;***********************************************************************
;For Primary
transit_prob, ring, rcen, regr, StarRad, knownstarrad, prob_t

;For Secondary
transit_prob, sring, srcen, sregr, StarRad, knownstarrad, prob_st

;***********************************************************************
;		DETERMINE TRANSIT TIMES
;***********************************************************************
;Bring the time of periastron up to date so that it calculates the 
;	next transit:

;stop

transit_time2calc, p, tc, tm2clc, tim2calc=tim2calc, tmoing=tmoing, $
tmoegr = tmoegr, tp = tp

;create Time of Periastron given Time of Center Transit:
if (keyword_set(Tc) and (e eq 0) and (Tp eq 0) ) then Tp = Tc - .25d * p

;If the tp is modified to be mission the 2.44d6 out front:
If Tp lt 4d4 then Tm2Clc -= 2.44d6

;Increment the Tp to be up to par with the Tm2Clc:
nperiods = floor( (Tm2Clc - Tp)/p, /L64)
Tp += nperiods*p


;The Primary Transit:
TIng = double(Tp) + Ming/(2.d * !pi)*p
TCen = double(Tp) + Mcen/(2.d * !pi)*p
TEgr = double(Tp) + Megr/(2.d * !pi)*p

;The Secondary Transit:
STIng = double(Tp) + SMing/(2.d * !pi)*p
STCen = double(Tp) + SMcen/(2.d * !pi)*p
STEgr = double(Tp) + SMegr/(2.d * !pi)*p

print, 'primary ingress: ', jul2cal(ting + 2.44d6)
print, 'primary egress: ', jul2cal(tegr + 2.44d6)
print, 'primary difference: ', tegr - ting
;print, 'secondary ingress: ', jul2cal(sting + 2.44d6)
;print, 'secondary egress: ', jul2cal(stegr + 2.44d6)
;print, 'difference: ', stegr - sting



;increase primary egress by one period if it's less than the ingress:
temp_ting = (ting lt tegr)*ting + (ting gt tegr)*tegr
tegr = (ting gt tegr)*ting + (ting lt tegr)*tegr
ting = temp_ting

;increase secondary egress by one period if it's less than the ingress:
temp_sting = (sting lt stegr)*sting + (sting gt stegr)*stegr
stegr = (sting gt stegr)*sting + (sting lt stegr)*stegr
sting = temp_sting

print, 'primary ingress: ', jul2cal(ting + 2.44d6)
print, 'primary egress: ', jul2cal(tegr + 2.44d6)
print, 'primary difference: ', tegr - ting
;print, 'secondary ingress: ', jul2cal(sting + 2.44d6)
;print, 'secondary egress: ', jul2cal(stegr + 2.44d6)
;print, 'difference: ', stegr - sting

print, '---------------------------------------------------------'

print, 'PRIMARY INGRESS: ', JUL2CAL(ting + 2.44d6)
print, 'PRIMARY TRANSIT CENTER: ', strt(floor(tcen)), $
        strt(tcen - double(floor(tcen)))     
print, 'IN CAL: ', jul2cal(tcen + 2.44d6)
print, 'PRIMARY EGRESS: ', JUL2CAL(tegr + 2.44d6)
print, '---------------------------------------------------------'


;***********************************************************************
;		ROSSITER-MACLAUGHLIN EFFECT
;***********************************************************************
if keyword_set(RoMac) then begin
print, '---------------------------------------------------------'
print, '              PARS BEFORE TRANSIT_RM:  '
print, '---------------------------------------------------------'
print, 'p is: ', p
print, 'tp is: ', tp
print, 'k is: ', k
print, 'om is: ', om
print, 'e is: ', e
print, 'gam is: ', gam
print, 'dvdt is: ', dvdt
print, '---------------------------------------------------------'

print, 'frz_ecc is: ', frz_ecc
print, 'frz_per is: ', frz_per
if keyword_set(debug) then stop

;restore velocity structure if not already open:
if n_elements(cf3) lt 1 then begin
  starnum = strmid(star, 2, strlen(star))
  
  xst = file_info('/mir1/vel/vst'+starnum+'.dat')
  if xst.exists then restore, '/mir1/vel/vst'+starnum+'.dat'
  
  xst = file_info('/mir3/vel/vst'+starnum+'.dat')
  if xst.exists then restore, '/mir3/vel/vst'+starnum+'.dat'
  
  xst = file_info('/mir3/vstbank/vst'+starnum+'.dat')
  if xst.exists then restore, '/mir3/vstbank/vst'+starnum+'.dat'
endif;opening velocity structure

if keyword_set(use_monte_ting) then ting=use_monte_ting
if keyword_set(use_monte_tegr) then tegr=use_monte_tegr

;stop
pars = transit_rm(p, tp, om, tim2calc, ting, tegr, cf3, vst, $
mstar = mstar, $
orbel = orbel, $
plidx = plidx, $
trend = trend, $
star = star, $
frz_ecc = frz_ecc, $
frz_k = frz_k, $
frz_per = frz_per, $
debug = debug, $
time_stamp = time_stamp)

p = pars[0]
Tp = pars[1]
e = pars[2]
om = pars[3]
k = pars[4]
gam = pars[5]
dvdt =  pars[6]
stddvtrans_no =  pars[7]
;stop
endif ;KW(RoMac)

if keyword_set(TmoIng) then begin
 print, 'my calculated ingress is ', $
strt(24.d*60.d*(tmoing - ting)), ' min before the observed ', $
'transit ingress.' 

 
 print, 'my calculated egress is ', $
 strt(24.d*60.d*(tegr - tmoegr)), ' min after the observed ', $
'transit egress.' 
endif

if 1 eq 0 then begin
   if keyword_set(vst) then begin
   jdvtimes = vtimes
   ct=0
   rng=7.d
   while ct le 0 do begin 
   x=where( (jdvtimes ge (Tm2Clc - rng)) AND $;
		   (jdvtimes le (Tm2Clc + rng) ), ct)
   rng *=7
   endwhile
   njds = n_elements(vtimes)
   jdvtimes = vtimes
   dum = label_date(date_format=['%D %H:%I', '%Y.%M'])
   plot, jdvtimes[x], vstar[x], xtickunits = ['Time', 'Time'], $
   xtickformat='LABEL_DATE', XSTYLE=1, xticks=8, $
   TITLE = ' Theoretical Vstar vs Jdvtimes Plot', color = 0, back = 255
   oplot, [ting, tegr], [0.,0.], color = 0
   plots, minmax(jdvtimes), [0.,0.], color = 0
   plots, [tim2calc, tim2calc], [-10,10], color = 0
   endif; KW: vst (plot)
endif ;1=0


;now to get the planet into Jupiter masses:
msini /= mjup

if keyword_set(star) then extitle = star $
else extitle = strupcase(strmid(vst, 3, strlen(vst)-4))

if ~keyword_set(no_transit_plot) then begin
  if ~keyword_set(transit_stdev) then begin
	transit_stdev = .125d
	transit_st_stdev = .125d
	tdur_unc = 0.125d
	tdur_st_unc = 0.125d
	stdevwarn = 1
  endif
  
  m_NEPT = 1.0244d26 ;kg
  r_NEPT = 2.4764d7 ;m
  rho_NEPT = m_nept / (4d / 3d * !dpi * r_NEPT^3)
  
  m_jup = 1.8987d27 ;kg
  r_jup = 7.1492d7
  rho_jup = m_jup/ (4d / 3d * !dpi * r_jup^3)
  
  m_earth = 5.98d24 ;kg
  r_earth = 6.378d6
  rho_earth = m_earth/ (4d / 3d * !dpi * r_earth^3)
  
  rho_pl = (msini lt 8d * m_earth)*rho_earth + $
		   ((msini gt 8d*m_earth) AND (msini lt 0.5*m_jup))*rho_nept + $
		   (msini ge m_jup)*rho_jup
  
  r_sun = 6.95d8 ;m
  albedo = 0.35d
  
  r_planet = (3d * msini*m_jup / (4d * !dpi * rho_pl) )^(1d / 3d)
  tdepth = r_planet^2 / (knownstarrad*r_sun)^2
  stdepth = albedo*r_planet^2 / ((knownstarrad*r_sun)^2 + albedo*r_planet^2)
  
  rmamp = 0d
  
  transit_print, p, Tp, e, om, k, $
  prob_t, $
  prob_st, $
  Tcen, $
  STcen, $
  chi = chi, $
  depthmmag = depthmmag, $
  depth_st_mmag = depth_st_mmag, $
  e_unc = e_unc, $
  extitle = star, $
  k_unc = k_unc, $
  lastob = jul2cal(max(cf3.jd) + 2.44d6, /date), $
  msini = msini, $
  nobs = strt(n_elements(cf3.jd)), $
  om_unc = om_unc, $
  p_unc = p_unc, $
  rms = rms, $
  rmamp = rmamp, $
  stdevwarn = stdevwarn, $
  stddvtrans_no = stddvtrans_no, $
  tdepth = tdepth, $
  stdepth = stdepth, $
  time_stamp = time_stamp, $
  ting = ting, $
  sting = sting, $
  tegr = tegr, $
  stegr = stegr, $
  Tp_unc = Tp_unc, $
  transit_stdev = transit_stdev, $
  transit_st_stdev = transit_st_stdev, $
  tdur_unc = tdur_unc, $
  tdur_st_unc = tdur_st_unc, $
  vsini = vsini, $
  uvsini = uvsini
  

endif

print, 'Probability of Transit: ', prob_t*100.

monte_tcen = TCen
monte_ting = TIng
monte_tegr = TEgr

monte_stcen = STCen
monte_sting = STIng
monte_stegr = STEgr

if keyword_set(Romac) then print, 'stddvtrans_no is: ', stddvtrans_no
;if keyword_set(Romac) then stop
;if ~keyword_set(monte_p) then stop
end ;transit.pro
