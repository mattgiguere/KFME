;***********************************************************************
; NAME: 	STARDAT
;																	   
; PURPOSE:    This function will hold all of the data for a bunch of
;		stars. when the user inputs the string with the name of the 
;		star, it will return a structure with data about that star
;		that will be useful for transit.pro
;																	   
; CATEGORY: EXOPLANETS							   
;																	   
; CALLING SEQUENCE:													   
;																	   
;																	   
; INPUTS:															   
;																	   
; OPTIONAL INPUTS:													   
;																	   
; KEYWORD PARAMETERS:												   
;																	   
; OUTPUTS:															   
;																	   
; OPTIONAL OUTPUTS:													   
;																	   
; COMMON BLOCKS:													   
;																	   
; SIDE EFFECTS:														   
;																	   
; RESTRICTIONS:														   
;																	   
; PROCEDURE:														   
;																	   
; EXAMPLE:															   
;																	   
; MODIFICATION HISTORY:												   
;     c. Matt Giguere, Sunday, September 16, 2007						   
;     -Added multiple planets 2009.03.08 MJG
;		20110425 - MJG: Resolved an issue when the planet is not in
;							AUXCAT2, KECK_ST, N2K_KECK or M2K.DAT
;							
;***********************************************************************
function stardat, star, nplanets=nplanets, pstate=pstate

if keyword_set(pstate) then kfmedatdir = (*pstate).kfmedir+'data/'
;stop
m_earth2m_jup = 0.00314646864d

a = 0d
spec = ''
frz_ecc = 0.
frz_per = 0.
frz_k = 0.
inc = 90.d
K = 0.d
msini = 0.d
om = 0.d
TmoEgr = 0.d
TmoIng = 0.d
Tc = 0.d
Tp = 0.d
TrKnown = 0.d
knownStarRad = 0.d
tim2calc = 0.d
vsini=0d
trend=0
dvdt=0
mstar=0

star = STRUPCASE(star)
print, 'star is: ', star

readcol, (*pstate).starlist, stararr, mstararr, mstar_uncarr, $
knownstarradarr, unc_rstararr, skip=1, delimit=',', format='a,f,f,f,f'
stararr = STRUPCASE(stararr)
x = where(stararr eq star, sct)
if sct then begin
	mstar = mstararr[x]
	mstar_unc = mstar_uncarr[x]
	knownstarrad = knownstarradarr[x]
	unc_rstar = unc_rstararr[x]
endif else print, 'DID NOT FIND IT!';!E in stararr
;stop

if tim2calc eq 0 then tim2calc = systime(/julian)
if n_elements(p) eq 0 then p=0d
if n_elements(spec) eq 0 then spec=0d
if n_elements(e) eq 0 then e=0d
if n_elements(distance) eq 0 then distance=0d
if n_elements(udist) eq 0 then udist=0d
if n_elements(gam) eq 0 then gam = 0d
if n_elements(inc) eq 0 then inc = 0d
if n_elements(k) eq 0 then k = 0d
if n_elements(lum) eq 0 then lum = 0d
if n_elements(msini) eq 0 then msini = 0d
if n_elements(om) eq 0 then om = 0d
if n_elements(plx) eq 0 then plx = 0d
if n_elements(uplx) eq 0 then uplx = 0d
if n_elements(comp) eq 0 then comp = 'b'
if n_elements(mstar) eq 0 then mstar = 1d
if n_elements(dvdt) eq 0 then dvdt = 0d
if n_elements(trend) eq 0 then trend = 0d
if n_elements(teff) eq 0 then teff=5777d
if n_elements(teff_unc) eq 0 then teff_unc=0d
if n_elements(vsini) eq 0 then vsini = 0d
if n_elements(uvsini) eq 0 then uvsini = 0d
if n_elements(unc_mstar) eq 0 then unc_mstar = 0d
if n_elements(mstar) eq 0 then mstar = 1d
if n_elements(unc_rstar) eq 0 then unc_rstar = 0d
if total(finite(vsini)) lt n_elements(vsini) then vsini[where(~finite(vsini))]=0d

mstar = mstar[0]
if mstar eq 0d then begin
print, 'The mass of your star is equal to zero...'
print, 'now setting it to 1 M_SUN.'
mstar = 1d
endif

knownstarrad = knownstarrad[0]
if knownstarrad eq 0d then begin
print, 'The radius of your star is equal to zero...'
print, 'now setting it to 1 R_SUN.'
knownstarrad = 1d
endif



print, 'knownstarrad is: ', knownstarrad
print, 'mstar is: ', mstar

;stop
stardat = create_struct($
'p', p, $
'spec', spec, $
'comp', comp, $
'dist', distance, $
'udist', udist, $
'dvdt', dvdt, $
'e', e, $
'frz_ecc', frz_ecc, $
'frz_per', frz_per, $
'frz_k', frz_k, $
'gam', gam, $
'inc', inc, $
'K', K, $
'lum', lum, $
'msini', msini, $
'mstar', mstar, $
'unc_mstar', unc_mstar, $
'om', om, $
'plx', plx, $
'uplx', uplx, $
'teff', teff, $
'teff_unc', teff_unc, $
'TmoEgr', TmoEgr, $
'TmoIng', TmoIng, $
'tim2calc', tim2calc, $
'Tc', Tc, $
'Tp', Tp, $
'trend', trend, $
'TrKnown', TrKnown, $
'knownStarRad', knownStarRad, $
'unc_rstar', unc_rstar, $
'vsini', vsini, $
'uvsini', uvsini)

return, stardat
end ;stardat.pro