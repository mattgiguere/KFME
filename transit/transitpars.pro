pro transitpars, $
per=per, $
om=om, $
ecc=ecc, $
Tp=Tp, $
msini=msini, $
inc=inc, $
t_cen = t_cen, $
t_dur = t_dur, $
hd17156b = hd17156b, $
hd195019b = hd195019b, $
cancri55c = cancri55c, $
printpars = printpars, $
r_star = r_star, $
m_star = m_star

;PURPOSE: This procedure will calculate the transit center, 
;duration, and probability given the orbital parameters
;making use of the analytical equations. 
;
;CATEGORY: TRANSIT
;
;KEYWORDS:
;
;PER: The orbital period of the planet
;OM: The argument of periastron passage
;ECC: The eccentricity
;Tp: The time of periastron passage
;Msini: In Earth Masses
;inc:the inclination in degrees
;t_cen: this will output the time of center transit 
;		  in JD - 2.44d6
;t_dur: this will output the time of the transit duration
;		  in hours.
;
;2010.11.21 ~MJG

R_sun = 6.955d8 ;m
M_sun = 1.989d30 ;kg

if ~keyword_set(per) then per = 283.584d
if ~keyword_set(om) then om = 341.86d
if ~keyword_set(ecc) then ecc = 0.128d
if ~keyword_set(Tp) then Tp = 15724.57d 
if ~keyword_set(msini) then msini = 134d ;in M_earth
if ~keyword_set(inc) then inc = 90d

;HD 164509:
;R_star = 1.06d
;M_star = 1.13d


;TEST CASES THAT ALL WORKED AND PRODUCED THE SAME RESULTS 
;AS TRANSITSEARCH.ORG:
if keyword_set(hd195019b) then begin
;;HD195019b:
per = 18.2025d
om = 234.483d
ecc = 0.012d
Tp = 2451015.678d - 2.44d6
msini = 3.723d
endif;HD195019b 
;
if keyword_set(hd17156b) then begin
;HD17156b
per = 21.2166d
om = 121.9d
ecc = 0.682d
Tp = 2454757.008d - 2.44d6
msini = 3.2d * 318d ;M_earth
inc = 87.9d ;degrees
endif ;HD17156b
;
if keyword_set(Cancri55c) then begin
;;55cnc c
per = 44.32411d
om = 40.2d
ecc = 0.493d
Tp = 2450028.85865d - 2.44d6
endif ;Cancri55c
;
;HD209458



;FIRST COMPUTE THE TRANSIT CENTER:
nu = 90d - om
x = where(nu lt 0, xct)
if xct gt 0 then nu[x] += 360d
nu *= (!dpi / 180d)

sqrs = sqrt( (1d - ecc)/(1d + ecc) )
bigE = 2d * atan(sqrs* tan(nu/2d))
bigM = bigE - ecc*sin(bigE)

t_M = per * bigM / (2d * !dpi)

t_center = Tp + t_M


now = systime(/julian) - 2.44d6

dift = now - t_center

npers = floor(dift/per)

t_center += npers*per

;print, 'The time of center transit is: '
;printjds, t_center
;printjds, t_center + per
;stop
;NEXT COMPUTE THE TRANSIT DURATION:

;Make a guess at the size of the planet:

rho = dblarr(n_elements(msini))
M_earth = 5.974d24 ;kg

lowx = where(msini lt 10d, loxct)
if loxct gt 0 then rho[lowx] = 5515d ;kg m^-3

midx = where(((msini ge 10d ) AND (msini lt 25d)), midxct)
if midxct gt 0 then rho[midx] = 1638 ;kg m^-3

hix = where(msini ge 25d, hixct)
if hixct gt 0 then rho[hix] = 1326d ;kg m^-3

jupx = where(msini gt 200d, jupxct)

msinikg = msini * M_earth
r_pl = (3d / (4d * !dpi) * msini / rho)^(1d / 3d)
r_pl /= r_sun

if jupxct gt 0 then r_pl[jupx] = 0.1d ;the radius of jupiter


M_starkg = M_star * M_sun

R_tot = (R_pl + R_star)*R_sun
M_tot = M_starkg + msinikg
psec = per * 24d * 3600d
Gcon = 6.67d-11; N m^2 / kg^2
a_maj = ( (psec / (2d * !dpi) )^2d * Gcon * M_tot)^(1d / 3d)
r_dif = a_maj*(1d - ecc^2)/(1d + ecc*cos(nu) )
inc *= (!dpi/180d)

t_dur1 = sqrt(1d - r_dif^2 * cos(inc)^2/R_tot^2)
t_dur2 = sqrt(1d - ecc^2)/(1d + ecc*cos(nu))
t_dur3 = ( psec / (2d * !dpi * Gcon * M_tot) )^(1d / 3d)

t_dur = 2d * R_tot * t_dur1 * t_dur2 * t_dur3 ;seconds
t_dur /= 3600d ; now in hours


t_dur_chk = 2d * R_tot * t_dur1 * sqrt( a_maj* (1d - ecc^2) / (Gcon * M_tot)) / (1d + ecc*cos(nu) )
if keyword_set(printpars) then begin
print, 'The duration of transit is: '
print, t_dur*60, ' minutes.'
print, t_dur, ' hours.'
print, t_dur / 24d, ' days.'
endif;KW:printpars
;stop
;COMPUTE THE TRANSIT PROBABILITY:

t_cen = t_center

;LASTLY COMPUTE THE TRANSIT DEPTH:

end;transitpars.pro