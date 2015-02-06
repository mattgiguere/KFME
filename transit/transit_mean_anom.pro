;***********************************************************************
; NAME: TRANSIT_MEAN_ANOM.PRO
;																	   
; PURPOSE: Calculate the mean anomaly for transit. 
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
; SIDE EFFECTS:														   
;																	   
; RESTRICTIONS:														   
;																	   
; PROCEDURE:														   
;																	   
; EXAMPLE:			
;																	   
; MODIFICATION HISTORY:												   
;     c. Matt Giguere, Monday, March 31, 2008		
;        2009.02.20: added secondary transit in calculation ~MJG
;***********************************************************************
pro transit_mean_anom, arel, big_om, inc, e, m1sini, m2sini, om, nu, $
xnew, ynew, znew, nel = nel, no_ellipse2sky = $
no_ellipse2sky, transit_plot = transit_plot, $
StarRad = StarRad, rcen, ring, regr, mcen, ming, megr, $
 srcen, sring, sregr, smcen, sming, smegr, starnm=starnm, $
 time_stamp=time_stamp, plot_transit=plot_transit, $
 pscolor=pscolor, psbw=psbw

;***********************************************************************
;		CALL ON ELLIPSE TO SKY TO PLOT THIS ORBIT
;***********************************************************************

m2au = 1.49598d11 ; 1 AU in meters
big_om = 0.d
inc = 90.d
om = double(om)

print, 'starting ellipse2sky @ ', systime()
ellipse2sky, arel, big_om, e, om, nu, xnew, ynew, $
	znew, inc = inc, nel = nel, $
	no_ellipse2sky = no_ellipse2sky
print, 'finished ellipse2sky @ ', systime()


;***********************************************************************
;		CALCULATE INGRESS, CENTER AND EGRESS FOR PRIMARY AND
;                     SECONDARY TRANSITS 
;***********************************************************************
;The equation for the disc of the host star:
;	x^2 + y^2 = R^2
xstar = StarRad

print, 'INGRESS, CENTER AND EGRESS OF TRANSITS: ', systime()
for i=1LL, n_elements(xnew)-1.d do begin
	if ( (xnew[i] le xstar) AND (xnew[i-1LL] gt xstar) ) then xing = i
	if ( (xnew[i] le -xstar) AND (xnew[i-1LL] gt -xstar) ) then xegr = i
	if ( (xnew[i] le 0) AND (xnew[i-1LL] gt 0) ) then xcen = i
	if ( (xnew[i-1LL] le xstar) AND (xnew[i] gt xstar) ) then sxegr = i
	if ( (xnew[i-1LL] lt -xstar) AND (xnew[i] ge -xstar) ) then sxing = i
	if ( (xnew[i] ge 0) AND (xnew[i-1LL] lt 0) ) then sxcen = i
endfor

if n_elements(xing) eq 0 then begin 
 if ( (xnew[0] le xstar) AND (xnew[n_elements(xnew)-1LL] gt xstar) ) then xing = 0
 print, 'there was no xing!'
 print, 'but #xing is now: ', n_elements(xing)
endif

;2009.02.18: added the following to take care of problems with 
; highly eccentric fits with short orbital period planets:
if n_elements(xing) le 0 then begin
  xing = LONG64(where(xnew eq max(xnew)))
  xing = xing[0]
endif
if n_elements(xegr) le 0 then begin
  xegr = LONG64(where(xnew eq min(xnew)))
  xegr = xegr[0]
endif

nnew = n_elements(xnew)
if n_elements(xcen) eq 0 then begin 
  if ( (xnew[0] le 0) AND (xnew[nnew-1LL] gt 0) ) then xcen = 0
  print, 'there was no xcen!'
  print, 'but #xcen is now: ', xcen
endif

if n_elements(xegr) eq 0 then begin 
  if (xnew[0] lt -xstar) AND (xnew[nnew-1LL] ge -xstar) then xegr = 0
  print, 'there was no xegr!'
  print, 'but #xegr is now: ', n_elements(xegr)
endif

if n_elements(sxcen) le 0 then begin
  if ( (xnew[0] ge 0) AND (xnew[nnew-1] lt 0) ) then sxcen = 0
endif;sxcen

if n_elements(sxing) le 0 then begin
  if ( (xnew[0] ge -xstar) AND (xnew[nnew-1] lt -xstar) ) then sxing =  0
endif;sxing

if n_elements(sxegr) le 0 then begin
  if ( (xnew[nnew-1] le xstar) AND (xnew[0] gt xstar) ) then sxegr=0
endif;sxegr

if n_elements(sxing) le 0 then begin
  usersymbol, 'star', sym_size=3
  plot, xnew, ynew
  oplot, [0,0], [0,0], ps=8
  sxing=where(xnew eq min(xnew))
endif;sxing

if n_elements(sxegr) le 0 then begin
  usersymbol, 'star', sym_size=3
  plot, xnew, ynew
  oplot, [0,0], [0,0], ps=8
  sxegr=where(xnew eq max(xnew))
endif;sxegr



if ~n_elements(xcen)*n_elements(xing)*n_elements(xegr) then stop
if ~n_elements(sxcen)*n_elements(sxing)*n_elements(sxegr) then stop

print, 'xing is: ', strt(xing), ' which has an xnew of: ', xnew[xing]
print, 'xcen is: ', strt(xcen), ' which has an xnew of: ', xnew[xcen]
print, 'xegr is: ', strt(xegr), ' which is: ', xnew[xegr]

print, 'sxing is: ', strt(sxing), ' which has an xnew of: ', xnew[sxing]
print, 'sxcen is: ', strt(sxcen), ' which has an xnew of: ', xnew[sxcen]
print, 'sxegr is: ', strt(sxegr), ' which is: ', xnew[sxegr]


print, 'finished I,E and C of Transit @ ', systime()

;For a planet with an ascending node on the RHS of the host star:

;The true anomaly of ingress:
nuing = nu[xing]

;The true anomaly of center:
nucen = nu[xcen]

;The true anomaly of egress:
nuegr = nu[xegr]

;The true anomaly of secondary transit ingress:
nusing = nu[sxing]

;The true anomaly of secondary transit center:
nuscen = nu[sxcen]

;The true anomaly of secondary transit egress:
nusegr = nu[sxegr]

;The planetary distance at ingress:
ring = arel*(1.d - e^(2.d))/(1.d + e*cos(nuing) )

;The planetary distance at center:
rcen = arel*(1.d - e^(2.d))/(1.d + e*cos(nucen) )

;The planetary distance at egress
regr = arel*(1.d - e^(2.d))/(1.d + e*cos(nuegr) )

;The planetary distance at secondary transit ingress:
sring = arel*(1.d - e^(2.d))/(1.d + e*cos(nusing) )

;The planetary distance at secondary transit center:
srcen = arel*(1.d - e^(2.d))/(1.d + e*cos(nuscen) )

;The planetary distance at secondary transit egress
sregr = arel*(1.d - e^(2.d))/(1.d + e*cos(nusegr) )

;The planetary distance throughout the orbit:
rarr = arel*(1.d - e^(2.d))/(1.d + e*cos(nu) )

;determine if ing,cen or egr is the last element in the array 
;(happened with HIP85977):
if xing eq n_elements(nu)-1 then nuing2 = nu[0] else nuing2 = nu[xing+1]
if xcen eq n_elements(nu)-1 then nucen2 = nu[0] else nucen2 = nu[xcen+1]
if xegr eq n_elements(nu)-1 then nuegr2 = nu[0] else nuegr2 = nu[xegr+1]
if sxing eq n_elements(nu)-1 then nsing2=nu[0] else nsing2=nu[sxing+1]
if sxcen eq n_elements(nu)-1 then nscen2=nu[0] else nscen2=nu[sxcen+1]
if sxegr eq n_elements(nu)-1 then nsegr2=nu[0] else nsegr2=nu[sxegr+1]

;first we determine the element up from the ingress, center and egress:
ring2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nuing2) )
rcen2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nucen2) )
regr2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nuegr2) )
sring2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nsing2) )
srcen2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nscen2) )
sregr2 = arel*(1.d - e^(2.d))/(1.d + e*cos(nsegr2) )

;this flag will take care of the ambiguity in the mean anomaly later on:
if ring2 lt ring then ingflag = 1. else ingflag = 0.
if rcen2 lt rcen then cenflag = 1. else cenflag = 0.
if regr2 lt regr then egrflag = 1. else egrflag = 0.
if sring2 lt sring then singflag = 1. else singflag = 0.
if srcen2 lt srcen then scenflag = 1. else scenflag = 0.
if sregr2 lt sregr then segrflag = 1. else segrflag = 0.


if keyword_set(transit_plot) then plot, nu, rarr
;***********************************************************************
;		DETERMINE THE ECCENTRIC ANOMALY
;***********************************************************************
e = double(e)

;This IF compensates for circular orbits:
;For Ingress
numer = 1.d - abs(ring)/arel
if ( (numer ne 0.d) AND (e ne 0) ) then EAing =acos(numer/e) $
	else eaing = 0.d

;For Center:
numer = 1.d - abs(rcen)/arel
if ( (numer ne 0.d) and (e ne 0) ) then EAcen =acos(numer/e) $
	else eacen = 0.d

;For Egress:
numer = 1.d - abs(regr)/arel
if ( (numer ne 0.d) and (e ne 0) ) then EAegr =acos(numer/e) $
	else eaegr = 0.d

;For Secondary Transit Ingress
numer = 1.d - abs(sring)/arel
if ( (numer ne 0.d) AND (e ne 0) ) then sEAing =acos(numer/e) $
	else seaing = 0.d

;For Secondary Transit Center:
numer = 1.d - abs(srcen)/arel
if ( (numer ne 0.d) and (e ne 0) ) then sEAcen =acos(numer/e) $
	else seacen = 0.d

;For Secondary Transit Egress:
numer = 1.d - abs(sregr)/arel
if ( (numer ne 0.d) and (e ne 0) ) then sEAegr =acos(numer/e) $
	else seaegr = 0.d


;The Eccentric Anomaly throughout the orbit:
numer = 1.d - abs(rarr)/arel

;these next few lines take care of errors due to DOUBLEs not
;having high enough precision:
if max(numer) gt e then begin
print, 'max numer was: ', max(numer)
x = where(numer gt e)
numer[x] -= 1.1d-8
endif

if min(numer) lt -e then begin
print, 'min numer was: ', min(numer)
x = where(numer lt -e)
numer[x] = 1.1d-8
endif

if max(numer) gt e then begin
print, 'now you went too far the other way! ', max(numer)
x = where(numer gt e)
numer[x] = e
endif

if ((min(numer) lt 0) AND (min(numer) gt -1d-10)) then begin
;numer[x] = -e
endif



if (min(numer) lt -e) OR (max(numer) gt e) then begin
print, 'e is: ', e
print, 'minmax(numer): ', minmax(numer)
print, 'minmax(numer) cannot be lt/gt e, respectively.'
stop
endif

EAarr =acos(numer/e)


;***********************************************************************
;		DETERMINE THE MEAN ANOMALY
;***********************************************************************
;For Ingress:
if eaing ne 0.d then Ming = EAing - e*sin(EAing) else ming = nu[xing]

;For Center:
if eacen ne 0.d then Mcen = EAcen - e*sin(EAcen) else mcen = nu[xcen]

;For Egress:
if eaegr ne 0.d then Megr = EAegr - e*sin(EAegr) else megr = nu[xegr]

;For Ingress:
if seaing ne 0.d then sMing=sEAing - e*sin(sEAing) else sming=nu[sxing]

;For Center:
if seacen ne 0.d then sMcen=sEAcen - e*sin(sEAcen) else smcen=nu[sxcen]

;For Egress:
if seaegr ne 0.d then sMegr=sEAegr - e*sin(sEAegr) else smegr=nu[sxegr]

;this takes care of the pi ambiguity in the mean anomaly:
if ingflag then ming = (2*!pi-ming)
if cenflag then mcen = (2*!pi-mcen)
if egrflag then megr = (2*!pi-megr)
if singflag then sming = (2*!pi-sming)
if scenflag then smcen = (2*!pi-smcen)
if segrflag then smegr = (2*!pi-smegr)

;For the total orbit:
Marr = EAarr - e*sin(EAarr)


end; transit_mean_anom.pro