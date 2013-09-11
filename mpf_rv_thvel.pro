;***********************************************************************
; NAME: MPF_RV_THVEL
;																	   
; PURPOSE: TO COMPUTE A SET OF THEORETICAL RADIAL VELOCITY DATA WHEN 
;	GIVEN TIME ABSCISSAE AND ORBITAL PARAMETERS AS NEEDED TO FIT THE 
;	SIM MULTIPLANET FITTING (MPF) "SIM"ULATIONS. THIS PROCEDURE IS
;	JUST A CLEANED UP VERSION ADAPTED FROM RV.PRO AND RV_DRIVE.PRO
;																	   
; CATEGORY: MPF							   
;																	   
; CALLING SEQUENCE:													   
;		THVELOCITIES = MPF_RV_THVEL(TIME_ARR, PAR)
;																	   
; INPUTS:															   
;	PAR: THE ORBITAL PARAMETERS USED TO CONSTRUCT THE THEORETICAL
;		VELOCITY CURVE (A TOTAL OF 5 ELEMENTS)
;	PAR[0]: PERIOD (DAYS)
;	PAR[1]: MASS OF PLANET (EARTH_MASSES)
;	PAR[2]: ECCENTRICITY
;	PAR[3]: INCLINATION (DEGREES)
;	PAR[4]: Omega (DEGREES) ;LINE OF APSIDES
;	PAR[5]: omega (DEGREES) ;LONGITUDE OF ASCENDING NODE
;	PAR[6]: MASS OF STAR (SOLAR_MASSES)
;	PAR[7]: TIME OF PERIASTRON (DAYS)
;	PAR[8]: K - VELOCITY AMPLITUDE (M/S)
;	PAR[9]: gam - CENTER OF MASS VELOCITY (M/S)
;
;	TIME_ARR: THE TIME ABSISSAE IN THE USUAL JD - 2.44D6 FORM. 
;		THE OUTPUT RADIAL VELOCITIES WILL CORRESPOND TO THESE TIMES.
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
;     c. Matt Giguere, Monday, May 05, 2008		
;***********************************************************************
function mpf_rv_thvel, time_arr, par, _EXTRA=functargs


;USEFUL CONSTANTS:
AU = 1.49597870662d11 	;m
G = 6.6725985d-11 		;m^3/(kg*s^2)
MSUN = 1.9889225d30		;kg
day2secs = 24d * 3600d 	;seconds
MEARTH = 5.9742d24		;kg

;REDEFINE PARAMETER ARRAY IN FRIENDLIER NAMED VARIABLES:
P = double(par[0])					;period, days
m2 = double(par[1]*MEARTH)		;planet mass, kg
e = double(par[2])						;eccentricity
i = double(par[3]*!dtor)					;inclination, radians
bigom = double(par[4]*!dtor)			;line of apsides, radians
om = double(par[5]*!dtor)				;longitude of ascending nodes, radians
m1 = double(par[6])*MSUN		;star mass (kg)
Tp = double(par[7])		;Time of periastron (jd - 2.44d6)
K = double(par[8])		;Velocity Amplitude (m/s)
gam = Par[9]  ;center of mass velocity (m/s)
dvdt = Par[10] ;linear trend
curve = Par[11] ;curvature trend
mt = double(m1+m2)		;total mass, solar masses
Ps=P * day2secs			;period in seconds

i *= !dtor

;ASSUMPTIONS!
; Center of mass velocity is zero:
cmvel = 0d

;CALCULATE A1 (SEMI MAJOR AXIS OF STAR):
; a1 = abs(K)*abs(Ps)*sqrt(1-e^2)/(2.*!pi*sin(i))
a1 = abs(K) * Ps * sqrt(1-e^2)/(2.d * !pi * sin(i))

;Use COM to find a2;
;a1m1  = a2m2
a2 = a1*m1/m2

; Calculate a(relative)
a = a1 + a2

;Use Kepler's 3rd in the case of i = 0.d:
if a1 gt 1d14 then a = ( G*(m1 + m2)* p^2 / (4* !pi^2))^(1./3.)

; Calculate the approximate eccentric anomaly, E1, via the mean
; anomaly, M.
M=2.d * !dpi * ( ((time_arr - tp)/P) - fix((time_arr - tp)/P))

;First Guess: Taff pg.54
E1 = M + e*sin(M)/(1.d0-sin(M+e)+sin(M))
E0 = 0d & ct = 0d

; Refine the estimate using the Newton-Rhapson iteration
repeat begin
  ct++
  E0=E1
  Ea=E0-e*sin(E0)-M
  Eb=1.d0-e*cos(E0)
  E1=E0+(Ea/Eb)*(E1 gt E0)-(Ea/Eb)*(E1 le E0) 
endrep until (max(E1-E0) lt 1.0d-6 or ct gt 100)


; Calculate nu and r
n1=1.d0 + e
n2=1.d0 - e
nu=2.d0*atan((n1/n2)^0.5*tan(E1/2.d0))
r=a*(1.d0-e*cos(E1))				;in meters

; Calculate the radial velocity
k1 = (2.d0*!dpi/P) * (m2*a/(m1+m2)) * sin(i)/(1.d0-(e^2))^0.5
rv1 = k1 * (cos(nu+om) + e*cos(om))

; convert the radial velocity from meters/day to meters/sec
vel = (rv1/day2secs) + cmvel

;stop

tinp = double(time_arr) - 1.4d4

vel += gam + dvdt*tinp + curve*tinp^2

return,vel


end;
