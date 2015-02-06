;***********************************************************************
; NAME: COORD_TRANS.PRO   
;																	   
; PURPOSE:  To c  
;																	   
; CATEGORY: EXOPLANETS							   
;																	   
; CALLING SEQUENCE:													   
;																	   
;																	   
; INPUTS:			
;	A:		The semi-major axis of the system (a1sini + a2sini) in
;			meters
;	BIG_OM:	The longitude, or position angle, of the node
;	E:		The eccentricity of the system
;	OM: 	The angle between the ascending nodal point and the
;			periastron as measured in the direction of orbit
;	NU:		The true anomaly
;	P:		The period of the system
;																	   
; OPTIONAL INPUTS:													   
;																	   
; KEYWORD PARAMETERS:	
;
; NO_ELLIPSE2SKY_PLOT: Use this keyword if you don't want any plots to 
;		be produced. This will be especially useful when running the
;		transit_monte procedure. 
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
;     c. Matt Giguere, Friday, July 20th, 2007						   
;***********************************************************************
pro ellipse2sky, $
a, $
big_om, $
e, $
om,  $
nu, $
xnew, $
ynew, $
znew, $
inc = inc, $
nel = nel, $
no_ellipse2sky_plot = no_ellipse2sky_plot

;***********************************************************************
;INPUT:
;***********************************************************************

;number of elements for nu:
nels = nel

;the inclination of the orbit will always be assumed to be 90:
if ~keyword_set(inc) then i = !pi/(2.d) else i = inc
;big_om = 0.d


;***********************************************************************
;Constants:
;***********************************************************************
  year = 3.15576d7						;in seconds
  day = 3600.d*24.d						;in seconds
  AU = 1.49597870662d11					;in m
  G = 6.6725985d-11						;SI
  RSUN = 6.95D8                     ;RADIUS OF SUN IN METERS


;See Taff, Celestial Mechanics: A Computational Guide for the
;	Practitioner, 1985

;The angle traced out from periastron around orbit:
nu = 2.d*!pi*findgen(nels)/nels

;***********************************************************************
;      IN THE ORBITAL PLANE, THE PLANET IS GIVEN BY
;***********************************************************************
;The distance of the planet from the star in the orbital plane:
r = a*(1.d - e^(2.d))/(1.d + e*cos(nu))

;And in Cartesian Coordinates:
X = r*cos(nu)
Y = r*sin(nu)
Z = 0

;OLD CARTESIAN COORDS (REPLACED 2007.10.17):



;***********************************************************************
;			   PROJECTING ONTO PLANE OF SKY:
;***********************************************************************

om *= !pi/180.d
big_om *= !dpi / 180.d
i *= !pi/180.d

print, 'omega is: ', strt(om), ' radians/ ',$
strt(om*!radeg), ' degrees.'
print, 'big_om is: ', strt(big_om), ' radians/ ',$
strt(big_om*!radeg), ' degrees.'
print, 'i is: ', strt(i), ' radians/ ',$
strt(i*!radeg), ' degrees.'

;stop
;The Thiele-Innes Constants are:
ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(i))
BTI =  (cos(om)*sin(big_om) + sin(om)*cos(big_om)*cos(i))
CTI =   sin(om)*sin(i)
FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(i))
GTI = -(sin(om)*sin(big_om) - cos(om)*cos(big_om)*cos(i))
HTI =   cos(om)*sin(i)

;The Coordinates of the planet are now:
xnew = ATI*X + FTI*Y
ynew = BTI*X + GTI*Y
znew = CTI*X + HTI*Y

;return om back to degrees:
om *= 180.d / !dpi

end ;ellipse2sky.pro