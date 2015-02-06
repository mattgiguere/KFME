;***********************************************************************
; NAME: TRANSIT_PROB.PRO
;																	   
; PURPOSE: To calculate the probability of transit
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
;***********************************************************************
pro transit_prob, ring, rcen, regr, StarRad, knownstarrad, prob_t

;use the minimum operator to find at what point the planet will be 
;closest to the star:

rleast = ring < rcen < regr

print, 'rleast is: ', rleast

print, 'knownstarrad is: ', knownstarrad


;the probability of transit:
cos_graze = (StarRad)/sqrt(rleast^2. + StarRad^2.)

prob_t = cos_graze

print, 'prob_t is: ', prob_t * 100.


end; transit_prob.pro