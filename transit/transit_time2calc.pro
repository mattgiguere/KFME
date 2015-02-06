;***********************************************************************
; NAME: TRANSIT_TIME2CALC														   
;
; PURPOSE: To determine the time to calculate the transit at.
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
pro transit_time2calc, p, tc, tm2clc, tim2calc = tim2calc, tmoing = tmoing, $
tmoegr = tmoegr, tp = tp, current = current

if ~keyword_set(tim2calc) then begin
if (keyword_set(TmoIng) AND (Tp lt TmoIng)) then begin
   Tm2Clc = TmoIng 
endif else begin
    if (keyword_set(TmoEgr) AND (Tp gt TmoEgr)) then begin
        Tm2Clc = TmoEgr
    endif else begin
        if (Tc ne 0.) then begin
           Tm2Clc = Tc + .25d * p
        endif else Tm2Clc=systime(/Julian)
    endelse
endelse
endif else Tm2Clc = tim2calc

if keyword_set(current) then Tm2Clc = systime(/julian)
print, 'Tm2Clc is: ', Tm2Clc

end; transit_time2calc.pro