;***********************************************************************
; NAME: transit_phase
;																	   
; PURPOSE: This procedure will phase the data. I developed a few lines
;	in the original transit procedure that did this, but in order to
;	be consistent with the plots in rv_fit, I decided to reppeat the 
;	same phasing procedure as Dr. Fischer did in rv_fit and therefore
;	adopted this code from that procedure. 
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
;     c. Matt Giguere, Tuesday, April 22, 2008	
;    revamped completely Thu, Aug 14, 2008
;***********************************************************************
function transit_phase, $
cf3 = cf3, $
par =par, $
tinit = tinit, $
make_plots = make_plots, times = times

;speed things up a bit:
compile_opt idl2, hidden


if n_elements(cf3) gt 0 then begin
  times = cf3.jd
  vels = cf3.mnvel
if n_elements(tinit) lt 1 then tinit=cf3[0].jd
endif ;cf structure provided

;calculate the integer number of periods the observation time is more
;than the initial time:
ntgrprds =  floor((times - tinit)/par[0])

;subtract the integer periods that the observation time is more than the
;initial time from the original time - the initial time to start
;phase-folding the data and then divide by the period to get the
;phase-folded time of observation:
phasedtimes = (times - tinit - ntgrprds*par[0])/par[0]

print, 'the minmax of the phased times are: ', minmax(phasedtimes)

;stop







return, phasedtimes
end;