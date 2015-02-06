;***********************************************************************
; NAME: TRANSIT_THVEL.PRO
;																	   
; PURPOSE: 
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
pro transit_thvel, mypars, mytfine, tpplot, tim2calc, $
no_transit_plot = no_transit_plot

p = mypars[0]
Tp = mypars[1];+2.44d6
e = mypars[2]
om = mypars[3]
k = mypars[4]
gam = mypars[5]
dvdt = mypars[6]

mytfine = p * dindgen(6000.d)/4000.d + tim2calc - p/2.d
thvel = rv_drive_mp(mytfine, mypars)

xth = where(abs(mytfine ) le 100., ct)

message, /informational

if ~keyword_set(no_transit_plot) then begin
   dum = label_date(date_format=['%D %H:%I', '%Y.%M'])
   plot, mytfine, thvel, xtickunits = ['Time','Time'], $
   xtickformat='LABEL_DATE', XSTYLE=1, xticks=4, YSTYLE = 1, $
   background = 255, color = 0, YMARGIN = [6,3], $
   YRANGE = 1.25*minmax(thvel), $
   TITLE = 'Theoretical Velocity Curve'
   
   ;LEGEND
   xyouts, .2, .82, 'Dashed = Time to Calc', color = 0, /norm
   xyouts, .2, .78, 'Dotted = Time of periastron', color = 0, /norm
   
   stp = strtrim(string(p, format = '(F10.2)'), 2)
   stk = strtrim(string(k, format = '(F10.2)'), 2)
   ste = strtrim(string(e, format = '(F10.2)'), 2)
   xyouts,.65, .82,'!6 P = ' + stp +' d', color = 0, /norm
   xyouts,.65, .76,'!6 K = ' + stk +' ms!u-1!n', color = 0, /norm
   xyouts,.65, .7,'!6 e = ' + ste , color = 0, /norm
   
   plots, minmax(mytfine), [0.,0.], color = 0
   
   ;DASHED LINE SHOWS TIME TO CALCULATE TRANSITS
   plots, [tim2calc, tim2calc], [-10,10], color = 0, linestyle = 2
   if e ne 0 then begin
   
   ; DOTTED LINE SHOWS TIME OF PERIASTRON
   plots, [Tpplot, Tpplot], [-10,10], color = 0, linestyle = 1
   tpplot += p
   plots, [Tpplot, Tpplot], [-10,10], color = 0, linestyle = 1
   tpplot -= 2*p
   plots, [Tpplot, Tpplot], [-10,10], color = 0, linestyle = 1
   endif ;plot periastron
endif ;KW: ~no_transit_plot

end; transit_thvel.pro