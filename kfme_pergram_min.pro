pro kfme_pergram_min, cf, nu_out, peri_out, pkperiods, pkheights,$
	title=title, fap=fap, signi=signi, simsigni=simsigni, $
	lowper=lowper, pmax=pmax, yra=yra, psdpeaksort=psdpeaksort, $
	multiple=multiple,noise=noise, verbose=verbose, noplot=noplot, $
	numper=numper, pergstruct=pergstruct
;+
; NAME:
;         scargle
;
; PURPOSE:
;         Compute the lomb-scargle periodogram of an unevenly sampled
;         lightcurve. 
;
;         This was originally written by Joern Wilms. This version 
;         has been slightly modified by John Johnson, to FOR loops, but
;         retaining Wilms' original simulation of "multiple" trials
;         for the FAP (simsigni).
;
; CALLING SEQUENCE:
;   scargle,tim,data,nu_out,peri_out,/verbose
; 
; INPUTS:
;         tim: The times at which the time series was measured
;         data: the corresponding measurments at times, t.
;
; OPTIONAL INPUTS: 
;         lowper,pmax: minimum and maximum PERIOD to be used
;         omega: angular frequencies for which the PSD values are
;                desired
;         fap : false alarm probability desired
;               (see Scargle et al., p. 840, and signi
;               keyword). Default equal to 0.01 (99% significance)       
;         noise: for the normalization of the periodogram and the
;            compute of the white noise simulations. If not set, equal to
;            the variance of the original lc.   
;         multiple: number of white  noise simulations for the FAP
;            power level. Default equal to 0 (i.e., no simulations).
;         numf: number of frequencies sampled
;      
;   
; KEYWORD PARAMETERS:
;         verbose: print out peak periods & FAPs, and time-elapsed
;
; OUTPUTS:
;            nu_out   : frequencies at which periodogram evaluated
;            peri_out : the periodogram - power at each nu_out
;
; OPTIONAL OUTPUTS:
;            pkperiods:   periods of 5 highest peaks
;            pkheights:   power at the 5 highest peaks
;            period: period corresponding to each omega
;            signi : power threshold corresponding to the given 
;                    false alarm probabilities fap and according to the
;                    desired number of independent frequencies
;            simsigni : power threshold corresponding to the given 
;                    false alarm probabilities fap according to white
;                    noise simulations
;            psdpeaksort : array with the maximum peak pro each simulation    
;
; PROCEDURE:
;         The Lomb Scargle PSD is computed according to the
;         definitions given by Scargle, 1982, ApJ, 263, 835, and Horne
;         and Baliunas, 1986, MNRAS, 302, 757. Beware of patterns and
;         clustered data points as the Horne results break down in
;         this case! Read and understand the papers and this
;         code before using it! For the fast algorithm read W.H. Press
;         and G.B. Rybicki 1989, ApJ 338, 277.
;
; MODIFICATION HISTORY:
;          Version 1.0, 1997, Joern Wilms IAAT
;              KP: significance levels   
;              JW: pmin,pmax keywords
;          Version 1.5, 1999.08.27, JW: compute the significance levels
;               from the horne number of independent frequencies, and not from
;               numf
;          Version 1.6, 2000.07.27, SS and SB: added fast algorithm and FAP
;               according to white noise lc simulations.    
;          Version 1.8, 2003.11.10 JohnJohn: Look Ma, no FOR loops!
;               New, vectorized version is 25% faster. Uses my FAN procedure: 
;               http://astron.berkeley.edu/~johnjohn/idl.html#FAN
;          Version 1.8a, 2003.11.19 JohnJohn: Jason Wright pointed out error on
;               line 225. Fixed now.
;          Version 1.8b, 2004.01.01 JohnJohn: Geoff Marcy pointed out error on
;               line 275. White noise simulation now properly
;               vectorized as well.
;-

;the number of peaks you want printed out in the terminal window (+1):
numpeaks = 5

tim=cf.jd
data=cf.mnvel

   ;; make times manageable (Scargle periodogram is time-shift invariant)
   time = tim-tim[0]
   if keyword_set(lowper) then pmin = lowper ;set orig keyword
;   c = data  ;   
   ;; defaults
   IF n_elements(noise) EQ 0 THEN noise = double(sqrt((moment(data))[1]))
   IF n_elements(multiple) EQ 0 THEN multiple = 0
   if multiple eq 1 then multiple = 1000  ;guard against /multiple .
   IF n_elements(fap) EQ 0 THEN fap = 0.01
   
   

   ;; number of independent frequencies, "horne"
   ;;  (Horne and Baliunas, eq. 13)
   n0    = n_elements(time)
   horne = long(-6.362+1.193*n0+0.00098*n0^2.)
   IF (horne LT 0) THEN horne=5
   
   IF (n_elements(numf) EQ 0) THEN numf = horne ELSE horne = numf

   ;; min.freq is 1/T
   IF (n_elements(fmin) EQ 0) THEN BEGIN 
       IF (n_elements(pmax) EQ 0) THEN BEGIN 
           fmin = 1.D0 /(2.* max(time))
       END ELSE BEGIN 
           fmin = 1.D0 / pmax
       END
   ENDIF 

      ;; max. freq: 
   IF (n_elements(fmax) EQ 0) THEN BEGIN 
       IF (n_elements(pmin) EQ 0) THEN BEGIN 
           pmin = 2.0d0   ; Default shortest period.
;           pmin = 0.10d0   ; Default shortest period.
           fmax = 1./pmin   ; Default highest frequency
       END ELSE BEGIN 
           fmax = 1.D0 / pmin
       END
   ENDIF 

;Get the number of sampled frequencies - - -
;Condition for spacing: delta nu such that during the
; entire duration of observations, phase slip is no more than P/4

timlen = max(time)    ; - min(t) implicit
dnu = 1./(4.*timlen)
;dnu = 1./(timlen)
numf = fix((fmax - fmin)/dnu+1)  ;Number of sampled frequencies.

if (keyword_set(numper)) then numf=numper

;stop
   ;; if omega is not given, compute it
   IF (n_elements(omega) EQ 0) THEN BEGIN 
       om = 2.D0 * !DPI* (fmin+(fmax-fmin)*findgen(numf)/(numf-1.D0))
   END ELSE BEGIN 
       om = omega
   END
   
   ;; False Alarm Probability according to Numf
   signi = -alog(  1.D0 - ((1.D0-fap)^(1./horne))  )
      
   ;; Periodogram
   ;; Ref.: W.H. Press and G.B. Rybicki, 1989, ApJ 338, 277
   ;; Eq. (6); s2, c2

   t_arr  = fan(time, numf, /trans)
   om_arr = fan(om, n0)

   s2 = total( sin(2d*om_arr*t_arr), 2 )
   c2 = total( cos(2d*om_arr*t_arr), 2 )
       
   ;; Eq. (2): Definition -> tan(2omtau)
   ;; --- tan(2omtau)  =  s2 / c2
   omtau = atan(s2/c2) / (2.D0)       
   
   ;; cos(tau), sin(tau)
   cosomtau= cos(omtau)  
   sinomtau= sin(omtau)
   
   ;; Eq. (7); total(cos(t-tau)^2)  and total(sin(t-tau)^2) 
   tmp = c2*cos(2.D0*omtau) + s2*sin(2.D0*omtau)
   tc2 = 0.5D0*(n0+tmp)         ; total(cos(t-tau)^2)       
   ts2 = 0.5D0*(n0-tmp)         ; total(sin(t-tau)^2) 
   
   ;; clean up
   tmp = 0. & omtau= 0.
   s2  = 0. & t2  = 0.
   
   ;; Computing the periodogram for the original data
   ;; Subtract mean from data
   cn = data - mean(data)

;Prepare for the "multiple" Monte Carlo trials call 50 lines down.
   IF (multiple GT 0) THEN BEGIN
       sisi=dblarr(n0,numf)
       coco=dblarr(n0,numf)
       FOR i=0,numf-1L DO BEGIN 
           sisi[*,i]=sin(om[i]*time)
           coco[*,i]=cos(om[i]*time)
           
;           sh[i]=total(cn*sisi[*,i])
;           ch[i]=total(cn*coco[*,i])
       END 
   END 
   
;Johnjohns replacement 
   ;; Eq. (5); sh and ch
   c_arr = fan(cn, numf, /trans)
   sh = total( c_arr * sin( om_arr * t_arr ), 2)
   ch = total( c_arr * cos( om_arr * t_arr ), 2)

   ;; Eq. (3)
   px = (ch*cosomtau + sh*sinomtau)^2  / tc2 + $
     (sh*cosomtau - ch*sinomtau)^2  / ts2     
   
   ;; correct normalization 
   px = 0.5D0*px/(noise^2)
   

   ;; --- RUN SIMULATIONS for multiple > 0
   IF multiple GT 0 THEN BEGIN
       IF (multiple*min(fap) LT 10) THEN BEGIN 
           message,'WARNING',/informational
           message,'Number of iterations (multiple keyword)',/informational
           message,'not large enough for false alarm probability',/informational
           message,'requested (need multiple*FAP > 10 )',/informational
       ENDIF 
       
       
       IF (keyword_set(verbose)) THEN BEGIN 
           t0=systime(1)
       END 
       
       psdpeak = dblarr(multiple)
       FOR m=0L,multiple-1L DO BEGIN
           IF (keyword_set(verbose)) THEN BEGIN 
               IF ((m+1) MOD 500 EQ 0) THEN BEGIN 
                   message,'...working on '+strtrim(m+1,2)+'th simul. ('+$
                     strtrim(string(format='(F8.3)',100.*m/multiple),2)+ $
                     '% done)',/informational
                   exec=systime(1)-t0
                   message,'   time expired : '+strtrim(exec,2)+'s',/info
                   message,'   time per step: '+ $
                     string(format='(F10.6)',exec/(m+1))+'s',/info
                   message,'   time remaining: '+ $
                     strtrim(exec/(m+1)*(multiple-m),2)+'s',/info
               END 
           END 

           ;; white noise simulation
           cn = randomn(anyseed,n0)*noise
           cn = cn-mean(cn) ;; .. force OBSERVED count rate to zero
           
   ;; Eq. (5); sh and ch
   sh = dblarr(numf) 
   ch = dblarr(numf) 
   
       FOR i=0L,numf-1L DO BEGIN 
           sh[i]=total(cn*sisi[*,i])
           ch[i]=total(cn*coco[*,i])
       END 


           ;; Eq. (3) ; computing the periodogram for each simulation
           psdpeak[m] = max ( (ch*cosomtau + sh*sinomtau)^2 / tc2 + $
             (sh*cosomtau - ch*sinomtau)^2 / ts2 )
   ENDFOR                
           
       ;; False Alarm Probability according to simulations
       IF n_elements(psdpeak) NE 0  THEN BEGIN 
           idx = sort(psdpeak)
           ;; correct normalization 
           psdpeaksort = 0.5D0 * psdpeak[idx]/(noise^2)
           simsigni = psdpeaksort[long((1.-fap)*(multiple-1))]
       ENDIF
       if keyword_set(verbose) then begin
           print,' '
           print,'Monte Carlo: At given FAP = ' + strmid(strtrim(fap,2),0,5)+ ', Power = ' + strmid(strtrim(simsigni,2),0,5)
       end
   END        
   
   ;; some other nice helpers
   ;; computed here due to memory usage reasons
   nu     = om/(2.D0*!dpi)
   period = 1.D0/nu
   nu_out = nu
   peri_out = px  ;for output
  
       ymax = max(px, imx)
       xmax = period[imx]
       i = reverse(sort(px ))
       pkperiods = period(i[0:numpeaks])  ;periodsof 5 tallest peaks
       pkheights = px(i[0:numpeaks])      ;heights of them
       ni_horne = horne
       faps = 1.0d0 - (1.0d0 - exp(-pkheights))^ni_horne
;       st_faps = strmid(strtrim(string(faps*100.),2),0,3) ;false alarm p

;print peak periods, heights, and FAPs
  if keyword_set(verbose) then begin
    print,'-----------------------------------------------'
    print,'       Period         Height       F.-A. Prob.'
    print,'-----------------------------------------------'
    for i=0, n_elements(pkperiods)-1 do begin
    print,pkperiods[i],pkheights[i],faps[i]
    endfor
    print,'-----------------------------------------------'
  end

  if not keyword_set(noplot) then begin

      if not keyword_set(title) then title=' '
      if strt(title) ne '' then title += ' Periodogram'

      if not keyword_set(yra) then yra = [-0.01,1.2*max(px,imx)]
      if keyword_set(fap) then yra[1] = 1.2*max(px,imx) > 1.2*max(simsigni)
      xra = [0.8*min(1./nu),1.01*max(1./nu)]
      plot,(1./nu),peri_out,xtitl='!6Period (d)', $
        /xlog,/nodata,yr=yra,xr=xra, $
        /xsty,/ysty,titl='!6'+title,ytitl='!6 Power'
      
      oplot,(1./nu),peri_out

if keyword_set(fap) then begin
		for i=0, n_elements(fap) -1 do begin
				oplot, minmax(1d / nu), [simsigni[i], simsigni[i]], linestyle=i
				xyouts, (max(1d / nu) - min(1d / nu))/30d * double(i+1), simsigni[i], strt(fap[i]*1d2, f='(F10.2)')+'%'
		endfor ;#FAP
		;al_legend, strt(fap, f='(E10.1)'), linestyle=indgen(n_elements(fap))
		;stop
endif else simsigni = 0;kw(fap)

pergstruct = create_struct('per', 1d / nu, $
'power', peri_out, $
'fap_levels', fap, $
'fap_power', simsigni)

	;This factor takes care of the xyouts for a !p.multi = [0,1,3]
	if strmid(title, 1, strlen(title)-1, /reverse_off) eq 'RA' then begin
	yfac = 2.1/3.
	endif else yfac = 1.
	if strmid(title, 1, strlen(title)-1, /reverse_off) eq 'on' then begin
	yfac = 0.9/3.
	endif

      pkperst = strmid(strtrim(string(pkperiods(0)),2),0,6)
      pkper = pkperiods(0)
      pkht  = pkheights(0)
      xpos = pkper
      ypos = pkht*1.01
      if xpos gt 0.8*xra(1) then begin
      xyouts,0.9, ypos/yra*yfac, ' '+pkperst+' d',size=2., /normal
      endif else begin
      xyouts, xpos, ypos*yfac, ' '+pkperst+' d',size=2.
      endelse
	
      oplot,[pkperiods],[pkheights],ps=3,symsize=1.5
  end
retstruct = {period:period, power:peri_out}
END ;kfme_pergram_min.pro




