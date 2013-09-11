PRO lspergram,tim,data,nu_out,peri_out,pkperiods,pkheights,title=title,     $
            fap=fap,signi=signi, omega = omega, $
            lowper=lowper,pmax=pmax,                   $
            noise=noise, numf = numf_in
;+
; NAME:
;         lspergram
;
; PURPOSE:
;         Compute the lomb-scargle periodogram of an unevenly sampled
;         lightcurve. 
;
;         This was originally written by Joern Wilms as pergram. This 
;         was slightly modified by John Johnson, to FOR loops, but
;         retaining Wilms' original simulation of "multiple" trials
;         for the FAP (simsigni).  JTW forked this version into lspergram,
;         retaining only the features necessary for its quick-and-dirty
;         implementation in rv_fit_mp.
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
;         numf: number of frequencies sampled
;      
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
;          Version 2.0,  2008 JTW: Forked lspergram from pergram for use with rv_fit_mp.
;      
;-

   ;; make times manageable (Scargle periodogram is time-shift invariant)
   time = tim-tim[0]
   if keyword_set(lowper) then pmin = lowper ;set orig keyword

   ;; defaults
   IF n_elements(noise) EQ 0 THEN noise = double(sqrt((moment(data))[1]))
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
           pmin = 2.0d0   ; Default shortest period ( 2 days )
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
numf = fix((fmax - fmin)/dnu+1)  ;Number of sampled frequencies.
if n_elements(numf_in) gt 0 then numf = numf > numf_in

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

   
;Johnjohn's replacement 
   ;; Eq. (5); sh and ch
   c_arr = fan(cn, numf, /trans)
   sh = total( c_arr * sin( om_arr * t_arr ), 2)
   ch = total( c_arr * cos( om_arr * t_arr ), 2)

   ;; Eq. (3)
   px = (ch*cosomtau + sh*sinomtau)^2  / tc2 + $
     (sh*cosomtau - ch*sinomtau)^2  / ts2     
   
   ;; correct normalization 
   px = 0.5D0*px/(noise^2)
   
   ;; some other nice helpers
   ;; computed here due to memory usage reasons
   nu     = om/(2.D0*!dpi)
   period = 1.D0/nu
   nu_out = nu
   peri_out = px  ;for output
  
       ymax = max(px, imx)
       xmax = period[imx]
;       i = reverse(sort(px))
       pxpki = where(shift(px, -1) lt px and shift(px, 1) lt px)
       i = reverse(pxpki[sort(px[pxpki])])
       pkperiods = period(i[0:9])  ;periodsof 5 tallest peaks
       pkheights = px(i[0:9])      ;heights of them
       ni_horne = horne
       faps = 1.0d0 - (1.0d0 - exp(-pkheights))^ni_horne
;       st_faps = strmid(strtrim(string(faps*100.),2),0,3) ;false alarm p



  
END 




