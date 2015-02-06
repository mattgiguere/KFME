 ;Fits Keplerian Velocity Curve to "CF" velocity results
 ;Typically the user must find To by trial and error
 
 ;INPUT
 ;      cf           Optional input cf array.  This overrides keyword
 ;                   "star"
 ;      bank=bank    Use this keyword if you want to restore the file
 ;                   from the /mir3/vstbank/ directory instead of the
 ;                   /mir3/vel/ directory.
 ;      star=star         Standard star name
 ;      p=p            guess of period
 ;      /trend       searches for trends imposed on keplerian curve
 ;      /circle      forces e=0 ... no eccentricity
 ;      /init_e      guess initial value of eccentricity
 ;      /init_om     guess initial value of omega
 ;      /phase       plot data and best-fit PHASED
 ;      /manTp       force in guess of periastron passage time, Tp
 ;      manpk=manpk  auto set of JD-4490000 of peak of velocity curve
 ;      tspan=tspan  [start_time,end_time]
 ;      yrs=yrs      Plot time in yrs (not Julian Date)
 ;      mstar        Mass of star in solar masses (for Msini calc.)
 ;      extend       extend x-range to see 30% more
 ;      cfkey           input which cf: cfkey = 'cf1' or 'cf3'
 ;     resid        to plot residuals also.   
 ;     timebin      time bin size, ala velplot, typically 2/24. 
 ;     frz_ecc       value to which the eccentricity will be FROZEN:
 ;			e = frzecc
 ;     errcut       thresh = errcut * median(errvel) --- for removal of
 ;			poor points.
 ;     slide        invoke for blue background (for slides)
 ;     points       display points only, not fit (for overlay)
 ;     fit          display fit only, not points (works for unphased
 ;			data only)
 ;     parerr       computes uncertainties in parameters.
 ;     rem_line     remove trend
 
 ;OUTPUT
 ;      par = [p, tp, e,om,k,gamma]  ;DBL precision
 ;      time         times of observations
 ;      obvel        velocities (binned in 1 hr)
 ;      errv         internal errors in velocities
 
 ;ROUTINES   --- in /mir1/gmarcy/kepler
 ;      rv_marq.pro         Non-Linear Least Squares Routine
 ;      rv_drive.pro     Drives RV.PRO
 ;      rv_drive_tr.pro  Drives RV.PRO with a trend
 ;      rv.pro           Computes Keplerin Velocity Curve
 ;      velplot.pro      Used to obtain BINNED velocities (in hour bins)
 ;      monte            Used to compute uncertainties in parameters
 ;
 ;USER NOTE:  To operate rv_fit without a CF structure,
 ;            simply start the program below at "TIMES", with t, obvel, 
 ;		sig.
 
 function rv_fit_func, $
 cf, $
 par_starname,$
 pergram_resid, $ ;end of variables and onto keywords...
 aat=aat, $
 cfkey=cfkey, $
 charsize=charsize, $
 chi=chi, $
 circle=circle, $
 color=color, $
 debug = debug, $
 encap=encap, $
 ephem=ephem, $
 errcut=errcut, $
 errv=errv, $
 extend=extend, $
 findobs=findobs, $
 fit=fit, $
 frz_ecc=frz_ecc, $
 frz_k=frz_k, $
 frz_per=frz_per, $
 frz_pk=frz_pk, $
 init_e=init_e , $
 init_om=init_om, $
 init_tp=init_tp, $
 jd=jd, $
 keck=keck, $
 lastpt=lastpt, $
 lick_keck=lick_keck, $
 manpk=manpk, $
 manTp=manTp, $
 mstar=mstar, $
 nobank=nobank, $
 nobs_bin=nobs_bin, $
 note=note, $
 noxyouts=noxyouts, $
 nozero=nozero, $
 obvel=obvel, $
 outparam=outparam, $
 paper=paper, $
 param=param, $
 parerr=parerr, $
 parout=parout, $
 period=period, $
 phase=phase, $
 points=points, $
 postscript=postscript, $
 rem_line=rem_line, $
 resid=resid, $
 rms=rms, $
 slide=slide, $
 subaru_keck=subaru_keck, $
 star=star, $
 thfit=thfit,$
 tim2calc = tim2calc, $
 timebin=timebin, $
 title=title, $
 trend=trend, $
 tspan=tspan, $
 two_obs=two_obs,$
 yrange=yrange, $
 web=web
 
 if keyword_set(star) then begin
   if strmid(star, 0, 2) eq 'HD' then begin 
	 hdstar = star
	 star = strmid(star, 2, strlen(star))
   endif
 endif
 
 if not keyword_set(title) then title = ''
 outtitle = title	; save for use at end
 print,' '
 print,'Common Keywords:'
 print,' phase, title, circle, charsize, color, mstar, mantp, jd, resid, timebin '
 print,' errcut, init_e,frz_ecc '
 print,' '
 
 color_orig = !p.color
 back_orig  = !p.background
	black = 0
	darkblue = 15
	blue = 40
	purple = 50
	red = 70
	orange = 85
	lightorange = 100
	algaegreen = 122
	yellow = 130
	white = 255
 if keyword_set(color) then begin   ;COLOR SETTINGS
	loadct,5, /silent
 
   !p.background = 255  ;white  background
   !p.color = blue      ;DEFAULT COLOR
   pcolor=!p.color
   col = [30, 50, 70, 85, 100, 120, 130] 
   nocolor=0
 ENDIF ELSE begin	;BLACK & WHITE SETTINGS
 ;  loadct,0
   !p.color = 0        ;black
   !p.background = 255 ;white background
   col = [0, 0, 0, 0, 0, 0, 0]   ;black 
   nocolor = 1         ;in case "nocolor" arises.
 ENDELSE
 
 ;stop1
 
 ans=' '
 xticks_orig = !x.ticks
 xtickv_orig = !x.tickv
 
 cf_arg_input = n_params()
 if cf_arg_input eq 1 and n_elements(cf) eq 0 then begin
   print,'The cf argument on the input line contains nothing.'
   print,'Please check that cf3 or cf1.  Perhaps your VST does not ', $
   	'have it.'
   print,'Returning...'
   stop
 end

if n_elements(cf) eq 0 then begin
 ;RESTORE CF  (if not input in argument list)
   if keyword_set(nobank) then begin 
     path='/mir3/vel/vst' 
   endif else path='/mir3/vstbank/vst/'
 if keyword_set(keck) then begin
   if ~keyword_set(bank) then begin 
     path='/mir3/vel/vst' 
   endif else path='/mir3/vstbank/'
   
 ;  cfkey = 'cf1'
   cfkey = 'cf3'  ;new cf3 use, for rawcts correction.
 endif

 ;Insert star name, if not specified
; if keyword_set(star) then begin
;	 fn = path+star+'.dat'
;	 restore,fn
;	 restore, '/mir3/vel/cfresid.dat'
; end
 if not keyword_set(star) and n_elements(cf) eq 0 then begin
	 read,'Give name of star, i.e., 8729, gl380: ',ans
	 star = ans
	 fn = path+star+'.dat'
	 restore,fn
 end
 endif;no cf
 
 ;Now we have either a restored vst, or an input cf = cf1 or cf3
 ; In the latter case, we need not ask for specific cf1/cf3.
 
 
 if n_elements(cf) eq 0 then begin
 if keyword_set(cfkey) then begin
	 if cfkey eq 'cf1' then cf=cf1
	 if cfkey eq 'cf3' then cf=cf3
	 if cfkey ne 'cf1' and cfkey ne 'cf3' then begin
		 read,'Want cf1 or cf3? ',ans
		 if ans eq 'cf1' then cf=cf1
		 if ans eq 'cf3' then cf=cf3
	 end
 end else begin
	 read,'Want cf1 or cf3? ',ans
	 if ans eq 'cf1' then cf=cf1
	 if ans eq 'cf3' then cf=cf3 
 endelse
 end ;no cf
	 
 ;END
 
 cforig=cf
 
 duration = cf(n_elements(cf.jd)-1).jd - cf(0).jd
 if keyword_set(frz_per) then period = frz_per
 if keyword_set(period) then begin
   p = period
 ;  if (period lt 0.05*duration and n_elements(resid) lt 1) then phase = 1
 end else begin
   read,'Guess Orbital Period (d): ',p
 endelse
 
 ;Defaults:
 yrs=1    ;unless /jd is set
 if keyword_set(jd) then yrs=0
 if n_elements(title) lt 1 then title = ' '
 
 
 ;ERRCUT SECTION
 if not keyword_set(errcut) then errcut=2.5
 ; else errcut = errcut
 
 ;VELPLOT: Bin Velocities within intervals of 2 hours
 inpcf = cf
 tbin = 2./24.  ;2 hours
 if keyword_set(timebin) then tbin = timebin
 
 if not keyword_set(nozero) then nozero = 0
 if keyword_set(paper) then $
 velplot,inpcf,' ',tbin,d,v,e,n,c,bincf,errcut=errcut,/noplot,/table
 velplot,inpcf,title,tbin,time,vel,errv,dum,nav,bincf,/noplot, $
	 errcut=errcut,nozero=nozero
 ;stop
 
 nobs=n_elements(time)
 nobs_bin = nobs
 
 
 ;Restrict Times  (TSPAN)
 if keyword_set(tspan) then begin
   i = where(bincf.jd ge tspan(0) and bincf.jd le tspan(1), numobs)
   bincf = bincf(i)
 endif
 
 ;Order Them
 t=bincf.jd
 i=sort(t)
 bincf = bincf(i)
 
 ;USER NOTE:  To operate rv_fit without a CF structure,
 ;            simply start the program here, with t, obvel, sig.
 ;TIMES
 t = double(bincf.jd)                 ;times of observation
 tmin = min(t)
 tlen = max(t)-min(t)
 
 ;VELOCITIES and ERRORS
 obvel = bincf.mnvel         ;observed velocities for each observation
 newmaxvel = 0.5*(max(obvel)-min(obvel))
 dvel = 0.
 if not keyword_set(nozero) then begin  ;don't muck with vel zero-point
	 dvel = newmaxvel - max(obvel)
	 obvel = obvel +dvel
	 bincf.mnvel=obvel
 end
 ;obvel = obvel - median(obvel)
 
 
 ;Remove Linear Fit
 if keyword_set(rem_line) then begin
	cof = poly_fit(t,obvel,1)
	linfit = poly(t,cof)
	plot,t,obvel,ps=8
	oplot,t,linfit
	wait,1
	obvel = obvel - linfit
	 
	dvel = 0.5*(max(obvel)-min(obvel)) - max(obvel)
	obvel = obvel +dvel+fudge
 
	print,' '
	print,'Linear fit removed from velocities.'
	print,' '
 stop
 end

 sig = bincf.errvel          ;errors
 obvel = double(obvel)
 sig=double(sig)
 t = double(t)
 
 ;GUESS PARAMETERS
 ; P = Given in input          ;Period (in days)
  p = double(p)
  Tp = double(t(0))              ;Periastron passage
 ; e = 0.15d0                    ;eccentricity
  e = 0.05d0                    ;eccentricity
  om = 1.d0                    ;little omega (deg)
 ; K = sqrt(2.d0)*stdev(obvel)  ;Vel amplitude (m/s)
 ; K = 0.5d0*(max(obvel)-min(obvel) - median(sig))  ;Vel amplitude (m/s)
  K = 1.4d0*median(abs(obvel))
  gamma = 0.d0                 ;center of mass vel (m/s , arbitrary)
  gamma = median(obvel)
  dvdt = 0.d0                  ;linear slope (m/s per day) added to theor curve
 
 ;Initialize trend at linear fit slope
 if keyword_set(trend) then begin
   cof = poly_fit(t,obvel,1)
   dvdt = cof(1)
   if abs(dvdt) gt 1. then dvdt = 1.d0*dvdt/abs(dvdt)  
   ;set to + or - 1 m/s/day
   print,'Initial dvdt = ',dvdt, ' m/s'
 end
 
 
 ;ADJUST INITIAL PARAMETERS
   par = [p, tp, e,om,k,gamma]
   par = double(par) 
 
 ;New!
 ;Tune inital guess of period, if P < 10 d.
   if p lt 200 then begin        ; use periodogram within 30% of P, to nail period:
	   pguess = p
	   pg,t,obvel,pguess,pkperiods ;periodogram, centered on pguess +- 30%
	   p = pkperiods(0)          ;period at tallest peak
	   par(0) = p
   end
 
   if keyword_set(init_om) then om = init_om
   if keyword_set(init_e) then e = init_e
 
 ;Adjust Tperi, based on cursor
  ;Peak of curve from input parameters
 
 
   IF keyword_set(manTp) then begin ;manual periastron time
	   tp = double(mantp)        ;force periastron time in
	   tp = tp-40000.d0
   END ELSE BEGIN
	  dum = max(obvel,indmax)
	  par(1) = t(indmax)
  END 
 
   if keyword_set(init_tp) then tp = init_tp
	  print,'New Tperi:',tp
 
   par = [p, tp, e,om,k,gamma]
   if keyword_set(trend) then par = [p, tp, e,om,k,gamma,dvdt]
 print,dvdt
 ;stop
 
 ;Partial Derivative Step Sizes
  dpar = par*0.000001d0
  dpar(0) = 0.01d0*par(0)/nobs  ;step for Period: Nyquist sampling of partial derivative
 
 ; dpar(0) = 0.1d0*par(0)^2/tlen  ;don't let fn get pi/2 out of phase
 ; dpar(0) = 0.1d0*par(0)
  if dpar(0) gt 0.05*tlen then dpar(0) = 0.05*tlen
 ; dpar(1) = 0.03*par(0)          ;step for Tp
  dpar(1) = 0.01 ;dpar(0)          ;step for Tp
  dpar(2) = 0.002d0        ;step ecc
  dpar(3) = 1.d0          ;step omega
  dpar(4) = 0.03d0*k      ;step K (m/s)
  dpar(5) = 1.d0          ;d gamma ...step is m/s
  if keyword_set(trend) then dpar(6) = 0.03d0*abs(dvdt)   ;step in dvdt (m/s/day)
   dparorig = dpar
 ;print,dpar
 ;stop
 
   if keyword_set(frz_ecc) then begin
	print,'Freezing the Eccentricity at',frz_ecc
	e=frz_ecc
	par(2) = frz_ecc
	dpar(2)=0.
	dparorig = dpar
   end
 
   if keyword_set(frz_k) then begin
	print,'Freezing K at',frz_k
	k=frz_k
	par(4) = frz_k
	dpar(4)=0.
	dparorig = dpar
   end
 
   if keyword_set(frz_per) then begin
	print,'Freezing the Period at',frz_per
	p=frz_per
	par(0) = frz_per
	dpar(0)=0.
	dparorig = dpar
   end
 
   if keyword_set(ephem) then begin
	  circle = 1  ;force circular
	  print,'/ephem on: Circular Orbit FORCED !!!'
	  phase=1
   end
 
   if keyword_set(circle) then begin
	 print,'Force Circular Orbit'
	 e = .00       ;freeze eccentricity at 0.
	 par(2) = .00  ;freeze eccentricity at 0.
	 om = 0.      ;freeze omega at 0.
	 par(3) = 0.  ;freeze omega at 0.
	 dpar(2) = 0. ;set dpar=0 to freeze in marquardt least squares
 ;    dpar(3) = 0. ;set dpar=0 to freeze in marquardt least squares
   dparorig = dpar
   end
 
 if keyword_set(frz_pk) then begin
	print,'Freezing Period=',p
	print,'Freezing K     =',k
	dpar(0) = 0.
	dpar(4) = 0.
   dparorig = dpar
 end
 
 initpar = par  ;remember...
 pararr = dblarr(n_elements(par),4)
 chiarr = dblarr(4)
 sigpararr = dblarr(n_elements(par),4)
 
 ;Cycle Through 4 values of little omega: 0, 90, 180, 270
  

 FOR jomega = 0,3 do begin
   par = initpar                     ;re-initialize all parameters
   dpar = dparorig
   om = 90.d0 * jomega + 5.         ;Guess omega =  5 + n*90 deg 
   par(3) = om
   if keyword_set(circle) then begin
 ;    om = 0.
	 par(3) = 0.
	 par(1) = initpar(1)+initpar(0)*(jomega/4.) ;cycle T_p
   end
   print,'Guess for omega: ',strtrim(string(fix(om)),2), ' deg'
 
   IF keyword_set(trend) then begin    ;use rv_drive_tr for trend inclusion
 ;    dpar(6) = 0.05        ;d dv/dt ...step is 0.05 m/s per day.
	 thvel = rv_marq('rv_drive_tr',t,obvel,sig,par,dpar, sigpar,chi=chi,/trace)
   end else begin
 ;  print,'in',jomega,dpar(2),par(2)

   thvel = rv_marq('rv_drive_old',t,obvel,sig,par,dpar, sigpar,chi=chi,/trace)
 
 ;  print,jomega,dpar(2),par(2)
   END
   pararr(*,jomega) = par      ;save first derived parameters
   chiarr(jomega) = chi      ;save first chisq
   sigpararr(*,jomega) = sigpar
 END  ;jomega
 
 ;Find best Fit among the four trials
 minchi = min(chiarr,imin)     ;Use minimum Chi from 4 initial omega's
 if strtrim(string(minchi),2) eq 'NaN' then begin
	 remove,imin,chiarr          ;remove the NaN element
	 minchi = min(chiarr,imin)   ;Use minimum Chi from 4 initial omega's
	 if strtrim(string(minchi),2) eq 'NaN' then begin
		 remove,imin,chiarr
		 minchi = min(chiarr,imin) ;Use minimum Chi from 4 initial omega's
	 end
 end
 par = pararr(*,imin)
 chi = chiarr(imin)
 sigpar = sigpararr(*,imin)
 

 ;Set Plotting to File
 
 if n_elements(star) lt 0.5 then star='rv_fit'
 
 !p.font=-1
 
 if keyword_set(postscript) and not keyword_set(encap) then begin      ; Make postscript file
	postname = ' '
	read,'Give name of postscript file (i.e., hd187123) ',postname
	!p.font=0
  if keyword_set(color) then begin ; color
	ps_open,postname,/color
  end else begin                   ; B&W
	ps_open,postname
  endelse
 end
 
 if keyword_set(encap) then begin      ; Make postscript file
	postname = ' '
	read,'Give name of encap ps file (i.e., hd187123) ',postname
	!p.font=0
  if keyword_set(color) then begin ; color
	ps_open,postname,/color,/encap
  end else begin                   ; B&W
	ps_open,postname,/encap,/color
 ;stop
  endelse
 end
 
 ;LAST ITERATION--- input is output from the 4 previous trials
 print,' '
 print,'Decreasing dpar''s '
   dpar = 0.2*dpar
 
   if keyword_set(circle) then begin
	par(3) = 0.
	par(2) = 0.   
   end
 
 IF keyword_set(trend) then begin    ;use rv_drive_tr for trend inclusion
 stop
   thvel = rv_marq('rv_drive_tr',t,obvel,sig,par,dpar, sigpar,chi=chi,/trace)
 end else begin
   thvel = rv_marq('rv_drive_old',t,obvel,sig,par,dpar, sigpar,chi=chi,/trace)
 END
 
 par(3) = par(3) mod 360.			;force 0 < omega < 360
 
 ;if keyword_set(circle) then par(3) = 0.d
 ;print, 'om is: ', par(3)
 ;print, 'added om =0 at this point.'
 ;stop
 
 if par(3) lt 0. then par(3) = par(3)+360.
 
   if n_elements(par) eq 6 then begin
	  par = [par, 0.]       ;tack on dv/dt = 0. --- no trend
	  sigpar = [sigpar,0.]
   end
 
 ;Convert output pars to names
 p = par(0)
 tp = par(1)
 e = par(2)
 om = par(3)
 k = par(4)
 gamma = par(5)
 dvdt = par(6)
 
 ;PLOTTING
 pt=1
 if keyword_set(fit) then pt = 0   ;Don't plot points; fit only
 
 !p.charsize=2
 sz=!p.charsize
 if keyword_set(charsize) then !p.charsize = charsize
 charsize=!p.charsize
 !x.charsize = 1.
 ;!x.charsize = 2.0
 !y.charsize = 1.2
 ;!y.charsize = 2.
 !p.thick=2*!p.charsize
 !x.thick=2*!p.charsize
 !y.thick=2*!p.charsize
 if keyword_set(postscript) then begin
	 !x.thick=4*!p.charsize
	 !y.thick=4*!p.charsize
 end  
 !p.charthick=2
 
 tinit = t(0)
 numpts = n_elements(t)
 tmin = min(t)
 tmax = max(t)
 lenx = tmax - tmin
 xt = '!6   Julian Date (-2450000)'
 yt = '!6   Velocity  (m s!u-1!n)'
 
 xr = [tmin-.05*lenx,max(t)+.15*lenx]
 
 ;THEORETICAL CURVE
 ;tfine = dindgen(5000)*1.3*(xr(1) - xr(0))/5000. + xr(0) - 1.d0
 tfine = dindgen(2000)*1.2*(xr(1) - xr(0))/2000. + xr(0) - 1.d0 
; stop
 ;29dec2002
 thvel = rv_drive_tr(tfine,par)    ;compute theoretical curve
 
 ;Correct for zero-pt offset coming from rv_drive_tr ;  29 dec 2002
  deltime = tfine - median(tfine)
  thvel = thvel - par(6)*deltime   ;undo last line of rv_drive_tr
 
  olddeltime = tfine - median(t)   ;using median of original times of obs.
  thvel = thvel + par(6)*olddeltime ;add in correction in the orig fit.
 ;End Corection
 
 minval = min([obvel,thvel])
 maxval = max([obvel,thvel])
 leny = maxval - minval
 yr = [minval - 0.25*leny,maxval + 0.38*leny]  & leny = max(yr) - min(yr)
 if keyword_set(resid) then begin
   yr(0) = yr(0) - 0.3*leny 
   yr(1) = yr(1) + 0.0*leny
   leny=yr(1)-yr(0)
 end
 
	   xarrorig = t
	   xarr=t
	 if keyword_set(phase) then yrs = 0
 
	 tjd = t  ;save jd times    ;29 dec 2002
	 IF keyword_set(yrs) then begin                             ; YEARS?
		tconst = 11544.5
		xarr = 2000. + (xarr - tconst)/365.25  
		tfine = 2000. + (tfine - tconst)/365.25  
 
		xt = '!6 Time  (Years)'
		if ( max(xarr) - min(xarr) ) lt 1.2 and not keyword_set(jd) then begin
		   xt='!6 Time - '+strtrim(fix(min(xarr)),2)+'  (Years)'
		   zero_yr = fix(min(xarr))
		   xarr =  xarr - zero_yr        ;Subtr. first year
		   tfine = tfine - zero_yr       ;Subtr. first year
		endif
		t=xarr
		tmin = min(t)
		tmax = max(t)
		lenx = tmax - tmin
		xr = [tmin-.05*lenx,max(t)+.15*lenx]
		if keyword_set(extend) then xr = [tmin-.05*lenx,max(t)+.6*lenx]
 
	 if lenx gt 1.3 then xtickform = '(F6.1)'
 ;    if lenx le 1.3 then xtickform = '(F6.2)'
	 if lenx le 1.3 then xtickform = '(F6.1)'
   ;  if max(t) gt 2.1 then xtickform = '(F6.1)'
	 if  lenx gt 3.5 then xtickform = '(I4)'
 endif ;years
 
 if keyword_set(two_obs) then begin
  keck=where(bincf.dewar eq 103,nk)
  lick=where(bincf.dewar ne 103,nl)
  if nk eq 0 or nl eq 0 then stop
 endif
 
 ;not phase
 IF not keyword_set(phase) then begin
 
  If keyword_set(color) then begin
    stop
	 plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt, $
	   titl=title,/nodata,xtickformat=xtickform,col = blue
	 if pt eq 1 and keyword_set(two_obs) then begin
	 psym,'diamond',/fill
	 oplot,t(keck),obvel(keck),symsize=0.8*!p.charsize,ps=8,col=blue
		 psym,'circle',/fill
	 oplot,t(lick),obvel(lick),symsize=0.8*!p.charsize,ps=8,col=red
	 stop
	 endif
  end else begin
  if keyword_set(debug) then stop
	 plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt, $
	   titl=title,/nodata,xtickformat=xtickform
	if not keyword_set(two_obs) then if pt eq 1 then oplot,t,obvel,$
	  symsize=0.8*!p.charsize,ps=8
	if keyword_set(two_obs) then begin
	 psym,'diamond',thick=4
	  if pt eq 1 then oplot,t(keck),obvel(keck),symsize=0.8*!p.charsize,ps=8
		 psym,'circle',/fill
	  if pt eq 1 then oplot,t(lick),obvel(lick),symsize=0.5*!p.charsize,ps=8
	   if keyword_set(debug) then stop 
	endif
 Endelse  ;color vs b&w
  print,' '
 
 

if keyword_set(debug) then stop 
;Error Bars
 !p.thick=1
 !p.font=blue
 if keyword_set(two_obs) then begin
	 tk=t(keck)  & obvelk=obvel(keck)  & sigk=sig(keck)
   for j=0,nk-1 do begin
	 if pt eq 1 then oplot,[tk(j),tk(j)],[obvelk(j)-sigk(j),obvelk(j)+sigk(j)],col=0
 end
   !p.font=red
   tl=t(lick)  & obvell=obvel(lick)  & sigl=sig(lick)
   for j=0,nl-1 do begin
	 if pt eq 1 then oplot,[tl(j),tl(j)],[obvell(j)-sigl(j),obvell(j)+sigl(j)],col=0
   end
 endif 
 if not keyword_set(two_obs) then begin
  If keyword_set(color) then begin
  if keyword_set(debug) then stop
	 plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt, $
	   titl=title,/nodata,xtickformat=xtickform,col = blue
	 if pt eq 1 then oplot,t,obvel,symsize=0.8*!p.charsize,ps=8,col=blue
  end else begin
	 plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt, $
	   titl=title,/nodata,xtickformat=xtickform
	if pt eq 1 then oplot,t,obvel,symsize=0.8*!p.charsize,ps=8
	if pt eq 1 then oplot,t,obvel,symsize=0.5*!p.charsize,ps=8,co=70
  if keyword_set(debug) then stop
  Endelse
  print,' '
 
 
 ;Error Bars
 !p.thick=1
 for j=0,numpts-1 do begin
   if pt eq 1 then oplot,[t(j),t(j)],[obvel(j)-sig(j),obvel(j)+sig(j)],col=0
 end
 end ;not two_obs
 END  ;not phase
 
	   xarr=tfine
 
 
 ;  if n_elements(par) eq 7 then if par(6) ne 0 then begin ;linear trend?
 ;    voffset = par(6) * median(tjd)    ;vel offset from rv_drive_tr (bottom)
 ;    thvel = thvel + voffset
 ;  endif
 
 IF not keyword_set(phase) and not keyword_set(points) then begin
	 if keyword_set(color) then begin
		oplot,tfine,thvel,co=darkblue-3,thick=1 
		if pt eq 1 then oplot,t,obvel,symsize=0.8*!p.charsize,ps=8,col=blue
		if pt eq 1 then oplot,t,obvel,symsize=0.6*!p.charsize,ps=8,col=purple+20
	 end else begin
		 oplot,tfine,thvel,thick=1., $
			syms=0.5*!p.charsize
	 end
 END               ;not phase
 ;stop
   thveli = rv_drive_tr(xarrorig,par)


if keyword_set(thfit) then begin
	tv=thvel
	tfine = dindgen(2000)*1.2*(xr(1) - xr(0))/2000. + xr(0) - 1.d0 ;29dec2002
	print, par
	
	;----------------------------------------------
	;this portion added by MJG 2007.09
	;mytfine = yearform2jd(tfine)
	if ~keyword_set(tim2calc) then tim2calc = systime(/julian)
	print, 'tim2calc is: ', tim2calc, ' or ', jul2cal(tim2calc)
;	stop
	mytfine = dindgen(par[0]*2400.d)/2400.+tim2calc - par[0]/2.
	thvel = rv_drive(mytfine, par)
	tv=thvel
	td=double(mytfine)   ; now not in yrs
	np=n_elements(tv)
	tharr=dblarr(np,2)
	tharr(*,0)=td
	tharr(*,1)=tv
	save,tharr,filename='thfit.dat'
end
;----------------------------------------------
  
 
 
 if keyword_set(findobs) then begin
	  print,'Click on point you wish to identify'
	  cursor,pkx1,pky1
	  dt = 3.   ;0.01*tlen         ;search time span for peak
	  i = where(t gt pkx1-dt and t lt pkx1+dt,n)
	  if n lt 1 then begin
		  dt = dt*2. & i = where(t gt pkx1-dt and t lt pkx1+dt,n)
	  end
	  obsnm=strarr(n)
	  obsnm = bincf(i).obnm
	  print,obsnm
	  oplot,t(i),obvel(i),ps=2,symsize=2,color=122
 endif
 
   thveli = rv_drive_tr(xarrorig,par)
   residu = bincf.mnvel - thveli
   dum = where(dpar gt 0., nfreepar)
   rms = sqrt(total(residu^2)/(n_elements(bincf) - nfreepar))
   residlev = yr(0)+0.17*leny    ;LEVEL of RESIDUALS
   residue = bincf.mnvel-thveli
 cfresid=bincf  ;initiate with jd's obnm, errvels
 cfresid.mnvel=residue
 save,cfresid,file='cfresid.dat'
 
 if keyword_set(resid) then begin
 if keyword_set(color) then begin
   oplot,t,residue + residlev,ps=1,symsize=1.8,co=red,thick=4
   oplot,t,obvel-obvel + residlev-2,co=0                        ;,co=70
 end else begin
   oplot,t,residue + residlev,ps=1,symsize=1.2
   oplot,t,obvel-obvel + residlev                  ;,co=70
 endelse
 end
 
 
 stp = strtrim(string(par(0)),2)   ;period (string)
 stp = strmid(stp,0,5) + ' day'
 if par(0) gt 365 then begin
   stp = strtrim(string(par(0)/365.),2)
   stp = strmid(stp,0,4) + ' yr'
 end
 
 stk   = strtrim(string(par(4)),2) ;K (amplitude) (string)
 stk = strmid(stk,0,4)
 
 ste   = strtrim(string(par(2)),2) ;eccentricty (string)
 ste = strmid(ste,0,4)
 if par(2) lt 0.005 then ste = '0.00'
 
 strms = strtrim(string(rms),2)
 strms = strmid(strms,0,4)
 stchi = strtrim(string(chi),2)
 stchi = strmid(stchi,0,4)
 
 IF not keyword_set(phase) then begin
	 sz=charsize
 
  if keyword_set(paper) then begin
	xyouts,xr(0)+0.054*lenx,yr(0)+0.914*leny,$
	 title,size=1.6 
	if keyword_set(lick_keck) then begin
	  xyouts,xr(1)-0.5*lenx,yr(0)+.05*leny,'!6 Lick & Keck',size=sz*0.9
	endif else xyouts,xr(0)+.7*lenx,yr(0)+0.05*leny, $
	 'Subaru Obs',size=1.6
	if keyword_set(note) then xyouts,xr(0)+0.05*lenx,yr(0)+0.9*leny,$
	 note,size=1.6
 ;   if keyword_set(note) then xyouts,xr(0)+0.05*lenx,180,note,size=1.6
  endif
 
 
	 if keyword_set(color) then !p.color=black
	 
	 if not keyword_set(noxyouts) and not keyword_set(points) then begin
		 xleft = xr(0)+0.73*lenx 
		 ytop  = yr(0) + 0.92*leny
		 xyouts,xleft,ytop     ,'!6 P = ' + stp ,size=sz*1.1
		 xyouts,xleft,ytop-0.06*leny,'!6 K = ' + stk +' ms!u-1!n',size=sz*1.1
		 xyouts,xleft,ytop-0.12*leny,'!6 e = ' + ste ,size=sz*1.1
		 xyouts,xr(0)+.04*lenx,yr(0)+.05*leny,'!6 RMS = ' + strms +' ms!u-1!n' ,size=sz*1.05
		 if keyword_set(keck) then begin
			 xyouts,xr(0)+.91*lenx,yr(0)+.05*leny,'!6 Keck',size=sz*0.9
		 end
		 if keyword_set(aat) then begin
			 xyouts,xr(0)+.91*lenx,yr(0)+.05*leny,'!6 AAT',size=sz*0.9
		 end
 ;        if (not keyword_set(aat)) and (not keyword_set(keck)) then begin
 ;            xyouts,xr(0)+.9*lenx,yr(0)+.05*leny,'!6 Lick Obs.',size=sz*0.9
 ;        end
		 
	 end ;paper
 END                             ;not phase
 
 ; if keyword_set(subaru_keck) then begin
 ;     xyouts,xr(1)-0.5,yr(0)+.05*leny,'!6 Subaru & Keck',size=sz*0.9
 ; end
 
 ;ANCILLARY QUANTITIES
 ;See Bowers and Deeming, pg. 317,318
 G = 6.67259d-11
 AU = 1.49598d11
 psec = p*24.*3600.
 msun = 1.9891d30  ;in kg
 mjup = 1.8987d27  ;in kg
 
 a1sini = k*psec*sqrt(1.-e^2)/(2.*!pi)   ;in meters
 ;print,arel & stop
 
 massfn = 4.*!pi^2*a1sini^3/(G*psec^2)/1.989d30 ;in Msun
 
 ;read,'Give Mass of the Star (Solar Masses):',mstar
 if not keyword_set(mstar) then mstar = 1.0
 
 ;Iterate to find m_2 sin i
 ;MASSFN defined:  massfn = [m_2/(m_star+m_2)]^2 * m_2 sini^3
 ;Thus expression for msini depends on m_2:
 ; msini = [massfn * (mstar + m_2)^2 ]^1/3
 ;First guess is the standard one, for m_2 << mstar:
	 msini = (massfn * mstar^2)^(1./3.)  ;in Solar masses, first estimate
 
 
 ;Now iterate twice, using msini for m_2:
	 msini = (massfn * (mstar+msini)^2)^(1./3.) ;in Solar masses
	 msini = (massfn * (mstar+msini)^2)^(1./3.) ;in Solar masses
 
 ;semimajor axis ;a_rel in AU
 ;arel = G * (mstar + msini) (psec/3.15d7)^2 / (4.*!pi^2)  
 arel = (mstar + msini) * (psec/3.15d7)^2 
 arel = arel^(1./3.)
 
	 msini = msini * (msun/mjup)   ;in Jupiter masses
 
 stmsini = strtrim(fix(msini*10000.)/10000.,2)
 ;stmsini = strtrim(fix(msini*1000.)/1000.,2)
 stmsini = strtrim(string(msini),2)
 stmsini=strmid(stmsini,0,4)
 
 
 origcol=0
 
 if keyword_set(color) then !p.color=origcol
 chart=!p.charthick
 xsz = !p.charsize * 0.8 *1.2
 
 IF not keyword_set(phase) and not keyword_set(points) then begin
  ;  if keyword_set(color) then begin
  ;   !p.color=0
  ;   xyouts,xr(0)+0.053*lenx,yr(0)+0.919*leny,'!6Mass = '+stmsini+' M!dJUP!n /sin!8 i!6',size=xsz
  ;  end else begin
  ;   xyouts,xr(0)+0.056*lenx,yr(0)+0.914*leny,'!6Mass = '+stmsini+' M!dJUP!n /sin!8 i!6',size=xsz
  ;  endelse
  
  if keyword_set(resid) then begin
	;compute slope
	coef = poly_fit(t,residue,1)
	fit = poly(t,coef)
  ;  oplot,t,fit + residlev,linesty=1  ;fit to residuals
	print,' '
	print,'Slope = ',coef(1), ' m/s per year'
  
	if keyword_set(postscript) or keyword_set(encap) then begin
		ps_close
  
		suf = '.ps &'
		if keyword_set(encap) then suf = '.eps &'   ;encapsulated ps
	endif ;postscript OR encap
  
	if keyword_set(pergram_resid) then begin
	  window,2
	  tday = (t-t(0))*365.25
	  pergram,tday,residue
	  wset,0
	endif ;pergram_resid
  endif  ;resid
  
 endif  ;~ phase AND ~points
 
 ;***************************PHASED****************************************

 IF keyword_set(phase) then begin    ;convert times to phases
;if keyword_set(debug) then stop 
   phpar=par
   offvel=0.
   if n_elements(par) eq 7 then if par(6) ne 0 then begin ;linear trend?
 ;new 19mar
 ;    obvel=obvel-(t-t(0))*par(6)    ;take out linear trends
 ;    obvel=obvel-(t-par(1))*par(6)    ;take out linear trends
 ;    voffset = par(6) * median(tjd)    ;vel offset from rv_drive_tr (bottom)
	 obvel=obvel-(tjd-median(tjd))*par(6)    ;take out linear trends 29dec2002
 ;     obvel=obvel-voffset    ;take out linear trends 29dec2002
 
	 dvel = 0.5*(max(obvel)-min(obvel)) - max(obvel)
	 obvel = obvel +dvel
	 phpar(6)=0                     ;remove linear trend from parameters
	 offvel=median(obvel-rv_drive_tr(t,phpar)) ;velocity offset after detrend
   endif
   tph = (t-t(0))/p - fix((t-t(0))/p)
   j1=where(tph gt 0.75,n1)
   j2=where(tph lt 0.25,n2)
 
   ;identify last point for emphasis
   numpts = n_elements(t)
   lastvel = obvel(numpts-1)
   thveli = rv_drive_tr(xarrorig,par)
   residu = obvel - thveli
 
 obsflag=intarr(n_elements(t))
 x=where(bincf.dewar eq 103,nx)
 y=where(bincf.dewar ne 103,ny)
; stop
 if nx gt 0 then obsflag(x) = 1
 obnm1=bincf.obnm
; stop
   if n1 gt 0 and n2 gt 0 then begin
	   t = [tph(j1)-1., tph, 1.+tph(j2)]    ;tack on pre/post-cycle phases
	   obvel = [obvel(where(tph gt 0.75)), obvel, obvel(where(tph le 0.25))]
	   residu = [residu(where(tph gt 0.75)), residu, residu(where(tph le 0.25))]
	   sig = [sig(where(tph gt 0.75)), sig, sig(where(tph le 0.25))]
	   obsflag = [obsflag(where(tph gt 0.75)), obsflag, obsflag(where(tph le 0.25))]
	   obnm1= [obnm1(where(tph gt 0.75)), obnm1, obnm1(where(tph le 0.25))]
   end
;stop
   if n1 le 0 then begin
	   t = [tph, 1.+tph(j2)]    ;tack on pre/post-cycle phases
	   obvel = [obvel, obvel(j2)]
	   residu = [residu, residu(j2)]
		sig = [sig, sig(j2)]
		obsflag = [obsflag,obsflag(j2)]
		obnm1 = [obnm1,obnm1(j2)]
   end

   if n2 le 0 then begin
	   t = [tph(j1)-1., tph] 
	   obvel = [obvel(j1), obvel]
	   residu = [residu(j1), residu]
	   sig = [sig(j1), sig]
	   obsflag = [obsflag(j1),obsflag]
	   obnm1 = [obnm1(j1),obnm1]
   end
 

;THIS SECTION CALCULATES THE THEORETICAL VELOCITY CURVE:
   tfine = p*(dindgen(301)-50.)/200. + tinit
   thvel = rv_drive_tr(tfine,phpar)    ;compute theoretical curve
;   offset_vel=(max(thvel) -min(thvel))/2.
;		midwy=-(max(thvel)-((max(thvel)-min(thvel))/2.))
midwy = 0
if keyword_set(debug) then stop
   thvel=thvel+midwy  ;+offvel
 

   tfine = (tfine-tinit)/p
 tfine = tfine+0.15
   numpts = n_elements(t)
   tmin = min(t)
   tmax = max(t)
   lenx = tmax - tmin
   minval = min(thvel)
   maxval = max(thvel)
   leny = maxval - minval
 
   xr = [tmin-.05*lenx,max(t)+.05*lenx] 
   if xr(1) lt 1.15 then xr(1)=1.15
   yr = [minval - 0.3*leny,maxval + 0.35*leny]  
   if keyword_set(yrange) then yr=yrange
   leny = max(yr) - min(yr)
 
   xt = star+'!6 Orbital Phase'
   yt = '!6   Velocity  (m s!u-1!n)'
 
   xticks_orig = !x.ticks
   xtickv_orig = !x.tickv
 ;  !x.charsize=1.
 ;  !y.charsize=1.2
   !x.ticks=2       ;make phases 0.0  0.5  1.0  
   !x.tickv=[0.0, 0.5, 1.0]
 ;PLOT PHASED VELOCITIES
  t = t+0.15  ;fudge
  if keyword_set(color) then loadct,5, /silent
  if not keyword_set(color) then loadct,0, /silent
  if not keyword_set(color) then !p.color=0
 
 ;!x.margin=[10,6]
 ;!y.margin=[6,6]
;if keyword_set(debug) then stop
  IF keyword_set(color) then begin
	plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt,titl=title, $
	   /nodata,col=blue-10,symsize=3
  END ELSE begin
	loadct,0, /silent
	!p.color=0
;if keyword_set(debug) then stop
	plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt,titl=title, $
	   /nodata,symsize=3,col=0
;if keyword_set(debug) then stop
  ENDELSE
 ;xyouts,xr(0)+0.72*(xr(1)-xr(0)),yr(0)+0.03*(yr(1)-yr(0)),'HD'+star,size=1.8
 
 
 xph=where(tfine gt 0.0 and tfine le 1.0)
  if keyword_set(color) then begin
   oplot,tfine,thvel,col=orange-10,thick=6.
   oplot,tfine(xph),thvel(xph),col=orange,thick=6.
   stop
  end else begin
;if keyword_set(debug) then stop
	loadct,0, /silent
	oplot,tfine,thvel,thick=6,col=200
	oplot,tfine(xph),thvel(xph),thick=6,col=0
 endelse
 

;FINAL PHASED PLOT
  if keyword_set(color) then begin
	  oplot,t,obvel+midwy,symsize=0.8*!p.charsize,ps=8,col=blue+10
	  oplot,t,obvel+midwy,symsize=0.55*!p.charsize, ps=8, col=darkblue
	if keyword_set(two_obs) then begin
		loadct,0, /silent
		psym,'diamond',thick=6
		oplot,t(where(obsflag eq 1)),obvel(where(obsflag eq 1))+midwy,symsize=1.8,ps=8,col=200
		oplot,t(where(obsflag eq 1 and t gt 0. and t le 1.0)),obvel(where(obsflag eq 1 and t gt 0. and t le 1.))+midwy,symsize=1.8,ps=8,col=0
		psym,'circle',thick=6
		oplot,t(where(obsflag ne 1)),obvel(where(obsflag ne 1))+midwy,symsize=1.8,ps=8,col=200
		oplot,t(where(obsflag ne 1 and t gt 0. and t le 1.)),obvel(where(obsflag ne 1 and t gt 0. and t le 1.))+midwy,symsize=1.8,ps=8,col=0
 
 ;       midwy=(max(thvel)-min(thvel))/2.
		plots,[0,1.2],[0.,0.],linesty=2   ;[max(thvel)-midwy,max(thvel)-midwy]
		if keyword_set(debug) then stop
	endif
if keyword_set(debug) then stop
	if keyword_set(lastpt) then begin ;plot last point with different symbol
		i=where(obvel gt lastvel-0.001 and obvel lt lastvel+0.001)
		oplot,[t(i)],[obvel(i)],co=140,ps=8,syms=1*!p.charsize
		if keyword_set(debug) then stop
	end
 
 end  else begin
; 'right before you plot the points.'
;if keyword_set(debug) then stop
 ; plot,t,obvel,xr=xr,/xsty,yr=yr,/ysty,xtit=xt,ytit=yt,titl=title,/nodata
	if not keyword_set(two_obs) then oplot,t,obvel+midwy,symsize=0.7*!p.charsize,ps=8
print, 'you should have just plotted the points.'
;if keyword_set(debug) then stop
	if keyword_set(two_obs) then begin
		loadct,0, /silent
		psym,'diamond',thick=6  ; keck points
		oplot,t(where(obsflag eq 1)),obvel(where(obsflag eq 1))+midwy,symsize=1.8,ps=8,col=200
		oplot,t(where(obsflag eq 1 and t gt 0. and t le 1.0)),obvel(where(obsflag eq 1 and t gt 0. and t le 1.))+midwy,symsize=1.8,ps=8,col=0
		psym,'circle',/fill     ; other obs points
		oplot,t(where(obsflag ne 1)),obvel(where(obsflag ne 1))+midwy,symsize=1.8,ps=8,col=200
		oplot,t(where(obsflag ne 1 and t gt 0. and t le 1.)),obvel(where(obsflag ne 1 and t gt 0. and t le 1.))+midwy,symsize=1.8,ps=8,col=0
 ;       midwy=(max(thvel)-min(thvel))/2.
		plots,[0,1.2],[0.,0.],linesty=2   ;[max(thvel)-midwy,max(thvel)-midwy]
		if keyword_set(debug) then stop
	endif
 endelse


  ;This section plots the Error Bars
  FOR j=0,numpts-1 do begin
	  loadct,0, /silent
	  !p.color=200
	  if t(j) le 0.0 or t(j) gt 1.0 then $
	  oplot,[t(j),t(j)],[obvel(j)+midwy-sig(j),obvel(j)+midwy+sig(j)],thick=1,col=200
 
	  !p.color=0
	  if t(j) gt 0.0 and t(j) le 1.0 then $
	  oplot,[t(j),t(j)],[obvel(j)+midwy-sig(j),obvel(j)+midwy+sig(j)],thick=1,col=0
  END
 
;if keyword_set(debug) then stop
 
 !p.thick=1
 ;Dashed lines in phased plot
 ;oplot,[0,0],yr*0.6,linestyle=2,thick=0.1
 ;oplot,[1,1],yr*0.6,linestyle=2,thick=0.1
 
 ecc=par(2)
 ecc=fix(ecc*100.)
 ecc=ecc/100.
 
 ecc=fix(ecc*1000.)
 ecc=ecc/1000.
 
 ste   = strtrim(string(ecc),2) ;eccentricty (string)
 ste = strmid(ste,0,4)
 if ecc lt 0.005 then ste = '0.00'
 
 slick = '!6 Lick Obs.'
 
 ;ANNOTATION  of "ORBITAL PHASE" plot
   if keyword_set(color) then !p.color=0
   xsz = !p.charsize*0.9
   !p.color = black
   ytop  = yr(0) + 0.92*leny
 
 if not keyword_set(noxyouts) then begin
   xyouts,xr(1)-0.6,ytop     ,'!6 P = ' +stp ,size=xsz*1.
   xyouts,xr(1)-0.6,ytop-0.08*leny,'!6 K = ' + stk +' m s!u-1!n',size=xsz*1.
   xyouts,xr(1)-0.6,ytop-0.16*leny,'!6 e = ' + ste ,size=xsz*1.
 endif
 ;
 ;Added without thought: 5 Nov 2002
 ;  if keyword_set(keck) then begin
 ;     xyouts,xr(1)-0.45,yr(0)+.05*leny,'!6 Keck',size=sz*0.7
 ;  end
  if keyword_set(lick_keck) then begin
	  xyouts,xr(1)-0.5,yr(0)+.05*leny,'!6 Lick & Keck',size=sz*0.9
  end
  if keyword_set(subaru_keck) then begin
	  xyouts,xr(1)-0.5,yr(0)+.05*leny,'!6 Subaru & Keck',size=sz*0.9
  end
 
 
 
 if not keyword_set(noxyouts) then begin
 ; if keyword_set(color) then begin
 ;   !p.color=0
 ;   xyouts,xr(0)+0.03*lenx,yr(0)+.917*leny,'!6Mass = '+stmsini+' M!dJUP!n /sin!8 i!6',size=xsz
 ; end else begin
 ;   xyouts,xr(0)+0.03*lenx,yr(0)+.92*leny,'!6Mass = '+stmsini+' M!dJUP!n /sin!8 i!6',size=xsz*0.85
 ; endelse
 
  xyouts,xr(0)+0.1*lenx,yr(0)+0.04*leny,'!5 RMS = '+strms+' m s!u-1!n',size=xsz*1.05
  xyouts,xr(0)+0.3*lenx,yr(0)+0.04*leny,'!5 Chi = '+strt(chi)+' m s!u-1!n',size=xsz*1.05
   ; xyouts,xr(0)+0.84*lenx,yr(0)+0.80*leny,'!7v!u2!n!6 = ' + stchi,size=xsz*0.5
 endif
 
 if keyword_set(paper) then begin
	xyouts,xr(0)+0.054*lenx,yr(0)+0.914*leny,$
	 title,size=1.6 
 ;   xyouts,xr(0)+.7*lenx,yr(0)+0.05*leny, $
 ;	'!6Lick and Keck Obs',size=1.6
	if keyword_set(note) then xyouts,xr(0)+0.1*lenx,yr(1)-0.1*leny,note,size=1.7
 ;   if keyword_set(note) then xyouts,0.05,30.,note,size=1.7
 ;   if keyword_set(note2) then xyouts,xr(0)+0.1*lenx,yr(1)-0.1*leny,note2,size=1.7
 endif
 
	
 END  ;phase
 
	 parout = [par(0:4),msini,arel,rms,chi]
 
 if star eq '145675' then begin
   !p.color=40
 ;  !p.charthick=4
   xyouts,1.01,-190,'!6 Keck/HIRES',size=1.2
 end
 
 
   if keyword_set(postscript) or keyword_set(encap) then begin
	   ps_close
	   suf = '.ps &'
	   if keyword_set(encap) then suf = '.eps &'   ;encapsulated ps
	   !p.font=-1
   end
 
 if keyword_set(ephem) then begin
	 per = double(par(0))
	 tperi = double(par(1)) + 40000.d0
	 kvel = double(par(4))
	 ecc = par(2)
	 if ecc gt 0.001 then begin
		 print,'*** Cannot compute ephemeris unless /circle invoked.'
		 print,' Stopping rv_fit'
		 stop
	 end
	 ephem,bincf,per,tperi,kvel,/nomssg
 end
 
 if keyword_set(parerr) then begin
	   par = [p, tp, e,om,k,gamma]
 ;pro monte,N,cf,p,tp,e,om,k,gamma,dvdt,sigpar,hist=hist,circle=circle
   N = 200   ;Number of monte Carlo trials
   inpcf = cforig
 ;  intp = par(1)+40000.
 ;Augment the values of cf.errvel to reflect jitter via the empiricial RMS
 ;augrms_2 = 9.4    ;rms^2 - median(inpcf.errvel)^2 
 ;print,inpcf.errvel
 ;orbpar=[par(0),par(1),par(2),par(3),par(4),par(5)]
 ;if augrms_2 gt 0. then inpcf.errvel = sqrt(inpcf.errvel^2 + augrms_2)
 end
 


testvel = cf.mnvel
newmaxvel = 0.5*(max(testvel)-min(testvel))
dvel = newmaxvel - max(testvel)
testvel += dvel

thvel1 = rv_drive_tr(cf.jd, par)

resid = cf.mnvel-thvel1
resid = testvel-thvel1

dum = where(par gt 0., nfreepar)
rms = sqrt(total(resid^2)/(n_elements(cf) - nfreepar))
print, 'rms of cf and thvel: ', rms


testvel2 = bincf.mnvel
newmaxvel2 = 0.5*(max(testvel2)-min(testvel2))
dvel2 = newmaxvel2 - max(testvel2)
testvel2 += dvel2

thvel2 = rv_drive_tr(bincf.jd, par)

resid = bincf.mnvel-thvel2
resid = testvel2-thvel2

dum = where(par gt 0., nfreepar)
rms = sqrt(total(resid^2)/(n_elements(bincf) - nfreepar))
print, 'rms of bincf and thvel2: ', rms

if keyword_set(debug) then stop

 ;Print Results
 fo='(A7,F10.4,A3,F8.3)'
 fo='(A7,F12.6)'
 print,' '
 print,'RMS to Fit = '+strms
 print,'Chi to Fit = '+stchi
 print,' '
 print,'  FINAL ORBITAL PARAMETERS'
 print,'------------------------------ '
 print, outtitle
 print,'------------------------------ '
 print,form=fo,'P = ',par(0) ;,' +-',sigpar(0)
 print,form=fo,'Tp = ',40000.+par(1) ;,' +-',sigpar(1)
 print,form=fo,'e = ',par(2) ;,' +-',sigpar(2)
 print,form=fo,'om = ',par(3) ;,' +-',sigpar(3)
 print,form=fo,'k = ',par(4) ;,' +-',sigpar(4)
 print,form=fo,'gam = ',par(5) ;,' +-',sigpar(5)
 print,form=fo,'dvdt = ',par(6) ;,' +-',sigpar(6)
 print,'a1sini=',a1sini, ' m = ',a1sini/AU,' AU'
 print,'Mass_Fn=',massfn,' Msun'
 
 print,'M2 sini = ',stmsini,  ' M_JUP'
 ;print,'M2 sini=',massfn^(1./3.) * (1.989e3/1.8991),'*[(M1+M2)/Msun]^2/3  M_JUP'
 ;print,'M2 sini=',massfn^(1./3.) * (1.989e3/1.8991),'*[(M1+M2)/Msun]^2/3  M_JUP'
 ;print,'N_obs=',n_elements(obvel)
 print,'N_obs = '+strtrim(nobs,2)
 print,'-------------------------------'
 print,' '
 
 ;if keyword_set(color) then loadct,5
   !x.ticks = xticks_orig
   !x.tickv = xtickv_orig 
 ;  !p.color = color_orig 
 ;  !p.background = back_orig  
 
 
print, 'gam is: ', par[5]

if n_elements(tharr) le 0 then tharr = fltarr(10,10)

if n_elements(hdstar) gt 0 then star = hdstar

 retvals = create_struct('par', par, 'vstar', tharr[*,1], 'vtimes', $
 tharr[*,0], 'nobs', nobs, 'a1sini', a1sini, 'massfn', massfn, $
 'msini', msini, 'mstar', mstar, 'rms', rms, 'chi', chi)
 
if keyword_set(debug) then stop 

 return, retvals
 end ;rv_fit_func.pro