pro   pg,tim,data,pguess,pkperiods,pkheights,title=title,noplot=noplot, $
      ymin=ymin

;For periods near specified period, pguess .

;   This is a power spectral estimation program that allows you to use
;   either the Scargle method or a minimization of chi square method
;   which returns the amplitude and phase of the best fit sine wave at
;   each trial frequency.
;   See paper: Gilliland,R.L. and Baliunas,S.L. 1987, Ap.J., v.314, p766.
;
;   Automatically attempts 3 offsets of velocities +- unc of the mean.

;INPUT
;      tim     times
;      data    data points at times

;OUTPUT
;      nu    output periods (or frequencies)
;      peri    output periodogram amplitude
;      pkperiods  array of 4 periods with the most power
;      pkheights  array of 4 highest points in power


;KEYWORDS
;     ymin   minimum height for y-axis.

if N_Params() lt 2 then begin
   print,'Syntax: pergram,tim,data,fq,peri[,amp,pha,unc]'
   retall
endif
if n_elements(title) eq 0 then title=' '
noplot=1

;SET FREQUENCY SAMPLING PARAMETERS
nu1 = 1./(pguess*1.3)           ;lowest freq. -> P = 20d
nu2 = 1./(pguess*0.7)           ;highest freq. -> P = 2. day
num = 50.*n_elements(tim)
dnu = (nu2-nu1)/num


;num = (nu2 - nu1)/dnu        ;number of frequency points to sample

;Define our grid of frequencies
ind = findgen(num)
nu = nu1 + ind * dnu         ;nu array = initial nu + increments

ndim = n_elements(data)
rndim = float(ndim)

;Subtract the mean off the data
;Revised aug 15
;mn = total(data)/rndim
mn = mean(data)

unc_mean = stdev(data)/sqrt(ndim)  ;uncert. in mean

peri_arr = fltarr(num,3)  ;peri_power at 3 different offsets


;"Floating Mean" Section
;Here is where the floating zero-point is explored, 
; and chooses the best. The idea is that the true mean
; of the population of values (velocity, S Values, etc.)
; remains unknown due to limited muber of data points.
; So, here we try three different zero-points, searching
; for the highest periodogram peak.

FOR offind=0,2 do begin    ;scan +-0.5 * (uncert. in mean)

  mean_offset = (1.-offind)*unc_mean  ;try 3 different offsets +- unc._mean
  dat = data-(mn + mean_offset)

;Scargle Test
 omeg = 2.0d0*!pi*nu
 ssin = fltarr(num)
 scos = fltarr(num)
 om2 = 2.*omeg
 for j = 0,ndim-1 do begin
    ssin = ssin+sin(om2*tim(j))
    scos = scos+cos(om2*tim(j))
 endfor
 tau=atan(ssin,scos)
 for j=0,num-1 do begin
     if (scos(j) lt 0) then begin
         tau = tau + !pi
     endif else begin
         if (ssin(j) lt 0) then tau = tau + 2.0*!pi  
     endelse
     
 endfor
 tau = tau/om2
 ocos = fltarr(num)
 ncos = fltarr(num)
 osin = fltarr(num)
 nsin = fltarr(num)
 omegtau = omeg*tau
 for j = 0,ndim-1 do begin
     ocos=ocos+dat(j)*cos(omeg*tim(j)-omegtau)
     ncos=ncos+(cos(omeg*tim(j)-omegtau))^2
     osin=osin+dat(j)*sin(omeg*tim(j)-omegtau)
     nsin=nsin+(sin(omeg*tim(j)-omegtau))^2
 endfor
 
;     Changed Aug 15 1999
;      var=total(dat^2)/(rndim-1.0)
 var = stdev(dat) & var = var^2
 
 ocos=ocos^2
 osin=osin^2
 peri=0.5*((ocos/ncos)+(osin/nsin))/var
 peri_arr(*,offind) = peri  ;load one of 3 periogram power for one offset.
ENDFOR   ;End trying 3 different offsets.

;Determine which of three offsets offers highest peak
max0 = max(peri_arr(*,0))
max1 = max(peri_arr(*,1))
max2 = max(peri_arr(*,2))

maxarr = [max0,max1,max2]
pkofpks = max(maxarr,offind)  ;peak of peaks (at 3 diff. offsets)

peri = peri_arr(*,offind)      ;final periodogram at offset (1-j)*unc_mean
mean_offset = (1.-offind)*unc_mean

;OK!  We have the basic periogram:  peri

;  Now compute the false alarm probability for the largest peak
 mx = double(max(peri,nm))
 ni = -6.4+1.19*rndim+0.00098*rndim^2
 prob = 1.0-(1.0-exp(-mx))^ni
 npr = 1/nu(nm)                     ;peak period
 pkprob=prob
 
;   Peak of Periodogram
 perimax = max(peri,maxloc)
 
 xo = min(1./nu) + 0.1*(max(1./nu) - min(1./nu))
 yo = max(peri)
 nm = perimax
 nuorig = nu
 periorig = peri
 
;Examine Highest Peaks
 npk = 4                        ; num peaks
 pkperiods = fltarr(npk)
 pkperiods(0) = npr 
 nperi = peri
 nnu   = nu 
;   print,npr, ' ',prob
 for q=1,npk-1 do begin         ;find next highest peaks
     i=where(nnu le nnu(maxloc)-4.*dnu or nnu ge nnu(maxloc)+4.*dnu,npts)
     if npts ge 1 then begin    ;avoid case of few velocity points
         nperi = nperi(i)       ;trim out peak
         nnu = nnu(i)           ;trim out peak
     end
     
     mx = double(max(nperi,maxloc)) ;next highest maximum
     prob = 1.0 - (1.0-exp(-mx))^ni
     pkperiods(q) = 1./nnu(maxloc)
 end	    
 
;PLOT
!p.thick=1
!p.charsize=2.
!x.thick=1
!y.thick=1
!x.charsize=1.2
!y.charsize=1.2
!p.charthick=2
 yra = [-0.5,1.2*max(peri)]
 xra = [min(1./nu),max(1./nu)]
 if keyword_set(ymin) then yra(1) = max([yra(1),ymin]) ;minimum yrange

if not keyword_set(noplot) then begin
    plot,(1./nu),peri,xtitl='!6Period (days)', $
      /xlog,/nodata,yr=yra,xr=xra, $
      /xsty,/ysty,titl='!6'+title,ytitl='!6 Power' ;,co=30
    oplot,(1./nu),peri
end
;END PLOT
;ZOOM IN ON PEAKS
pkperiods = [pkperiods]
pkheights = pkperiods*0.
pkprob    = pkperiods*0.

npks = n_elements(pkperiods)    ;number of peaks to examine
;print,' '
;print,'          Highest Peaks:'
;print,'       Period    F.-A. Prob.'
;print,'--------------------------------'

FOR ipk = 0,npks-1 do begin
    
                                ;SET FREQUENCY SAMPLING PARAMETERS
    timlen = max(tim) - min(tim) ;duration of string
    dnu = 1./(4.*timlen)        ;OLD nu spacing to assure: phase lag < pi/2 rad
    nu0 = 1./pkperiods(ipk)
    
                                ;Define our grid of frequencies
    nu1 = nu0 - 2.*dnu
    nu2 = nu0 + 2.*dnu
    dnu = 0.1*dnu               ;New frequency intervals, 1/10 as big
    num = (nu2 - nu1)/dnu       ;number of frequency points to sample
    ind = findgen(num)
    nu = nu1 + ind * dnu        ;nu array = initial nu + increments
    
    ndim = n_elements(data)
    rndim = float(ndim)
    
                                ;Subtract the mean off the data
    
    mn = total(data)/rndim
    dat = data-(mn+mean_offset)
    
;Scargle Test
    omeg = 2.0*!pi*nu
    ssin = fltarr(num)
    scos = fltarr(num)
    om2 = 2.*omeg
    for j = 0,ndim-1 do begin
        ssin = ssin+sin(om2*tim(j))
        scos = scos+cos(om2*tim(j))
    endfor
    tau=atan(ssin,scos)
    FOR j=0,num-1 do begin
        if (scos(j) lt 0) then begin
            tau = tau + !pi
        endif else begin
            if (ssin(j) lt 0) then begin
                tau = tau + 2.0*!pi
            endif
        endelse
    ENDFOR
    tau = tau/om2
    ocos = fltarr(num)
    ncos = fltarr(num)
    osin = fltarr(num)
    nsin = fltarr(num)
    omegtau = omeg*tau
    for j = 0,ndim-1 do begin
        ocos=ocos+dat(j)*cos(omeg*tim(j)-omegtau)
        ncos=ncos+(cos(omeg*tim(j)-omegtau))^2
        osin=osin+dat(j)*sin(omeg*tim(j)-omegtau)
        nsin=nsin+(sin(omeg*tim(j)-omegtau))^2
    endfor
;  New Aug 15, 1999
;      var=total(dat^2)/(rndim-1.0)
    var = stdev(dat) & var = var^2
    
    ocos=ocos^2
    osin=osin^2
    peri=0.5*((ocos/ncos)+(osin/nsin))/var
    
;  now we can compute the false alarm probability for the largest scargle
;  peak
    
    mx = double(max(peri,nm))
    
    
    ni = -6.4+1.19*rndim+0.00098*rndim^2
    prob = 1.0-(1.0-exp(-mx))^ni
    npr = 1/nu(nm)                      
;    print, npr,prob
    
    pkperiods(ipk) = npr
    pkheights(ipk) = mx
    pkprob(ipk) = prob
    
    pkperst = strmid(strtrim(string(npr),2),0,6)
    fap = strmid(strtrim(string(prob*100.),2),0,3) ;false alarm prob
;   xyouts,pkperiods(ipk), 1.0*pkheights(ipk), ' '+pkperst+' d'+' ('+fap+'%)',size=0.4
;   xyouts,pkperiods(0), pkheights(0), ' '+pkperst+' d'+' ('+fap+'%)',size=0.6
    if ipk eq 0 then begin
;       xyouts,pkperiods(ipk)-7, 1.*pkheights(ipk)+0.3, ' '+pkperst+' d',size=1.4
    end

; if ipk eq 0 then  xyouts,pkperiods(ipk), 1.0*pkheights(ipk), ' '+pkperst+' d',size=1.3
END
;print,'--------------------------------'

if not keyword_set(noplot) then begin
    pkperst = strmid(strtrim(string(pkperiods(0)),2),0,6)
   dum = max(pkheights,imax)
   pkper = pkperiods(imax)
   pkht  = pkheights(imax)
   xpos = pkper
   ypos = pkht*1.05
   if xpos gt 0.7*xra(1) then xpos = 0.2*xra(1)
   xyouts,xpos, ypos, ' '+pkperst+' d',size=0.9

    oplot,[pkperiods],[pkheights],ps=8,symsize=0.5
end
;   oplot,[pkperiods(0)],[pkheights(0)],ps=8,symsize=1.3
   
;HARDCOPY
   ans = ' '
;read,'Make hard copy? (y,n)',ans
   if ans eq 'y' or ans eq 'Y' then begin
       ps_open,'junk'
       print,'junk.ps opened'
                                ;PLOT
       !p.charsize=1.
       if not keyword_set(noplot) then begin
           plot,(1./nuorig),periorig,xtitl='!6Period (days)', $
             /xlog,/nodata,yr=[-.5,1.1*max(periorig)],xr=[min(1./nuorig),max(1./nuorig)], $
             /xsty,/ysty,titl='!6'+title
           oplot,(1./nuorig),periorig
           oplot,pkperiods,pkheights,ps=8,symsize=1.
       end
       for ipk = 0,npk-1 do begin
           pkperst = strmid(strtrim(string(pkperiods(ipk)),2),0,6)
           fap = strmid(strtrim(string(pkprob(ipk)*100.),2),0,3) ;false alarm prob
                                ; xyouts,pkperiods(ipk), 1.0*pkheights(ipk), ' '+pkperst+' d'+' ('+fap+'%)',size=1.3
           xyouts,pkperiods(ipk), 1.0*pkheights(ipk), ' '+pkperst+' d',size=1.3
       end
       
       ps_close
       spawn,'lp junk.ps'  & print,'Plot ',title,' sent to printer'
   end 
   
end



