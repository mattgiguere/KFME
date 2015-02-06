pro ephem,cf,p,tp,k,nomssg=nomssg

;pro ephem,cf,p,tp,k,corr=corr
;Generate Transit times from Keplerian Velocity Curve 
;
;INPUT   cf  for observation for which transits are desired
;        p   Precise orbital period
;        tp  Precise Tp (5xxxx or 4xxxx) from rv_fit with /circle invoked
;        K   K from rv_fit

;ROUTINES
;      rv_drive.pro  Drives RV.PRO
;      rv.pro        Computes Keplerian Velocity Curve

ans = ''
print,' '

    print,'***************************************************************'
    print,'Input P, Tp, must have been determined for e = om = 0 (/circle)'
    print,'***************************************************************'
if not keyword_set(nomssg) then begin
    print,' '
    print,'WARNING:  Program only works for CIRCULAR orbits.'
    print,'          The input orbital parameters must have been computed'
    print,'          (in rv_fit with /circle) forcing e=0 and omega=0 .'
    print,'          The resulting Tp must have assumed e=0 and om=0 .'
    print,' '
    print,'WARNING:  This program does not use heliocentric JD (HJD)'
    print,'          as it should.  Associated error is ~9 min .'
    read,'Did you remember to enter p and tp as DOUBLE PRECISION? y or n ',ans
    if ans eq 'n' then return
end

if tp lt 40000 or tp gt 60000 then begin
  print,'Input Tp should have form 4xxxx or 5xxxx'
  return
end


;VELPLOT: Bin Velocities within intervals of 0.01 hour
inpcf = cf
;if keyword_set(corr) then begin
;    velplot,inpcf,title,0.01/24.,time,vel,errv,dum,dum,bincf,/noplot,/corr
;end else begin
    velplot,inpcf,title,0.01/24.,time,vel,errv,dum,dum,bincf,/noplot
;end

;Eliminate terrible points
i = where(bincf.errvel lt 3.*median(bincf.errvel),numobs)
bincf = bincf(i)
bincf.jd = bincf.jd + 40000.

;Fixed Orbital Parameters
;WARNING:  This program only works for CIRCULAR orbits.
;          The input orbital parameters must have been computed
;          (in rv_fit with /circle) forcing e=0 and omega=0 .
;          The resulting Tp must have assumed e=0 and om=0 .

; P= 4.2293d0            ;Period (in days)
;Be sure Tp is that for e=0 and om = 0
; and it should start 244xxxx leaving off the first "24".

p = double(p)
tp = double(tp)
 e = 0.00d0                ;eccentricity
 om = 0.d0                ;little omega (deg)
; K = 55.89                 ;Vel amplitude (m/s)
 dvdt = 0.0              ;linear slope (m/s per day) added to theoretical curve
; gamma set to 0. below
 gamma = 0.

par = [p,tp,e,om,k,gamma ] ;,dvdt]

;Prompted TIME DURATION
print,'Give starting date for ephemeris:'
read,'  Year (4 dig):',yr
read,'  Month (1 or 2 digits):',mon
read,'  Day (1 or 2 digits):',day
print,' '
read,'Give desired span of time in days:',ndays
startt = [yr,mon,day]
juldate,startt,startjd
endjd = startjd + ndays
mint = startjd
maxt = endjd
trange = maxt-mint
xr = [mint,maxt]  & lenx = max(xr) - min(xr)
leny = max(yr) - min(yr)

;PLOTTING
!p.charsize=1.7
!p.thick=3
!p.charthick=1.7
xt = '!6   Julian Date (-2450000)'
yt = '!6   Velocity  (m/s)'
ti = '!6 '
names = '!6 Marcy & Butler'

nsub = 10000.d0    ;make "nsub" time steps within range of time.
tfine = dindgen(nsub)*(2.d0 + trange)/nsub + mint - 1.d0

;Turn off linear trend
par(5)=0.d0    ;Set gamma to 0.
;par(6)=0.

thvel = rv_drive(tfine,par)
yra = [min(thvel)*1.3,max(thvel)*1.3] 
plot,tfine,thvel,xr=xr,/xsty,yra=yra,/ysty,xtit=xt,ytit=yt,titl=ti,thick=0.6

;Find time of Transits:  vel = 0.
 vnext = thvel(1:nsub-1)
 vprev = thvel(0:nsub-2)
 tnext = tfine(1:nsub-1)
 tprev = tfine(0:nsub-2)
 dt = tnext-tprev
 dv = vnext-vprev

 i = where(vprev gt 0. and vnext lt 0.,ncros)   ;zero crossings:red to blue
 tcros = tprev(i) - vprev(i)*dt(i)/dv(i) ;simple extrap. of tprev, starting at vprev

;Errors estimated
;Logic:  
;  eps_t0 Error in horizontal "shift" of sine wave is (P/4)/K * eps_vel/sqrt(Npts)
;  Each point gives an error of (P/4)/K * errvel on average.
;
; eps_per comes from an error in t0 at both ends of data.  In the first 1/4
; of data, the error in t0 is about 2*eps_t0 because only 1/4 data pts are
; used.  The same error accrues for the last quarter of the data.
; Added in quadtrature, the time difference has an error of: sqrt(2)*2*eps_t0
; But, some number of cycles went by:  Ncycle = Delta t / P which cuts the
; error in the derived period from that of the error in "time difference" above.
;
; eps_cros is just the error in t0 plus the accruing error
; due to the poorly known period.  Each cycle, the expected crossing time
; has an added error of eps_per

 eps_t0 = (0.25*p/K) * median(bincf.errvel) / sqrt(n_elements(bincf.mnvel))
 deltat = max(bincf.jd) - min(bincf.jd)
 eps_per = 2.*sqrt(2.)*eps_t0*p/(0.75*deltat)  ;new
 eps_cros = eps_per * ((tcros-max(bincf.jd))/p) + eps_t0 ;error in crossing time

 i=where(tcros lt max(bincf.jd),nlt)
 if nlt gt 0 then eps_cros(i) = eps_t0
 eps_cros = eps_cros *24.*60.  ;in minutes
 fudgefac = 1.3  ;fudge factor to incr. the uncertainties: conservative
 eps_cros = eps_cros * fudgefac
 oplot,tcros,tcros*0.,ps=7,symsize=1.6

oplot,bincf.jd,bincf.mnvel-gamma,ps=8,symsize=1.5

ncros = n_elements(tcros)

print,' '
print,'   Times of Transit by Companion'
print, ' '
print,'  DATE      UT   UNCERT       JD'
print,'                 (min)         '
print,'--------------------------------------------'
fo='(i4,A1,i2,A1,i2,A1,i2,a1,i2,a2, F5.1,A2,F15.5)'
for i=0,ncros-1 do begin
  daycnv,tcros(i) + 2400000.,yr,mn,d,hr
  fhr = fix(hr)
  min = (hr - fhr)*60.
  print,format=fo,yr,'/',mn,'/',d,' ',fhr,':',min,'  ', eps_cros(i),' ', tcros(i)
;print,'<p>'
end
print,'--------------------------------------------'

xyouts,max(xr)-.3*lenx, .015*leny+max(yr),names,size=1.5

stp = strtrim(string(par(0)),2)   ;period (string)
stp = strmid(stp,0,5)

stk   = strtrim(string(par(4)),2) ;K (amplitude) (string)
stk = strmid(stk,0,5)

ste   = strtrim(string(par(2)),2) ;eccentricty (string)
ste = strmid(ste,0,5)

;strms = strtrim(string(rms),2)
;strms = strmid(strms,0,3)
xyouts,xr(0)+.06*lenx,yr(0)+.91*leny,'!6 P = ' +stp +' day',size=1.6
xyouts,xr(0)+.06*lenx,yr(0)+.87*leny,'!6 K = ' + stk +' m/s',size=1.6
xyouts,xr(0)+.06*lenx,yr(0)+.83*leny,'!6 e = ' + ste ,size=1.6
;xyouts,xr(0)+.7*lenx,yr(0)+.91*leny,'!6 RMS = ' + strms +' m/s',size=1.5
;xyouts,7,-88,'!6RMS = ' + strms +' m/s',size=1.
;xyouts,7,-78,'Resid. x 2',size=1.2
;!p.color=100
;Plot Data
;print,'RMS to Fit =',rms
print,' '

fo='(A6,F17.6)'

print,'Actual Orbital Elements'
print,form=fo,'P =',par(0)
print,form=fo,'To =',par(1)
print,form=fo,'e =',par(2)
print,form=fo,'om =',par(3)
print,form=fo,'k =',par(4)
;print,form=fo,'gam =',par(5)
;print,form=fo,'dvdt =',par(6)

stp = strtrim(string(par(0)),2)   ;period (string)
stp = strmid(stp,0,8)

stt0 = strtrim(string(tcros(0)),2)   ;first transit
stt0 = strmid(stt0,0,9)

print, ' '
print, ' '

print,'Ephemeris of Transits:'
print,'t = 24'+ stt0 + ' n(' + stp + ')'

end
