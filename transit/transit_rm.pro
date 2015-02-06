;***********************************************************************
; NAME: TRANSIT_RM
;																	   
; PURPOSE: This procedure will remove observations taken during 
;	the transit, refit the data using rv_fit, then spit the points
;	back into place. 
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
; :KEYWORDS:	
;
;   BANK: in, optional, type="boolean'
;     This keyword restores vst structures from the vstbank directory
;     instead of vel or vel_all directories. 
;
; OUTPUTS:		
;
;   stddvtrans_no: The ratio of the standard deviation of observations
;       taken during transit - theoretical velocity to the same taken
;       not during transit.
;         
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
function transit_rm, p, tp, om, tim2calc, ting, tegr, cf3, vst, $
frz_ecc = frz_ecc, $
frz_k = frz_K, $
frz_per =  frz_per, $
mstar = mstar, $
orbel = orbel, $
plidx = plidx, $
star = star, $
time_stamp = time_stamp, $
trend = trend, $
debug = debug

print, '---------------------------------------------------------'
print, '        TIMES IN TRANSIT_RM:  '
print, '---------------------------------------------------------'
print, jul2cal(ting + 2.44d6)
print, jul2cal(tegr + 2.44d6)
print, '---------------------------------------------------------'
;stop

print, 'orbel is: '
print, orbel

;CONSTANTS:
MEARTH = 5.9742d24
MSUN = 1.98892d30
MJUP = 1.8987d27

t_initial_phase = cf3[0].jd
usersym, cos(findgen(32*!pi*2/32.)), sin(findgen(32*!pi*2/32.)), /fill                

;The REAL offset that took me FOREVER to find!!! ArghhhH!!!!
;VELOCITIES and ERRORS
obvel = cf3.mnvel         ;observed velocities for each observation
;newmaxvel = 0.5*(max(obvel)-min(obvel))
;dvel = newmaxvel - max(obvel)
;obvel = obvel +dvel
;cf3.mnvel = obvel


print, 'ting is: ', ting
print, 'tegr is: ', tegr

extitle = star
if ~keyword_set(vst) then begin
 vst = 'vst'+strmid(star, 2, strlen(star)-1)+'.dat'
endif ;~vst

if keyword_set(frz_ecc) then print, 'frz_ecc is: ', frz_ecc
if keyword_set(frz_per) then print, 'frz_per is: ', frz_per
if keyword_set(frz_k) then print, 'frz_k is: ', frz_k


if ting gt 2d6 then ting -= 2.44d6
if tegr gt 2d6 then tegr -= 2.44d6

if ting gt 4d4 then ting -= 4d4
if tegr gt 4d4 then tegr -= 4d4

jds = cf3.jd
vels = cf3.mnvel

print, 'you have entered transit_rm'
;stop

;***********************************************************************
;FIRST TO COMPARE THE OBSERVATIONS TO THE THEORETICAL TIMES OF TRANSIT
;AND REMOVE POINTS TAKEN DURING TRANSIT
;***********************************************************************
print, 'the ting is:', ting
print, 'the teger is:', tegr
print, 'the first jds is:', jds[0]

print, '---------------------------------------------------------'
print, '        TIMES IN TRANSIT_RM:  '
print, '---------------------------------------------------------'
print, jul2cal(ting + 2.44d6)
print, jul2cal(tegr + 2.44d6)
print, '---------------------------------------------------------'
;stop

;---------------------------------------------------------
;---------------------------------------------------------
;        TIMES BEFORE TRANSIT IN TRANSIT_MONTE:  
;---------------------------------------------------------
;2009/04/30 12:34:37.699
;2009/04/30 13:18:47.807
;2009/04/30 15:39:14.327
;---------------------------------------------------------

;Here I will create a FOR loop that will cycle through all of the JDs
;checking to see if any of them are within the transit window. 

print, 'the first jds: ', jul2cal(jds[0] + 2.44d6)

;the array that will house which observations were taken during transit
transiters = dblarr(n_elements(cf3))



if keyword_set(debug) then stop
for i=0, n_elements(jds)-1 do begin
 dif1 = floor((jds[i]  - ting)/p)
 print, 'ting is: ', ting, ' or: ', jul2cal(ting + 2.44d6)
 print, 'tegr is: ', tegr, ' or: ', jul2cal(tegr + 2.44d6)
 if (tegr - ting)/p gt 0.4 then begin
   print, '!!!!!!!ERROR!!!!!!!!!!!!!!'
;   stop
   ting2 = ting + p
   print, 'ting2: ', ting2
   ting = tegr
   tegr = ting2
   print, 'new ting is: ', ting, ' or: ', jul2cal(ting + 2.44d6)
   print, 'new tegr is: ', tegr, ' or: ', jul2cal(tegr + 2.44d6)
;   stop
 endif
 
 
 print, 'jds[i]+2.44d6 is: ', jds[i]+2.44d6, ' or: ', jul2cal(jds[i]+ 2.44d6)
 print, 'dif1 is: ', dif1
 dif2 = ceil((jds[i] - ting)/p)
 print, 'dif2 is: ', dif2
 
 ;The below prints how the observation times compare to the times of
 ;ingress and egress. Note that the next ingress and egress could be 
 ;in the future, so a time in the future is acceptable. The sign
 ;difference below is OK as well:
 print, 'dif1*p + jds', jds[i]+2.44d6 - dif1*p, ' or: ', jul2cal(jds[i]+2.44d6 - dif1*p)
 print, 'dif2*p + jds', jds[i]+2.44d6 - dif2*p, ' or: ', jul2cal(jds[i]+2.44d6 - dif2*p)
 
 gv = .0625*p < 1.125*(abs(tegr - ting)) ;the "give" to each side of transit to account for uncertainty
 
 if ( ((jds[i] - dif1*p ge (ting-gv)) AND (jds[i] - dif1*p le (tegr+gv))) OR $
   ( (jds[i] - dif2*p ge (ting-gv)) AND (jds[i] - dif2*p le (tegr+gv))) ) $
   then begin
   print, 'WE HAVE A TRANSITER!'
;   STOP
   transiters[i] = 1
 endif ;in the sweet spot
;stop
endfor ;i=0 -> #(jds)-1


print, 'the indices of the observations taken during transit: ', $
        where(transiters eq 1)

if total(transiters) then begin
newcf3 = rm_elements(cf3, where(transiters eq 1))
endif else newcf3 = cf3

print, '#transiters: ', total(transiters)
print, '#jds: ', n_elements(jds)
print, '#news: ', n_elements(newcf3.jd)

print, 'the minmax of mnvel is: ', minmax(cf3.mnvel)
;stop
;***********************************************************************
; NOW TO REFIT THE DATA USING RV_FIT:
;***********************************************************************

print, 'the minmax of the velocities are: ', minmax(newcf3.mnvel)
;window, /free

nplanets = n_elements(orbel)/7

jds = newcf3.jd
;sorter = sort(jds)
;vels = newcf3[sorter].mnvel
;errs = newcf3[sorter].errvel
vels = newcf3.mnvel
errs = newcf3.errvel
;stop


if n_elements(newcf3) gt 5 then begin
orbpar = rv_fit_mp(newcf3.jd, newcf3.mnvel, newcf3.errvel, $
         nplanets=nplanets, orbel=orbel, $
         chi=chi, rms=rms, circ=circle, /plotfit, $
         _EXTRA=plotextra, fixed=fixed, $
         offset=offset)
endif else orbpar=orbel
;stop
thvelnotrans = rv_drive_mp(newcf3.jd, orbel)
;thvel = mp_rv_thvels(jds, orbel)
thvel = rv_drive_mp(jds, orbpar)
;plot, jds, thvel, ps=8, title='Black Dots are theoretical'
;loadct, 39, /silent
;oplot, jds, vels, ps=8, color=60
;stop

p = orbel[7*plidx]
print, 'p is: ', p


my_lin_pars = orbel[7*plidx: 7*plidx+6]
mypars = my_lin_pars
;stop

;add the offset to the data so that it is centered when plotted:
;newcf3.mnvel += gam
;notsatisfied=1
;while notsatisfied do begin
;***********************************************************************
;NOW TO ADD THE DATA POINTS BACK AND PLOT TO LOOK FOR THE RM EFFECT:
;***********************************************************************

;To plot a phase-folded set of observations that were NOT taken during
;transit

;***********************************************************************
;First to express each observation in terms of it's phase rather than 
;julian date:
;stop

phase1 = transit_phase(cf3 = newcf3, par = my_lin_pars, $
tinit = t_initial_phase);, /make_plots)
;stop

print, 'Tp is: ', jul2cal(2.44d6 + mypars[1])
tfine = p*(dindgen(301)-50.)/200. + newcf3[0].jd
;thvelold = rv_drive(tfine,mypars)    ;compute theoretical curve
;offset_vel=(max(thvel) -min(thvel))/2.

tfine = (tfine- newcf3[0].jd)/p
tfine = tfine+0.15
xph = where( (tfine gt 0.) and (tfine lt 1.)) 

Window, /free
;Special for 179079
!p.font = 1


  ;This section will calculate the range for the data:
  range = max(thvel) - min(thvel)
  yrange = [min(thvel) - 0.5*range, max(thvel) + 0.5*range] > $
           [min(vels) - 0.5*range, max(vels) + 0.5*range]

window, 7

;yrange=[-250,250]
;filen='~/Documents/SFSU/research/Thesis/figures/86264plot'+time_stamp
;ps_open, filen, /encaps,/color
usersymbol, 'circle', /fill
plot, phase1, newcf3.mnvel, psym=8, xra=[-0.2, 1.2], /xstyle, $
xtitle='!6 Orbital Phase', $
ytitle='!6 Radial Velocity [m s!u-1!n]', $
/ystyle, yra = yrange
ploterrbrs, phase1, newcf3.mnvel, newcf3.errvel

;***********************************************************************
;make the shaded region that shows the transit window:
transitbeg = transit_phase(par = my_lin_pars, $
times = ting, tinit = t_initial_phase)
transitend = transit_phase(par = my_lin_pars, $
times = tegr, tinit = t_initial_phase)
polyx = [transitbeg, transitend, transitend, transitbeg]
polyy = [yrange[0], yrange[0], $
yrange[1] - 5d-3 * yrange[1], yrange[1] - 5d-3 * yrange[1]]
loadct, 0, /silent
polyfill, polyx, polyy, color = 246
oplot, phase1, newcf3.mnvel, psym=8

;***********************************************************************
;now to overplot the points that WERE taken during transit:
;stop

if total(transiters) then begin
  
  print, 'number of transiters: ', total(transiters)
  
  blockers = cf3[where(transiters eq 1)]
  

  loadct, 3, /silent
  phase = transit_phase(cf3 = blockers, par = my_lin_pars, tinit = t_initial_phase);, /make_plots)
  print, n_elements(phase)
  print, 'now back in transit_rm.pro'
  
  
  blockingvels = blockers.mnvel
  blockingerrs = blockers.errvel
  if n_elements(phase) le 1 then begin
	  ;This part takes care of the case where there are less than two 
	  ;observations taken during transit. I don't omit the overplot
	  ;altogether incase there is one point taken during transit
	  ;(as is the case for HD17156)
	  phase = [phase, phase, phase]
	  blockingvels = [blockingvels, blockingvels, blockingvels]
	  blockingerrs = [blockingerrs, blockingerrs, blockingerrs]
  endif
  
  
  print, 'the phase at this point is: ', phase
  
  loadct, 3, /silent
  !p.charthick = 2.
  usersymbol, 'circle', SIZE_OF=2, thick=2
  ploterrbrs, phase, blockingvels, blockingerrs, color=20
  oplot, phase, blockingvels, color = 144., ps = 8, thick=4
  ;oplot, phase, blockingvels, color = 20., ps = 4, symsize = 2.5
  usersymbol, 'circle', /filled ;SIZE_OF=2, thick=4,
endif ;oplot transiters

;create the time array:
num = 500
tfine = p[0]*(dindgen(num+1)/num*1.4 - 0.2) + t_initial_phase
;thvel = mp_rv_thvels(tfine, orbel)
print, 'orbel before producing the theoretical curve: '
print, orbel

fixed = dblarr(n_elements(orbel))
fixed[0]=1 & fixed[2]=1 & fixed[6] = ~orbel[6]
window, /free
neworbpar=rv_fit_mp(cf3.jd, cf3.mnvel, cf3.errvel, nplanets=nplanets, $
         chi=chi, rms=rms, $;/trend, $;/plotfit,$
         fixed=fixed, orbel=orbel, offset=offset)
wset, 7
thvel = rv_drive_mp(tfine, orbel)
thvel42 = rv_drive_mp(tfine, orbpar)
;stop
phase_tfine = (tfine - t_initial_phase)/p[0]
;overplot the theoretical velocity curve:
;oplot,phase_tfine,thvel,thick=4,col=0
;stop
loadct, 0, /silent
thvel2 = rv_drive_mp(tfine, neworbpar)
oplot,phase_tfine,thvel2,thick=2 ;,col=100, linestyle=2
;stop
;     tconst = 11544.5
;     m = makearr(10000, mm(jds)*[0.9, 1.1])
;     tfinelin = 2000. + (m - tconst)/365.25

;     plot, xarr, v, ps = 3, xrange=xra_yrs,$ 
;           _extra = plotextra
;     oplot, tfinelin, rv_drive_mp(m, orbel),linesty=2,thick=4

;stop
;***********************************************************************
;  THIS PART WILL COMPUTE THE RATIO OF THE STANDARD DEVIATION OF THE 
;  OBSERVATIONS TAKEN DURING TRANSIT TO THE STDDEV OF THE OBSERVATIONS
;  NOT TAKEN DURING TRANSIT:
;***********************************************************************
rvfits = orbel[7*plidx: 7*plidx+6]
print, 'rvfits is: ', rvfits

if total(transiters) then begin
  ;The theoretical velocities during transit:
;  thveltransit = mp_rv_thvels(cf3[where(transiters eq 1)].jd, orbel)
  thveltransit = rv_drive_mp(cf3[where(transiters eq 1)].jd, neworbpar)
  
  ;The standard deviation of the blocking vels to the theoretical curve:
  stddvtransit = sqrt(total((blockingvels - thveltransit)^2)/total(transiters))
  
  ;The theoretical velocities at the times of observation of the rest
  ; of the velocities:
  ;thvelnotrans = mpf_rv_thvel(newcf3.jd, mynewpars)
  ;IF THE # OF OBS IS TOO LOW SDR WILL BLOW UP:
  ;IN THAT CASE, KEEP VALUES WITHIN TRANSIT WINDOW:
  if n_elements(newcf3) lt 15 then begin
    thvelnotrans=rv_drive_mp(newcf3.jd, neworbpar)
  endif;quelling explosion

  
  stddvnotrans = sqrt(total((newcf3.mnvel - thvelnotrans)^2)/n_elements(newcf3.mnvel))
  
  stddvtrans_no = stddvtransit/stddvnotrans
print, 'STDTRANS/NOTRANS: ', stddvtrans_no
  if stddvtrans_no gt 5 then stop
endif else stddvtrans_no = 0.d

print, 'HERE IS THE STANDARD DEVIATION RATIO: '
print, 'STDTRANS/NOTRANS: ', stddvtrans_no


	 
stp = strtrim(string(mypars[0], format = '(F10.2)'), 2)
stk = strtrim(string(mypars[4], format = '(F10.2)'), 2)
ste = strtrim(string(mypars[2], format = '(F10.2)'), 2)
;strms = strtrim(string(rvfits.rms, format = '(F10.2)'), 2)
;stchi = strtrim(string(rvfits.chi, format = '(F10.2)'), 2)
xyouts,.8, .82,'!6 P = ' + stp +' d', color = 0, /norm
xyouts,.8, .76,'!6 K = ' + stk +' ms!u-1!n', color = 0, /norm
xyouts,.8, .7,'!6 e = ' + ste , color = 0, /norm
;xyouts,.45, .21,'!6 rms = ' + strms , color = 0, /norm
;xyouts,.25, .21,'!6 chi = ' + stchi , color = 0, /norm

xyouts, .15, .85, star, /norm
plots, [-0.2, 1.2], [0.,0.], color = 0
;ps_close
;stop
 pngdir = '~/tempngs/'
;stop
;endwhile


 pngdir = '~/Sites/tauceti/secure/transit/ephemerides/pngs/'
stop   
png_file = pngdir + star + '_' + time_stamp + '.png'
tvlct, redvector,greenvector,bluevector,/get
write_png,png_file,tvrd(),redvector,greenvector,bluevector

epsdir='~/transit/PDFS/'+star+'/'
ps_open, epsdir+star+'rvfit'+time_stamp, /encaps

usersymbol, 'circle', /fill, /color
plot, phase1, newcf3.mnvel, psym=8, xra=[-0.2, 1.2], /xstyle, $
xtitle='!6 Orbital Phase', $
ytitle='!6 Radial Velocity [m s!u-1!n]', $
/ystyle, yra = yrange
ploterrbrs, phase1, newcf3.mnvel, newcf3.errvel

loadct, 0, /silent
polyfill, polyx, polyy, color = 246
oplot, phase1, newcf3.mnvel, psym=8

if total(transiters) then begin
  loadct, 3, /silent
  !p.charthick = 2.
  usersymbol, 'circle', SIZE_OF=2, thick=2
  ploterrbrs, phase, blockingvels, blockingerrs, color=20
  oplot, phase, blockingvels, color = 144., ps = 8, thick=4
  usersymbol, 'circle', /filled ;SIZE_OF=2, thick=4,
endif ;oplot transiters

oplot,phase_tfine,thvel2,thick=2 ;,col=100, linestyle=2

xyouts,.8, .82,'!6 P = ' + stp +' d', color = 0, /norm
xyouts,.8, .76,'!6 K = ' + stk +' ms!u-1!n', color = 0, /norm
xyouts,.8, .7,'!6 e = ' + ste , color = 0, /norm
xyouts, .15, .85, star, /norm
plots, [-0.2, 1.2], [0.,0.], color = 0
ps_close

return, [rvfits, stddvtrans_no]
end; transit_rm.pro







;yomama=0
;if yomama then begin
;
;;80606 plot
;ps_open, '80606plot'+time_stamp, /color, /encaps
;
;usersymbol, 'circle', /fill
;plot, phase1, newcf3.mnvel, psym=8, xra=[-0.2, 1.2], /xstyle, $
;xtitle='!6 Phased Observations', $
;ytitle='!6 Radial Velocity [m s!u-1!n]', $
;/ystyle, yra = [-500,600]
;ploterrbrs, phase1, newcf3.mnvel, newcf3.errvel
;;oplot, [0,1], [-500,-500]
;;oplot, [0,1], [700,700]
;
;yrange=[-500,600]
;polyy = [yrange[0], yrange[0], $
;yrange[1] - 5d-3*yrange[1], yrange[1] - 5d-3*yrange[1]]
;loadct, 0, /silent
;polyfill, polyx, polyy, color = 246
;oplot, phase1, newcf3.mnvel, psym=8
;loadct, 0, /silent
;thvel2 = rv_drive_mp(tfine, neworbpar)
;oplot,phase_tfine,thvel2,thick=2 ;,col=100, linestyle=2
;  loadct, 3, /silent
;  !p.charthick = 2.
;  usersymbol, 'circle', SIZE_OF=2, thick=2
;  ploterrbrs, phase, blockingvels, blockingerrs, color=20
;  oplot, phase, blockingvels, color = 144., ps = 8, thick=4
;  ;oplot, phase, blockingvels, color = 20., ps = 4, symsize = 2.5
;  usersymbol, 'circle', /filled ;SIZE_OF=2, thick=4,
;xyouts,.8, .82,'!6 P = ' + stp +' d', color = 0, /norm
;xyouts,.8, .76,'!6 K = ' + stk +' ms!u-1!n', color = 0, /norm
;xyouts,.8, .7,'!6 e = ' + ste , color = 0, /norm
;;xyouts,.45, .21,'!6 rms = ' + strms , color = 0, /norm
;;xyouts,.25, .21,'!6 chi = ' + stchi , color = 0, /norm
;
;xyouts, .25, .85, star, /norm
;plots, [-0.2, 1.2], [0.,0.], color = 0
;;end 80606 plot
;
;;begin 80606 plot zoom:
;
;ps_open, '80606zoomplot'+time_stamp, /color, /encaps
;
;usersymbol, 'circle', /fill
;xrange=[0.7,0.8]
;yrange=[-50,125]
;plot, phase1, newcf3.mnvel, psym=8, xra=xrange, /xstyle, $
;xtitle='!6 Phased Observations', $
;ytitle='!6 Radial Velocity [m s!u-1!n]', $
;/ystyle, yra = yrange
;ploterrbrs, phase1, newcf3.mnvel, newcf3.errvel
;;oplot, [0,1], [-500,-500]
;;oplot, [0,1], [700,700]
;
;polyy = [yrange[0], yrange[0], $
;yrange[1] - 5d-3*yrange[1], yrange[1] - 5d-3*yrange[1]]
;loadct, 0, /silent
;polyfill, polyx, polyy, color = 246
;oplot, phase1, newcf3.mnvel, psym=8
;loadct, 0, /silent
;thvel2 = rv_drive_mp(tfine, neworbpar)
;oplot,phase_tfine,thvel2,thick=2 ;,col=100, linestyle=2
;  loadct, 3, /silent
;  !p.charthick = 2.
;  usersymbol, 'circle', SIZE_OF=2, thick=2
;  ploterrbrs, phase, blockingvels, blockingerrs, color=20
;  oplot, phase, blockingvels, color = 144., ps = 8, thick=4
;  ;oplot, phase, blockingvels, color = 20., ps = 4, symsize = 2.5
;  usersymbol, 'circle', /filled ;SIZE_OF=2, thick=4,
;xyouts,.8, .82,'!6 P = ' + stp +' d', color = 0, /norm
;xyouts,.8, .76,'!6 K = ' + stk +' ms!u-1!n', color = 0, /norm
;xyouts,.8, .7,'!6 e = ' + ste , color = 0, /norm
;;xyouts,.45, .21,'!6 rms = ' + strms , color = 0, /norm
;;xyouts,.25, .21,'!6 chi = ' + stchi , color = 0, /norm
;
;xyouts, .25, .85, star, /norm
;plots, [0.7, .8], [0.,0.], color = 0
;
;ps_close
;
;;end 80606 plot zoom
;endif
;
;star80606=0
;if star80606 then begin
;oplot, [0.7,0.8], [-50,-50]
;oplot, [0.7,0.8], [125,125]
;oplot, [0.8,0.8], [-50,125]
;oplot, [0.7,0.7], [-50,125]
;endif
;
;