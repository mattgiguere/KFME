pro transit_monte, $
circle = circle, $
database = database, $
debug = debug, $
frz_ecc = frz_ecc, $
frz_k = frz_k, $
frz_om = frz_om, $
frz_per = frz_per, $
init_e = init_e, $
incs = incs, $
lick = lick, $
nobank = nobank, $
normanimation=normanimation, $
nplanets=nplanets, $
planetid=planetid, $
star = star, $
sdatpars = sdatpars, $
specialvst = specialvst, $
widget=widget, $
pararr=pararr, $
pstate=pstate, $
trend=trend

;stop

database=0
;spawn, 'networksetup -getcomputername', cname
;if cname eq 'ring' then ring = 1 else ring = 0

;This is reset by STARDAT.PRO if the #>1:
;nplanets=1

;incs is the number of times to repeat the transit calculating process 
if n_elements(incs) lt 1 then incs = 10.d
if n_elements(star) lt 1 then star = 'HIP85977'

;Create a time stamp of when the file was created so that I don't
;overwrite it at a later point:
time_stamp = jul2cal(systime(/julian), /dash)


;This section will create arrays of orbital parameters to pass into
;transit using Debra's monte_new (now modified and called 
; MONTE_LIN) procedure:

;First create the initial guess for the orbital parameters using
;STARDAT.PRO:
 ;	ORBPAR: The orbital parameters [p, Tp, e, om, k, gam]
;stop
print, 'star is: ', star
print, 'nplanets before stardat is: ', nplanets
norbs = stardat(star, nplanets = nplanets)
print, 'nplanets after stardat is: ', nplanets

ipnums = nplanets
;nplanets = n_elements(ipnums)
knownstarrad = norbs.knownstarrad
vsini=norbs.vsini
uvsini=norbs.uvsini
mstar=norbs.mstar
unc_mstar=norbs.unc_mstar

if strmid(star, 0, 2) eq 'HD' then begin
	starnum = strmid(star, 2, strlen(star)-1)
endif else starnum = 'hip'+strmid(star, 3, strlen(star)-1)

if ((strmid(star, 0, 1) eq 'T') OR (strmid(star, 0, 1) eq 'G')) then begin
    starnum = star
endif ;TrES and GL stars

;Now restore the cf structure:

vstdir = '/mir3/vel/'
xst = file_info(vstdir+'vst'+starnum+'.dat')
if ~(xst.exists) then vstdir = '/mir1/vel/'

if keyword_set(lick) then begin
     vstdir = '/mir1/vel/'
	 xst = file_info(vstdir+'vst'+starnum+'.dat')
	 if ~(xst.exists) then vstdir = '/mir3/vel/'
endif;lick


specialvsts = ['11964', 'HIP113020', '217107b', 'GL317']


dum = where(((starnum eq specialvsts) OR (star eq specialvsts)), svct)

if ~svct then restore, vstdir+'vst'+starnum+'.dat'
;restore, '/mir3/vel/vst'+starnum+'.dat'
if starnum eq '11964' then restore, '/mir3/vel/vst11964a.dat'
if starnum eq 'HIP113020' then restore, '/mir3/vel/vstgl876.dat'
if starnum eq '217107b' then restore, '/mir3/vel/vst217107.dat'
if star eq 'GL317' then restore, '/mir3/vel/vstgl317.dat'

;This takes care of the case if we're restoring a special vst that
;doesn't have a cf3 structure:
if n_elements(cf3) eq 0 then begin
 if n_elements(cf) gt 0 then cf3=cf
 if n_elements(cfresid) gt 0 then cf3=cfresid
 if n_elements(cfb) gt 0 then cf3=cfb
 if n_elements(cfc) gt 0 then cf3=cfc
endif

if n_elements(cf3) lt 5 then return



title=star

;print, 'The starttime is: ', systime()
;startold = systime(/julian)
;print, 'The endtime of the old way is: ', systime()
;print, 'this took: ', (systime(/julian) - startold)*24d*3600d, ' seconds.'

fixed = intarr(7*nplanets) ;+ 1
fixed[2] = keyword_set(frz_ecc)
fixed[3] = keyword_set(frz_om)
fixed[6] = ~norbs.trend
idxer = indgen(nplanets)
orbel = dblarr(7*nplanets)
orbel[7*idxer] = norbs.p
orbel[7*idxer+1] = norbs.tp - 2.44d6
orbel[7*idxer+2] = norbs.e
orbel[7*idxer+3] = norbs.om
orbel[7*idxer+4] = norbs.k
orbel[5] = norbs.gam[0]
orbel[6] = norbs.dvdt[0]
if fixed[6] then orbel[6]=0d

if keyword_set(frz_per) then orbel[0] = frz_per 
if keyword_set(frz_tp) then orbel[1] = frz_tp 
if keyword_set(frz_ecc) then orbel[2] = frz_ecc
if keyword_set(frz_om) then orbel[3] = frz_om
if keyword_set(frz_k) then orbel[4] = frz_k

print, 'fixed is: ', fixed
;if orbel[0] lt 10 then begin
;   fixed[2]=1
;   orbel[2]=1d-4
;endif

fitobs = cf3.jd
fitdat = cf3.mnvel
err = cf3.errvel

;***************************************************************
;***************************************************************
;NOW OVERWRITE EVERYTHING IF THIS IS BEING RUN FROM THE WIDGET
;***************************************************************
;***************************************************************
;NOTE: Right now it is set to perform the MC analysis on one
;planet at a time. Use the "REMOVE OTHERS" BUTTON and TEXT
;FIELD to run the MC analysis on the planet of your choosing. 
if keyword_set(widget) then begin
	nplanets=1
	n_planets=nplanets

	;JUST RV
	cf3=(*(*pstate).pcf).cf_rv
	fitobs = (*(*pstate).pcf).cf_rv.jd
	fitdat = (*(*pstate).pcf).cf_rv.mnvel
	err = (*(*pstate).pcf).cf_rv.errvel
	
orbpar=pararr.value

mstar = (*(*pstate).pfunctargs).m_star
m_star = mstar
knownstarrad = (*(*pstate).pfunctargs).rstar

;n_planets = (*(*pstate).pfunctargs).n_planets
;indx=findgen(n_planets)
indx = (*pstate).controlbar.aloneplanet-1

period=orbpar[indx*5]
a_pl=((period/365.2564d)^2*m_star)^(1./3.)
m_pl_earth = orbpar[indx*5+1]
inc = dblarr(n_planets)+89.9
ecc = orbpar[indx*5+2]

k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)

orbel=dblarr(n_planets*7)
orbel[0] = orbpar[indx*5] ;p
orbel[0+1] = orbpar[5*indx+4] ;tp
orbel[0+2] = orbpar[5*indx+2] ;e
orbel[0+3] = orbpar[5*indx+3] ;om
orbel[0+4] = k ;k
;orbel[7*indx] = orbpar[indx*5] ;p
;orbel[7*indx+1] = orbpar[5*indx+4] ;tp
;orbel[7*indx+2] = orbpar[5*indx+2] ;e
;orbel[7*indx+3] = orbpar[5*indx+3] ;om
;orbel[7*indx+4] = k ;k
orbel[5] = orbpar[5*(*(*pstate).pfunctargs).n_planets];gam
orbel[6] = orbpar[5*(*(*pstate).pfunctargs).n_planets+1];dvdt

fixed=dblarr(n_planets*7)
fixed[0] = pararr[5*indx].fixed
fixed[0+1] = pararr[5*indx+4].fixed
fixed[0+2] = pararr[5*indx+2].fixed
fixed[0+3] = pararr[5*indx+3].fixed
fixed[0+4] =  pararr[5*indx+1].fixed
fixed[5] = pararr[5*(*(*pstate).pfunctargs).n_planets].fixed;gam
fixed[6] = pararr[5*(*(*pstate).pfunctargs).n_planets+1].fixed;dvdt
	
;now to subtract the signal from the other planets:
	alpl = (*pstate).controlbar.aloneplanet
	
	pararrrest = pararr
	pararrrest[(5*(alpl-1d)):(5*(alpl-1d)+4)].value = 0
	
	tfitrest = rv_mpfit_rv(fitobs, pararrrest.value, $
		_EXTRA = (*(*pstate).pfunctargs))


cf3.mnvel = fitdat - tfitrest
plot, cf3.jd, cf3.mnvel, ps=8
stop
endif ;widget

bincf = velplot_func(cf3,/noplot, /nozero)
cf3 = bincf
inpcf = cf3

fitobs = cf3.jd
fitdat = cf3.mnvel

;NOTE THAT YOU ARE USING BINNED VELOCITIES!
 orbel=rv_fit_mp(fitobs,fitdat, err, fixed=fixed, $
		yfit=syn_fit,tps=max(fitobs),perror=perror, $
						chi=chi,rms=rms, orbel=orbel, /plotfit, /trend)

stop

;SINCE WRIGHT AND HOWARD'S PARTIALLY LINEARIZED PROCEDURE 
;DOESN'T SEEM TO BE WORKING RIGHT FOR HIGHLY ECCENTRIC
;SYSTEMS, I WILL GO OLD SCHOOL FOR THOSE:
if 0 then begin
	;if orbpar[2] gt 0.6 then begin
	  print, 'NOW USING THE OLD METHOD!'
	;  STOP
	  newrvfit = rv_fit_func(cf3, per=norbs.p, /phase, frz_ecc = frz_ecc, $
					 circle = circle, debug = debug, star = star)
					 stop
	  orbpar = newrvfit.par
	  outarr = monte_new(inpcf, orbpar, ntrial = incs, star = starnum, $
	  				debug = debug, frz_ecc = frz_ecc, frz_per = frz_per, $
	  				frz_k = frz_k, circle = circle)
	  nplanets=1
	  rmsarr = outarr[7,*]  
	  chiarr = outarr[8,*]
	
	tdir = '~/transit/PDFS/'
	fulldir = tdir+star
	xst = file_info(fulldir)
	if ~(xst.exists) then begin
	spawn, 'mkdir '+ fulldir
	spawn, 'cp '+tdir+'deluxetable.sty '+fulldir
	endif ;directory did not exist
	epsdir='~/transit/PDFS/'+star+'/'
	plot, outarr[2,*], outarr[4,*], ps=8, xtitle='eccentricity', $
	ytitle='K (m/s)', title=star, xra=[0d,1d], /xsty
	ps_open, epsdir+star+'ek'+time_stamp, /encaps
	plot, outarr[2,*], outarr[4,*], ps=8, xtitle='eccentricity', $
	ytitle='K (m/s)', title=star, xra=[0,1], /xsty
	ps_close
	stop
	
	window, /free
	plot, outarr[2,*], outarr[4,*], ps=8, xtitle='eccentricity', $
	ytitle='K (m/s)', title=starnum
	plothist, outarr[2,*], bin=0.01, title=starnum, xtitle='e', $
	ytitle='# of realizations', xra=[-0.025,1.025]
	
	  ;now erase these so it doesn't think they're slope and offset later, 
	  ;as they would be for the partially linearized code:
	  outarr[6,*]*=0d ;this is NOT dvdt, but msini from rv_fit_old ;20091128
	  outarr[7,*]*=0d
	  outarr[8,*]*=0d
endif else begin ;high ecc
  startmonnew = systime(/julian)
  ;NOTE THAT YOU ARE USING BINNED VELOCITIES!

  linstruct = monte_lin(cf3, orbel, ntrial = incs, star = starnum, $
				  debug = debug, frz_ecc = frz_ecc, frz_per = frz_per, $
				  frz_k = frz_k, circle = circle, nplanets=nplanets, $
				  fixed=fixed, trend=trend, frz_om=frz_om)
  endmonnew = systime(/julian)
  
  outarr = linstruct.outarr
  chiarr = linstruct.chiarr
  rmsarr = linstruct.rmsarr
  old_outarr = outarr
  print, 'NOW AT THE END OF MONTE_LIN'
  for idx=0, nplanets-1 do begin
		print, 'p is: ', median(outarr[0+7*idx,*], /double), $
		' +/- ', stddev(outarr[0+7*idx,*], /double)
		print, 'tp is: ', median(outarr[1+7*idx,*], /double), $
		' +/- ', stddev(outarr[1+7*idx,*], /double)
		print, 'om is: ', median(outarr[3+7*idx,*], /double), $
		' +/- ', stddev(outarr[3+7*idx,*], /double)
		print, 'k is: ', median(outarr[4+7*idx,*], /double), $
		' +/- ', stddev(outarr[4+7*idx,*], /double)
		print, 'e is: ', median(outarr[2+7*idx,*], /double), $
		' +/- ', stddev(outarr[2+7*idx,*], /double)
		print, 'gam is: ', median(outarr[5+7*idx,*], /double), $
		' +/- ', stddev(outarr[5+7*idx,*], /double)
		print, 'dvdt is: ', median(outarr[6+7*idx,*], /double), $
		' +/- ', stddev(outarr[6+7*idx,*], /double)
		print, ''
		print, ''
  endfor
  stop
endelse;low ecc


if nplanets gt 1 then plidx = where(planetid eq ipnums) else plidx=0
if plidx lt 0 then plidx=0
monte_p = transpose(outarr[7*plidx, *])
monte_Tp = transpose(outarr[7*plidx+1, *])
monte_e = transpose(outarr[7*plidx+2, *])
monte_om = transpose(outarr[7*plidx+3, *])
monte_k = transpose(outarr[7*plidx+4, *])
monte_gam = transpose(outarr[7*plidx+5, *])
monte_dvdt = transpose(outarr[7*plidx+6, *])
comp = norbs[plidx].comp

plothist, monte_p, bin=0.01, title='Orbital Periods (monte_p)'
stop
print, 'old incs: ', incs
incs = n_elements(monte_p)
print, 'new incs: ', incs

plothist, monte_tp, bin = 0.1, title='Tp before correcting'
print, 'the old stddev: ', stddev(monte_tp, /double)
stop
offbyper = where( monte_tp lt (( median(monte_tp, /double)  - $
	median(monte_p, /double) )/2.d ), ct)

if ct gt 0 then begin
	amountps = round( (median(monte_tp, /double) - (monte_tp[offbyper]) )/ $
		median(monte_p, /double))
	monte_tp[offbyper] += median(monte_p, /double)*amountps
endif

offbyper = where(monte_tp gt ( ( median(monte_tp, /double)  + $
	median(monte_p, /double) )/2.d ), ct)

if ct gt 0 then begin
	amountps = round( (monte_tp[offbyper] - median(monte_tp, /double) )/ $
		median(monte_p, /double))
	monte_tp[offbyper] -= median(monte_p, /double)*amountps
endif

window, 2
plothist, monte_tp - min(monte_tp), bin = 0.1, $
title='monte_tp after correction', $
xtitle='monte_tp - min(monte_tp)'
print, 'the new stddev: ', stddev(monte_tp, /double)

monte_TCen_arr = dblarr(incs)
monte_TIng_arr = monte_TCen_arr
monte_TEgr_arr = monte_TCen_arr
monte_STCen_arr = dblarr(incs)
monte_STIng_arr = monte_TCen_arr
monte_STEgr_arr = monte_TCen_arr
msini_arr = monte_TCen_arr
prob_t_arr =  dblarr(incs)
prob_st_arr =  dblarr(incs)

;if e eq 0 then begin 
;  monte_om *= 0.d
;  monte_e *= 0.d
;endif
stop

print, 'mstar is: ', norbs.mstar

monte_msini = dblarr(incs)
monte_a1sini = dblarr(incs)
monte_arel = dblarr(incs)
for i=0, incs-1 do begin

print, '*******************************'
print, 'STAR IS: ', star
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'NOW ON TRANSIT MONTE RUN: ', i
print, 'Monte_Tp is: ', monte_Tp[i]
print, 'Monte_P is: ', monte_p[i]
print, '*******************************'

;NOTE THAT YOU ARE USING BINNED VELOCITIES!
transit, star = star, $
knownstarrad=knownstarrad, $
monte_p = monte_p[i], $
monte_Tp = monte_Tp[i], $
monte_om = monte_om[i], $
monte_e = monte_e[i], $
monte_k = monte_k[i], $
monte_gam = monte_gam[i], $
monte_dvdt = monte_dvdt[i], $
monte_tcen = monte_tcen, $
monte_ting = monte_ting, $
monte_tegr = monte_tegr, $
monte_stcen = monte_stcen, $
monte_sting = monte_sting, $
monte_stegr = monte_stegr, $
mstar = norbs.mstar[0], $
msini = msini, $
a1sini=a1sini, $
arel=arel, $
prob_t = prob_t, $
prob_st = prob_st, $
/no_transit_plot, $
cf3 = cf3, $
vsini = vsini, $
uvsini = uvsini

monte_msini[i] = msini
monte_a1sini[i] = a1sini
monte_arel[i] = arel

monte_TCen_arr[i] = monte_tcen
monte_TIng_arr[i] = monte_ting
monte_TEgr_arr[i] = monte_tegr
monte_STCen_arr[i] = monte_stcen
monte_STIng_arr[i] = monte_sting
monte_STEgr_arr[i] = monte_stegr
msini_arr[i] = msini

prob_t_arr[i] = prob_t
prob_st_arr[i] = prob_st

plot, (cf3.jd mod monte_p[i]), cf3.mnvel, ps=8
xyouts, [0.2, 0.2], 'monte_p[i] is: ',strt(monte_p[i]), /norm
;wait, 0.5
endfor
print, 'monte_msini is: ', msini

bin = 0.05

plothist, monte_tcen_arr - avg(monte_tcen_arr, /double), bin = bin, $
title = 'Distribution of Time of Center Transit (Before Folding '+$
'To the same orbit)', $
xtitle = 'Deviation from average time of Center Transit (days)'

mdnper = median(monte_p, /double)
offbyper = where(monte_tcen_arr gt ( median(monte_tcen_arr, /double) + $
				mdnper/2.d), ct )
if ct gt 0 then monte_tcen_arr[offbyper] -= mdnper

offbyper2 = where(monte_stcen_arr gt ( median(monte_stcen_arr, /double) + $
				mdnper/2.d), ct )
if ct gt 0 then monte_stcen_arr[offbyper2] -= mdnper

window, 2
plothist, monte_tcen_arr - median(monte_tcen_arr, /double), bin = bin, $
title = 'Distribution of Time of Center Transit', $
xtitle = 'Deviation from median time of Center Transit (days)'

mdnp=median(monte_p, /double)
y = monte_tcen_arr - median(monte_tcen_arr, /double)
y = y + (y lt -1d*mdnp/2d)*mdnp
y = y - (y gt mdnp/2d)*mdnp

window, /free
plothist,y , bin = bin, $
title = 'Distribution of Time of Center Transit', $
xtitle = 'New Deviation from median time of Center Transit (days)'

y += median(monte_tcen_arr, /double)
stop
monte_tcen_arr = y
stop

if keyword_set(frz_ecc) then print, 'frz_ecc is: ', median(monte_e, /double)
if keyword_set(frz_per) then print, 'frz_per is: ', median(monte_p, /double)

print, '---------------------------------------------------------'
print, '        PARS BEFORE TRANSIT IN TRANSIT_MONTE:  '
print, '---------------------------------------------------------'
print, 'p is: ', median(monte_p, /double)
print, 'tp is: ', median(monte_tp, /double)
print, 'k is: ', median(monte_k, /double)
print, 'om is: ', median(monte_om, /double)
print, 'e is: ', median(monte_e, /double)
print, 'gam is: ', median(monte_gam, /double)
print, 'dvdt is: ', median(monte_dvdt, /double)
print, '---------------------------------------------------------'

;orbel=[p, tp, e, om, k, gam, dvdt]
orbel = dblarr(nplanets*7)

for i=0, nplanets-1 do begin
orbel[i*7:i*7+6] = [median(outarr[i*7  ,*], /double), $
						  median(outarr[i*7+1,*], /double), $
						  median(outarr[i*7+2,*], /double), $
						  median(outarr[i*7+3,*], /double), $
						  median(outarr[i*7+4,*], /double), $
						  median(outarr[i*7+5,*], /double), $
						  median(outarr[i*7+6,*], /double)]
endfor;create orbel

print, '---------------------------------------------------------'
print, '        TIMES BEFORE TRANSIT IN TRANSIT_MONTE:  '
print, '---------------------------------------------------------'
print, jul2cal(median(monte_ting_arr) + 2.44d6)
print, jul2cal(median(monte_tcen_arr) + 2.44d6)
print, jul2cal(median(monte_tegr_arr) + 2.44d6)
print, '---------------------------------------------------------'
;stop

;This will generate the plot to be used in the final HTML file:
;NOTE THAT YOU ARE USING BINNED VELOCITIES!
print, 'time_stamp before transit: ', time_stamp
stop
transit, star = star, $
cf3 = cf3, $
knownstarrad=knownstarrad, $
monte_p = median(monte_p, /double), $
monte_Tp = median(monte_Tp, /double), $
monte_om = median(monte_om, /double), $
monte_e = median(monte_e, /double), $
monte_k = median(monte_k, /double), $
monte_gam = median(monte_gam, /double), $
monte_dvdt = median(monte_dvdt, /double), $
mstar=mstar, $
orbel = orbel, $
plidx = plidx, $
prob_t = prob_t, /no_transit_plot, /romac, $
frz_ecc = median(monte_e, /double), $
frz_per = median(monte_p, /double), $
frz_k = median(monte_k, /double), $
stddvtrans_no = stddvtrans_no, $
time_stamp = time_stamp, $
use_monte_ting = median(monte_ting_arr, /double), $
use_monte_tegr = median(monte_tegr_arr, /double)

print, 'time_stamp after transit: ', time_stamp
stop

m_NEPT = 1.0244d26 ;kg
r_NEPT = 2.4764d7 ;m
rho_NEPT = m_nept / (4d / 3d * !dpi * r_NEPT^3)

m_jup = 1.8987d27 ;kg
r_jup = 7.1492d7
rho_jup = m_jup/ (4d / 3d * !dpi * r_jup^3)

m_earth = 5.98d24 ;kg
r_earth = 6.378d6
rho_earth = m_earth/ (4d / 3d * !dpi * r_earth^3)

;use earth masses:
m_es = msini*m_jup/m_earth
rho_pl = (m_es le 10d)*rho_earth + $
         ((m_es gt 10d) AND (m_es lt 20d))*rho_nept + $
         (m_es ge 20d)*rho_jup

if rho_pl eq rho_JUP then dense='GAS GIANT'
if rho_pl eq rho_nept then dense='ICE GIANT'
if rho_pl eq rho_earth then dense='TERRESTIAL'

print, 'This planet is a ', dense, ' planet.'

m_sun = 1.989d30 ;kg
r_sun = 6.95d8 ;m
albedo = 0.35d

r_planet = (3d * msini*m_jup / (4d * !dpi * rho_pl) )^(1d / 3d)

tdepth = r_planet^2 / (knownstarrad*r_sun)^2
stdepth = albedo*r_planet^2 / ((knownstarrad*r_sun)^2 + albedo*r_planet^2)


depthmmag = -2.5*alog10(1-tdepth) *1000d ;1000X for millimag
depth_st_mmag = -2.5*alog10(1-stdepth) *1000d ;1000X for millimag

;maxrm=0d
;stop
rm_amp, maxrm=maxrm, rstar=knownstarrad, $
        normanimation=normanimation, $
        rpl=r_planet/r_jup, vsini=vsini[0], $
        teff=norbs.teff
;stop

;From Gaudi, Winn 2007 ApJ 655:550-563:
ampv = vsini[0]
if ampv le 0 then ampv=2.46
unc_rmamp=52.8*(ampv/5d)*(r_planet/r_jup)^2d*(knownstarrad)^(-2d)


transit_print_tex, $
mdnper, $
median(monte_Tp), median(monte_e), $
median(monte_om), median(monte_k), $
strt(median(prob_t_arr, /double)), $
strt(median(prob_st_arr, /double)), $
median(monte_Tcen_arr, /double), $
median(monte_STcen_arr, /double), $
arel = median(monte_arel, /double), $
unc_arel = stddev(monte_arel, /double), $
chi = median(chiarr, /double), $
class=dense, $
comp = comp[plidx], $
depthmmag = depthmmag, $
depth_st_mmag = depth_st_mmag, $
dvdt = median(monte_dvdt, /double), $
unc_dvdt = stddev(monte_dvdt, /double), $
e_unc = stddev(monte_e, /double), $
extitle = star, $
gam = median(monte_gam, /double), $
unc_gam = stddev(monte_gam, /double), $
iter = strt(incs), $
k_unc = stddev(monte_k, /double), $
lastob = jul2cal(max(cf3.jd) + 2.44d6, /date), $
msini = msini, $
unc_msini = stddev(monte_msini, /dou), $
mstar = mstar/m_sun, $
unc_mstar = unc_mstar, $
nobs = strt(n_elements(cf3.jd)), $
nplanets = nplanets, $
om_unc = stddev(monte_om, /double), $
planetid=planetid, $
p_unc = stddev(monte_p, /double), $
rmamp = maxrm, $
unc_rmamp = unc_rmamp, $
rms = median(rmsarr, /double), $
rstar = knownstarrad, $
unc_rstar = unc_rstar, $
stddvtrans_no = stddvtrans_no, $
stdepth = stdepth, $
stdur_unc = stddev(monte_stegr_arr - monte_sting_arr, /double), $
stegr = median(monte_stegr_arr, /double) + 2.44d6, $
sting = median(monte_sting_arr, /double) + 2.44d6, $
tdepth = tdepth, $
tegr = median(monte_tegr_arr, /double) + 2.44d6, $
time_stamp = time_stamp, $
ting = median(monte_ting_arr, /double) + 2.44d6, $
Tp_unc = stddev(monte_tp, /double), $
transit_stdev = STDDEV(monte_TCen_arr, /double), $
transit_st_stdev = STDDEV(monte_STCen_arr, /double), $
tdur_unc = stddev(monte_tegr_arr - monte_ting_arr, /double), $
vsini = vsini[0], $
uvsini = uvsini[0]
;END OF TRANSIT_PRINT_TEX
stop

;stop
transit_print, $
mdnper, $
median(monte_Tp), median(monte_e), $
median(monte_om), median(monte_k), $
strt(median(prob_t_arr, /double)), $
strt(median(prob_st_arr, /double)), $
median(monte_Tcen_arr, /double), $
median(monte_STcen_arr, /double), $
arel = median(monte_arel, /double), $
unc_arel = stddev(monte_arel, /double), $
chi = median(chiarr, /double), $
class=dense, $
comp = comp[plidx], $
depthmmag = depthmmag, $
depth_st_mmag = depth_st_mmag, $
dvdt = median(monte_dvdt, /double), $
unc_dvdt = stddev(monte_dvdt, /double), $
e_unc = stddev(monte_e, /double), $
extitle = star, $
gam = median(monte_gam, /double), $
unc_gam = stddev(monte_gam, /double), $
iter = strt(incs), $
k_unc = stddev(monte_k, /double), $
lastob = jul2cal(max(cf3.jd) + 2.44d6, /date), $
msini = msini, $
unc_msini = stddev(monte_msini, /dou), $
mstar = mstar/m_sun, $
unc_mstar = unc_mstar, $
nobs = strt(n_elements(cf3.jd)), $
nplanets = nplanets, $
om_unc = stddev(monte_om, /double), $
planetid=planetid, $
p_unc = stddev(monte_p, /double), $
rmamp = maxrm, $
unc_rmamp = unc_rmamp, $
rms = median(rmsarr, /double), $
rstar = knownstarrad, $
unc_rstar = unc_rstar, $
stddvtrans_no = stddvtrans_no, $
stdepth = stdepth, $
stdur_unc = stddev(monte_stegr_arr - monte_sting_arr, /double), $
stegr = median(monte_stegr_arr, /double) + 2.44d6, $
sting = median(monte_sting_arr, /double) + 2.44d6, $
tdepth = tdepth, $
tegr = median(monte_tegr_arr, /double) + 2.44d6, $
time_stamp = time_stamp, $
ting = median(monte_ting_arr, /double) + 2.44d6, $
Tp_unc = stddev(monte_tp, /double), $
transit_stdev = STDDEV(monte_TCen_arr, /double), $
transit_st_stdev = STDDEV(monte_STCen_arr, /double), $
tdur_unc = stddev(monte_tegr_arr - monte_ting_arr, /double), $
vsini = vsini[0], $
uvsini = uvsini[0]



medianper=strcompress(string(median(monte_p, /double)),/remove_all)
pererr=strcompress(string(stddev(monte_p, /double)),/remove_all)
mediantp= median(monte_tp, /double)
tperr= stddev(monte_tp, /double)
medianecc=strcompress(string(median(monte_e)),/remove_all)
eccerr=strcompress(string(stdev(monte_e)),/remove_all)
medianom=strcompress(string(median(monte_om)),/remove_all)
omerr=strcompress(string(stdev(monte_om)),/remove_all)
mediank=strcompress(string(median(monte_k)),/remove_all)
kerr=strcompress(string(stdev(monte_k)),/remove_all)

print, '----------------------------------------------------------'
print, '             FINAL ORBITAL PARAMETERS'
print, '----------------------------------------------------------'

;a is string
;f f15.5
form1 = '(a6, a10, a5, a9)'
form0 = '(a6, f13.5, a5, f10.5)'
form2 = '(f15.5)' ; use this normally for Tp (if it wasn't already a string
form3 = '(i15)' ;for integers

print,'Per: ', medianper, ' +/- ', pererr, format = form1
print,'Tp: ', mediantp, ' +/- ', tperr, format = form0
print,'Ecc: '+medianecc+' +/- '+eccerr
print,'Om: '+medianom+' +/- '+omerr
print,'K: '+mediank+' +/- '+kerr
print,'Median msini: ',median(monte_msini, /dou),' +/- ', $
	stddev(monte_msini, /dou)
print,'Median arel: '
print,'Median RMS: ',median(rmsarr)
print,'Median chisq: ',median(chiarr)
stop
print, 'The median time of central transit is: ', $
	jul2cal(median(monte_Tcen_arr + 2.44d6, /double))
	
print, 'Which has a julian number of: ', $
	strt(floor(median(monte_tcen_arr, /double ))), $
	strmid(strt(median(monte_tcen_arr, /double ) - $
	floor(median(monte_tcen_arr, /double ))), $
	1, strlen(strt(median(monte_tcen_arr, /double ) - $
	floor(median(monte_tcen_arr, /double ))))), $
 ' +/- ', strt(STDDEV(monte_TCen_arr, /double))
 

tdur = (median(monte_tegr_arr, /double) - $
       median(monte_ting_arr, /double))* 24d

stdur = (median(monte_stegr_arr, /double) - $
       median(monte_sting_arr, /double))* 24d


database = create_struct('chi', median(chiarr, /double), $
                         'a1sini', median(monte_a1sini, /double), $
                         'unc_a1sini', stddev(monte_a1sini, /double), $
                         'arel', median(monte_arel, /double), $
                         'unc_arel', stddev(monte_arel, /double), $
                         'comp', comp[plidx], $
                         'depthmmag', depthmmag, $
                         'depth_st_mmag', depth_st_mmag, $
                         'dist', norbs.dist, $
                         'udist', norbs.udist, $
                         'dvdt', median(monte_dvdt), $
                         'unc_dvdt', stddev(monte_dvdt, /double), $
                         'e', median(monte_e), $
                         'unc_e', stddev(monte_e, /dou), $
                         'gam', median(monte_gam), $
                         'unc_gam', stddev(monte_gam, /dou), $
                         'iter', strt(incs), $
                         'k', median(monte_k), $
                         'unc_k', stddev(monte_k, /double), $
                         'lastob',jul2cal(max(cf3.jd) +2.44d6, /date), $
                         'lum',norbs.lum, $
                         'msini', msini, $
                         'unc_msini', stddev(monte_msini, /dou), $
                         'mstar', mstar, $
                         'unc_mstar', unc_mstar, $
                         'name', star, $
                         'nobs', strt(n_elements(cf3.jd)), $
                         'nplanets', nplanets, $
                         'om', median(monte_om), $
                         'unc_om', stddev(monte_om, /double), $
                         'per', mdnper, $
                         'unc_per', stddev(monte_p, /dou), $
                         'planetnum', planetid, $
                         'plx', norbs.plx, $
                         'uplx', norbs.uplx, $
                         'prob_t', strt(median(prob_t_arr)), $
                         'prob_st', strt(median(prob_st_arr)), $
                         'rmamp', maxrm, $
                         'unc_rmamp', unc_rmamp, $
                         'rms', median(rmsarr, /double), $
                         'rstar', knownstarrad, $
                         'unc_rstar', norbs.unc_rstar, $
                         'STcen', median(monte_STcen_arr, /double), $
                         'stdepth', stdepth, $
                         'stdrat', stddvtrans_no, $
                         'stdur', stdur, $
                         'Tcen', median(monte_Tcen_arr, /double), $
                         'unc_tcen', STDDEV(monte_TCen_arr, /double), $
                         'tdepth', tdepth, $
                         'tdur', tdur, $
                         'teff', norbs.teff, $
                         'teff_unc', norbs.teff_unc, $
                         'time_stamp', time_stamp, $
                         'Tp', median(monte_Tp, /double), $
                         'Tp_unc', stddev(monte_tp, /double), $
                         'vsini', vsini[0], $
                         'uvsini', uvsini[0])
    
    
    
    
    
end; transit_monte.pro