;+
;Fits Keplerian Velocity Curve to RV data set
;v0.91
;function rv_fit_mp, $
;             t_in, v_in, e_in, $
;             tve4, tve5, tve6, $
;             telvec = telvec_in, $
;             orbel = orbel_in, $   ;explicitly specified starting guesses
;             fixed = fixed_in, $   ;binary array of fixed parameters -- only useful for P, tp, e, and dvdt
;             nplanets = nplanets, $       ;if no orbel_in, specify number of planets to fit
;             pers = per, $                 ;starting per overide
;             tps = tp, $                   ;starting tp override
;             circ = circ, $   ;force e=0;
;             eccs = ecc, $    ; starting ecc override
;             trend = trend, $             ;fit dvdt override
;             plotfit = plotfit, $         ;plot fit
;             yfit = yfit, chi = chi, rms = rms, $;fit parameters
;             tveout = tveout,$              ;combined tve structure
;             quiet = quiet, $
;             permax = permax, $
;             permin = permin, $
;             tpmax  = tpmax, $
;             tpmin  = tpmin, $
;             eccmin = eccmin, $
;             eccmax = eccmax, $             ;upper limit of ecc
;             time_base = time_base, $     ;
;             offset = offset, $           ;offset between telescopes
;             residuals=residuals,$        ;residuals to fit
;             jitter=jitter,$              ;errors to be added in quadrature to all data points
;             _extra = plotextra           ;parameters passed to plotting routines
;
;INPUT
;     Inputs may be in the form of tve structures or (in the case of a single telescope) times, velocities, and errors.
;      
;      t_in        times of observations or tve structure for telescope 1
;      v_in        measures RVs or tve structure for telescope 2
;      e_in        errors in RVs or tve structure for telescope 3
;      tve4, tve5, tve6  tve structures for telescopes 4-6
;      
;OPTIONAL INPUTS:
;      telvec: if data is specified as a sequence of times, velocities, and errors (instead of with tve structures), this optional vector (of the same length as t_in, v_in, and e_in) contains the corresponding telescope identifier of each observation (telescope identifiers can be anything -- "Keck", 1, 7.6, etc.)
;      orbel:  input orbital elements - 7*n dimensional vector: [P,tp,e,om,K,gamma,dvdt]
;      fixed:  - 7*n dimensional vector which orbital elements to fix.  Defaults to only dvdt fixed.  Only P, tp, e, and dvdt may be fixed.
;      nplanets: forces number of planets to be fit. 
;      pers:  specifies input periods, overriding values in orbel.  Periods must be specified here or in orbel except in 1 planet case.
;      tps, eccs: specifies starting tp and ecc values.
;      circ: vector of flags specifying those planets for which ecc should be fixed at 0.
;      trend: flag indicating that dvdt sholud be a free paramater
;      plotfit: flag indicating that a rudamentary time - velocity plot with the best fit should be displayed to current device.  Additional plot keywords will be passed to the initial plot command.
;      quiet: flag supressing verbose output;
;      xxxmyy: where xxx={per,tp,ecc} and yy={in,ax}:  Vectors specifing the minimum and/or maximum range for P, tp, and/or e for all planets.  To specify a limit for only one planet, set other planets' limits to reasonable values (i.e. Pmin > 0.1d, Pmax < 100y, e > 0, e < 0.999, etc.).
;      jitter: errors to be added in quadrature to all data points.  ***NOTE THAT THESE JITTERS WILL BE REFLECTED IN TVEOUT.ERRVEL****
;
;
;OUTPUT
;      return value: [p, tp, e,om,k,gamma,dvdt]  a 7*n length vector. gamma and dvdt =0 for all planets after first.
;
;OPTIONAL KEYWORD OUTPUTS:
;      tveout:  tve structure of input. times, velocities, and errors.  This option allows rv_fit_mp to generate tve structures for the user from input times, velocities, and errors.  If multiple telescopes were specified, this structure combines their data into a single structure after applying the best-fit offset between their velocities.
;
;      offset: best-fit value(s) of offset(s) between (among) the various telescopes specified
;      yfit: A vector containing the model fit at the times of observation.
;      chi, rms: sqrt(reduced chi^2) and r.m.s. residuals to best fit.
;      residuals: vector of residuals to the fit.  Equivalent to tveout.mnvel-yfit.
; 
;
;Written by JTW 12-2007, based upon onekep.pro by G. Marcy et al.
;v1.1 -- fixed eccmin bug -- eccmin now implemented
;
;-

function rv_fit_mp_drive, np, orbel, fixed, $
t, v, e, telvec = telvec, errmsg = errmsg, perror = perror, $
covar = covar, status = status, bestnorm = bestnorm, niter = niter, $
nfree = nfree, permin = permin, permax = permax, tpmin = tpmin, $
tpmax = tpmax, eccmin = eccmin, eccmax = eccmax, offset = offset, $
time_base = time_base, quiet = quiet

  ignp7 = indgen(np)*7
  iper = ignp7
  itp =  ignp7+1
  iecc = ignp7+2
  iom = ignp7+3
  ik = ignp7+4
  igam = ignp7+5
  idvdt = ignp7+6

;Load up parinfo valiable for mpfit
  parinfo = replicate({value:0d, fixed:0b, limited:[0b, 0b], limits:[0d, 0d]}, np*3)
  pii = indgen(np)*3
  piper = pii
  pitp = pii+1
  piecc = pii+2
  
  parinfo[piper].value = orbel[iper]
  parinfo[pitp].value = orbel[itp]
  parinfo[piecc].value = orbel[iecc]

  parinfo[piper].fixed = fixed[iper]
  parinfo[pitp].fixed = fixed[itp]
  parinfo[piecc].fixed = fixed[iecc]

  parinfo[piper].limited = [1, 1]
  parinfo[pitp].limited = [1, 1]
  parinfo[piecc].limited = [1, 1]

  if n_elements(permax) eq 0 then permax = replicate(100*365d, np)
  if n_elements(permin) eq 0 then permin = replicate(0.1, np)
  if n_elements(tpmax) eq 0 then tpmax = max(t)+orbel[iper]
  if n_elements(tpmin) eq 0 then tpmin = min(t)-orbel[iper]
  if n_elements(eccmax) eq 0 then eccmax = replicate(0.99, np)
  if n_elements(eccmin) eq 0 then eccmin = replicate(0., np)

  for i = 0, np-1 do begin
  	;set the limits for all 3 parameters for all planets:
    parinfo[piecc[i]].limits = [eccmin[i], eccmax[i]]   ;set limits
    parinfo[pitp[i]].limits = [tpmin[i], tpmax[i]]
    parinfo[piper[i]].limits = [permin[i], permax[i]]

	;now set the values:
    parinfo[piecc[i]].value = parinfo[piecc[i]].limits[0] > $
			parinfo[piecc[i]].value < $
			parinfo[piecc[i]].limits[1]  ;put guesses within limits
    parinfo[piper[i]].value = parinfo[piper[i]].limits[0] > $
			parinfo[piper[i]].value < $
			parinfo[piper[i]].limits[1]
    parinfo[pitp[i]].value = parinfo[pitp[i]].limits[0] > $
			parinfo[pitp[i]].value < $
			parinfo[pitp[i]].limits[1]
  endfor

  enfa0 = (parinfo[piecc].value eq 0 and ~parinfo[piecc].fixed)     ;where it's allowed to float...
  parinfo[piecc].value = parinfo[piecc].value*(~enfa0)+0.01*(enfa0) ;lift e off of 0 to start
  

  if n_elements(time_base) eq 0 then time_base =  median(t)
;stop
;Normalize parameters
  timenorm = t-time_base
  parinfo[pitp].limits = parinfo[pitp].limits-time_base ;we want these to be the same order of magnitude
  parinfo[pitp].value = parinfo[pitp].value-time_base ;so we subtract time_base from tp
  parinfo[piecc].limits = parinfo[piecc].limits*1000 ;and multiply e by 1000
  parinfo[piecc].value = parinfo[piecc].value*1000
;end normalizing parameters
;stop
  if fixed[6] and orbel[idvdt[0]] ne 0 then $ ;if we're fixing dvdt, we need to subtract it
    vt = double(v)-orbel[idvdt[0]]*(t-time_base) $
  else vt = double(v)


  if fixed[5] and orbel[igam[0]] ne 0 then $ ;if we're fixing gamma, we need to subtract it
    vt = v-orbel[igam[0]]
  
  functargs = {time:timenorm,  velocity:vt, error:double(e), $
  				trend:~fixed[6], telvec:telvec, epoch:time_base}

;stop
  parmp = mpfit('rvlin', functargs = functargs, parinfo = parinfo, errmsg = errmsg, $
  			perror = perror, covar = covar, status = status, bestnorm = bestnorm, $
  			niter = niter, ftol = 1d-15, xtol = 1d-15, autoderivative = 0, quiet = quiet)

  if status eq 0 then stop
  ;stop

  junk = rvlin(parmp, time = timenorm, velocity = vt, error = e, pars = par, trend = ~fixed[6], telvec = telvec, offset = offset, epoch = time_base)

  nfree = total(~parinfo.fixed)+n_elements(offset)+(~fixed[6])+np*2+(~fixed[5])

  if fixed[5] and orbel[igam[0]] ne 0 then par[5] = orbel[igam[0]]
  if fixed[6] and orbel[idvdt[0]] ne 0 then par[6] = orbel[idvdt[0]]

  return, par

end


function rv_fit_mp, $
            t_in, v_in, e_in, $
            tve4, tve5, tve6, $
            telvec = telvec_in, $
            orbel = orbel_in, fixed = fixed_in, $;specify starting guesses and fixed params
            nplanets = nplanets, $       ;if no orbel_in, specify number of planets to fit
            pers = per, $                 ;starting per overide
            tps = tp, $                   ;starting tp override
            circ = circ, eccs = ecc, $    ;force e=0; starting ecc override
            trend = trend, $             ;fit dvdt override
            plotfit = plotfit, $         ;plot fit
            yfit = yfit, chi = chi, rms = rms, $;fit parameters
            tveout = tveout,$              ;combined tve structure
            quiet = quiet, $
            permax = permax, $
            permin = permin, $
            perror = perror, $
            tpmax  = tpmax, $
            tpmin  = tpmin, $
            eccmin = eccmin, $
            eccmax = eccmax, $             ;upper limit of ecc
            time_base = time_base, $     ;
            offset = offset, $           ;offset between telescopes
             residuals=residuals,$        ;residuals to fit
             jitter=jitter,$              ;errors to be added in quadrature to all data points
            _extra = plotextra           ;parameters passed to plotting routines


;Default for California Planet Search Spectra -- consistent with rv_drive
if n_elements(time_base) eq 0 then time_base = 14000 

nparams = n_params()

if nparams eq 0 then begin
  print, 'Input data required'
  stop
  return, [-1]
endif


if nparams eq 3 and datatype(t_in) ne 'STC' then begin ;NO tve files
  nt = n_elements(t_in)
  ntv = n_elements(telvec_in)
  telvec = intarr(nt)

  t = t_in
  v = v_in
  e = e_in
  if ntv ne nt then begin   ;no good telvec array
    if nt eq 0 then print, 'Telvec array not of same length as times of observations -- ignoring'
    ntels = 1
    utel = 0
  endif else begin  ;good telvec array
    utel = telvec_in[uniq(telvec_in, sort(telvec_in))]
    ntels = n_elements(utel)
    for i = 0, ntels-1 do telvec[where(telvec_in eq utel[i])] = i
  endelse

endif else begin                       ;tve files
  ntels = nparams
  utel = indgen(ntels)
  for i = 0, ntels-1 do begin        ;for each tve file...
    case i of
      0:tve = t_in
      1:tve = v_in
      2:tve = e_in
      3:tve = tve4
      4:tve = tve5
      5:tve = tve6
    endcase
    if i eq 0 then begin
      t = tve.jd
      v = tve.mnvel
      e = tve.errvel
      telvec = intarr(n_elements(t))
    endif else begin
      t = [t, tve.jd]
      v = [v, tve.mnvel]
      e = [e, tve.errvel]
      telvec = [telvec, intarr(n_elements(tve))+i]
    endelse
  endfor
endelse

if n_elements(jitter) eq 1 then e = sqrt(e^2+jitter^2)

st = sort(t)
t = t[st]
v = v[st]
e = e[st]
telvec = telvec[st]

nobs = n_elements(t)
if n_elements(e) ne nobs then begin
  print, 'Problem:  bad uncertainties in v -- assuming constant'
  e = t*0+1
endif

if n_elements(v) ne nobs then begin
  print, 'Problem:  t, v, and e must have same number of elements'
  stop
  return, [-1]
endif


;TIMES
tmin = min(t)
tlen = max(t)-min(t)

;Interpret arguments and do error checking:

nino = n_elements(orbel_in)

;Priority for number of planets:  nplanets keyword, then greater of pers keyword & orbel keyword
np = -1    ;number of planets
if n_elements(nplanets) gt 0 then np = nplanets   ;Nplanet override keyword
if np le 0 then np = n_elements(per)              ;Pers keyword 
ninop = (3+nino)/7
if np lt ninop then np = ninop                    ;orbel keyword
if np le 0 then np = 1                            ;Default to 1

nan = !values.d_nan

orbel = dblarr(np*7)+nan       ;set to nan to force good 1st guess below

if nino gt 0 then $              ;check input orbel array
  orbel[0:nino-1] = orbel_in       ;Set user-specified defaults

ignp7 = indgen(np)*7            ;Every 7th index
iper = ignp7
itp =  ignp7+1
iecc = ignp7+2
iom = ignp7+3
ik = ignp7+4
igam = ignp7+5
idvdt = ignp7+6
;stop
orbel[5] = total(orbel[igam])  ;add all gammas, put them in first planet
if np gt 1 then orbel[igam[1:*]] = 0
orbel[6] = total(orbel[idvdt])  ;add all dvdts, put them in first planet 
if np gt 1 then orbel[idvdt[1:*]] = 0

npar = n_elements(orbel)  ;number of independent parameters should =  nplanet*5+2, but npar=nplanet*7
  
if ~finite(orbel[6]) then orbel[6] = 0    ;NaNs are Interpreted as 0.

nfi = n_elements(fixed_in)
fixed = fixed_in       ;User overrides

if n_elements(trend) eq 0 then trend = (orbel[6] ne 0) or ~fixed[6] ;if dvdt is set or is floating, fit a trend

nper = n_elements(per)
if nper gt np then begin
  print, 'Too many period input guesses.'
  stop
endif
if nper gt 0 and nper le np then orbel[indgen(nper)*7] = per  ;load period input overrides

ntp = n_elements(tp)
if ntp gt np then begin
  print, 'Too many Tp input guesses.'
  stop
endif
if ntp gt 0 and ntp le np then orbel[indgen(ntp)*7+1] = tp  ;load tp input overrides

necc = n_elements(ecc)
if necc gt np then begin
  print, 'Too many ecc input guesses.'
  stop
endif
if necc gt 0 and necc le np then orbel[indgen(necc)*7+2] = ecc  ;load ecc input overrides

ncirc = n_elements(circ)
if ncirc gt np then begin
  print, 'Too many circular orbit requests (> N_planets).'
  stop
endif

if ncirc gt 0 and ncirc le np then begin  ;fix ecc=0 where specified.
  wcirc = where(circ, nwcirc)
  if nwcirc gt 0 then begin
    orbel[(indgen(ncirc)*7)[wcirc]+2] = 0    ;ecc=0
    fixed[(indgen(ncirc)*7)[wcirc]+2] = 1    ;fix ecc
  endif
endif

if total(fixed[iom]) gt 0 then begin
  print, 'Warning: Cannot fix omega'
  fixed[iom] = 0
endif
if total(fixed[ik]) gt 0 then begin
  print, 'Warning: Cannot fix K'
  fixed[ik] = 0
endif

eccs = orbel[iecc]
feccs = where(fixed[iecc] and eccs eq 0, nfeccs)  ;find where circular orbits are fixed
if nfeccs gt 0 then begin
  orbel[itp[feccs]] = median(t) ;set tp = mean(t), arbitrarily
  fixed[itp[feccs]] = 1  ;fix tp  (tp is degenerate with omega, which is a linear parameter now)
endif

;Get initial guesses for unspecified parameters

;Periods
pers = orbel[iper]
badper = where(~finite(pers) or pers le 0, nbad)

if nbad gt 0 then begin
  if np gt 1 then begin
    print, 'Need more period guesses'
    stop
    return, [-1]
  endif else begin
    lspergram, t, v, nu_out, peri_out, pkper, pkheight
    if trend then begin
      p = poly_fit(t, v, meas = e, 1)
      vl = v-poly(t, p)
      lspergram, t, vl, nu_out, peri_out, pkper2, pkheight2
      pkper = [pkper, pkper2]
      pkheight = [pkheight, pkheight2]
    endif
    pks = reverse(sort(pkheight))
    orbel[0] = pkper[pks[0]]
  endelse
endif

;tp
tps = orbel[itp]
badtps = where(~finite(tps), nbad)
if nbad gt 0 then orbel[itp[badtps]] = median(t)  ;guess the median of the observations

;ecc
badecc = where(~finite(eccs) or eccs ge 1 or eccs lt 0, nbad)
if nbad gt 0 then orbel[iecc[badecc]] = 0.3  ;guess 0.3

;omega
if total(~finite(orbel[iom])) gt 0 then orbel[iom[where(~finite(orbel[iom]))]] = 0
if total(~finite(fixed[iom])) gt 0 then fixed[iom[where(~finite(fixed[iom]))]] = 0
;k
if total(~finite(orbel[ik])) gt 0 then orbel[ik[where(~finite(orbel[ik]))]] = 0
if total(~finite(fixed[ik])) gt 0 then fixed[ik[where(~finite(fixed[ik]))]] = 0
;gam
if total(~finite(orbel[igam])) gt 0 then orbel[igam[where(~finite(orbel[igam]))]] = 0
if total(~finite(fixed[igam])) gt 0 then fixed[igam[where(~finite(fixed[igam]))]] = 0

if total(~finite(orbel)) gt 0 then stop  ;Huh?
if total(~finite(fixed)) gt 0 then stop    ;Huh?

printpars, orbel, fixed, message = 'Inital fit:', quiet = quiet

par = rv_fit_mp_drive(np, orbel, fixed, t, v, e, telvec = telvec, errmsg = errmsg, $
	perror = perror, covar = covar, status = status, bestnorm = bestnorm, $
	niter = niter, nfree = nfree, eccmax = eccmax, eccmin = eccmin, $
	offset = offset, time_base = time_base, permin = permin, permax = permax, $
	tpmin = tpmin, tpmax = tpmax, quiet = quiet)

printpars, par, fixed, message = "Final Fit:", quiet = quiet

if keyword_set(errmsg) then begin
	print, 'Error message from MPFIT:'
	print, errmsg
endif

vall = v
if ntels gt 1 then begin
  for i = 1, ntels-1 do begin
    tel = utel[i]
    j = where(telvec eq i)
    vall[j] = vall[j]-offset[i-1]
  endfor
endif

dev = (vall-rv_drive_mp(t, par))
chi2 = total((dev/e)^2)
;print, chi2, n_elements(t), nfree, chi2/(n_elements(t)-nfree)
chi = sqrt(chi2/(n_elements(t)-nfree))
rms = sqrt(total(dev^2)/(n_elements(t)-nfree))

tveout = {jd:0d, mnvel:0d, errvel:0d}
tveout = replicate(tveout, n_elements(t))
tveout.jd = t
tveout.mnvel = vall
tveout.errvel = e

tveout = tveout[sort(tveout.jd)]

if ~keyword_set(quiet) then begin
  ;only print niter if it exists: (0 == UNDEFINED)
  if size(niter, /type) ne 0 then print, 'Niter: '+string(niter)
  print, 'Reduced Chi^2:   '+sigfig(chi^2, 0.1)
  print, 'RMS  :   '+sigfig(rms, 0.1)
endif

if keyword_set(plotfit) then begin
  plot, t, vall, ps = 8, _extra = plotextra
  m = makearr(10000, !x.crange)
  oplot, m, rv_drive_mp(m, par)
endif

yfit = rv_drive_mp(t, par)

residuals = tveout.mnvel-yfit

return, par

end

