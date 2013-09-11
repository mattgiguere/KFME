;***********************************************************************
; NAME: RV_MPFIT_RV
;																	   
; PURPOSE: A procedure for use by MPFITFUN that will find the best fit
;	for the radial velocity data for RV_WIDGET. 
;																	   
; CATEGORY: MPF							   
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
;     c. Matt Giguere, June 26, 2008 5:03:52 PM PDT
;***********************************************************************
function rv_mpfit_rv, X, P, _EXTRA=functargs

n_planets = functargs.n_planets
dew24arr = (*functargs.dew24)
dew39arr = (*functargs.dew39)

period=fltarr(n_planets)
m_pl_earth=fltarr(n_planets)
ecc=fltarr(n_planets)
inc=fltarr(n_planets) + 90d
big_om=fltarr(n_planets)
om=fltarr(n_planets)
tp_rv=fltarr(n_planets)
gam = fltarr(n_planets)
dvdt = fltarr(n_planets)
curve = fltarr(n_planets)

gam[0] = double(p[5*n_planets ])  ;center of mass velocity (m/s)
dvdt[0] = double(p[5*n_planets + 1]) ;linear trend
curve[0] = double(P[5*n_planets+ 2]) ;curvature trend
dew24val = double(P[5*n_planets+ 3]) ;offset from dewar 24
dew39val = double(P[5*n_planets+ 4]) ;offset from dewar 39

for planet=0,n_planets-1 do begin
  period[planet]=P[planet*5]   ; days
  if period[planet] eq 0 then period[planet] = 1d8
  m_pl_earth[planet]=P[planet*5+1]    ; earth masses
  if m_pl_earth[planet] eq 0 then m_pl_earth[planet] = 1d-8
  ecc[planet]=P[planet*5+2]
;  inc[planet]=P[planet*7+3]   ;deg
;  big_om[planet]=P[planet*7+4]    ;deg
  om[planet]=P[planet*5+3]  ;deg
  tp_rv[planet]=P[planet*5+4]  ; days
endfor

for u=0, n_planets-1 do begin
 while om[u] gt 360. do om -=360.
 while big_om[u] gt 360. do big_om -=360.
endfor



m_star = functargs.m_star
timerv = X
n_obs_rv = n_elements(timerv)
fitrv = fltarr(n_obs_rv)

for planet=0,n_planets-1 do begin
  a_pl=((period[planet]/365.2564d)^2*m_star)^(1./3.)

  k = mpf_K(a_pl, m_pl_earth[planet], period[planet], m_star, inc[planet], ecc[planet])

  par_rv = [period[planet], m_pl_earth[planet], ecc[planet], $
  inc[planet], big_om[planet], om[planet], m_star, tp_rv[planet], k, $
  gam[planet], dvdt[planet], curve[planet]]

  fitrv += mpf_rv_thvel(timerv, par_rv)
endfor
if total(dew24arr) ge 0 then  fitrv[dew24arr] += dew24val
if total(dew39arr) ge 0 then   fitrv[dew39arr] += dew39val
return, fitrv

end;rv_mpfit_rv