function rv_drive_mp, t, orbel, quiet = quiet

; Calculates barycentric velocities of an SB1 from the orbital
; parameters.  For multiple planets, simply adds Keplerians.

  ;orbel is a 6n-element vector containing P,tp,e,om,k,cmvel 
  ;units are days, days, dimensionless, degrees, m/s, m/s
  
  rv = t*0
  neo = n_elements(orbel)
  n = neo/7 ;number of planets

  if total(~finite([orbel, t])) gt 0 then begin
    print, 'Bad inputs to rv_drive'
    stop
  endif

  for i = 0, n-1 do begin

    p = double(orbel[0+i*7])
    tp = double(orbel[1+i*7])
    e = double(orbel[2+i*7])
    om = double(orbel[3+i*7]*!dtor)
    k = double(orbel[4+i*7])
    gamma = double(orbel[5+i*7])
    dvdt = double(orbel[6+i*7])
    curv = 0
    if i eq 0 and neo/7*7 eq neo-1 then curv = double((reverse(orbel))[0])
    

    if ~keyword_set(quiet) and (p lt 0 or e lt 0 or e ge 1 or k lt 0) then begin
      print, 'Bad inputs to rv_drive'
;      stop
    endif

    ;Error checking
    if p lt 0 then p = 1d-2
    if e lt 0 then e = 0
    if e ge 1 then e = 0.99
    if k lt 0 then k = 1d-2

    
; Calculate the approximate eccentric anomaly, E1, via the mean
; anomaly, M.
    
    M = 2.d0*!dpi*( ((t-tp)/P) - floor((t-tp)/P))
    E1 = kepler(M, e)
    
; Calculate nu
    n1 = 1.d0 + e
    n2 = 1.d0 - e
    nu = 2.d0*atan((n1/n2)^0.5*tan(E1/2.d0))
    
; Calculate the radial velocity
;T0_trend=14000
    rv = rv+k*(cos(nu+om)+e*cos(om))+gamma+dvdt*(t-14000)+curv*(t-14000)^2  ;Default epoch for planet seaarch epoch
  endfor

  return, rv

end
   







