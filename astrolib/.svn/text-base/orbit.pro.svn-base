pro orbit,mrat,msum,period,semimajor,eccentric,inclin,viewangle $
  ,time,vel1,vel2,plot=plot
;Simulation of binary orbit.
; mrat (input scalar) ratio of component masses.
; msum (input scalar) sum of component masses [solar masses]
; period (input scalar) orbit period [solar days]
; semimajor (input scalar) semi-major axis of orbit [AU]
; eccentric (input scalar) eccentricity of orbit
; inclin (input scalar) angle between line of sight and axis of orbit.
; viewangle (input scalar) angle between line of sight and the line
;  connecting periastron and apoastron [degrees]
; time (output vector) time at each orbit step [days]
; vel1 (output vector) velocity of mass 1 [km/s]
; vel2 (output vector) velocity of mass 2 [km/s]
;Set one of massfunc, period, or semimajor to zero. It will be computed
;  from the two positive parameters.
;See Goldstein - Classical Mechanics, Sections 3-7 and 3-8.

if n_params() lt 7 then begin
  print,'syntax: orbit,mrat,msum,p,a,e,i,angle [time,vel1,vel2,/plot]'
  retall
endif

;print,'This program is sort of tested!  Believe results at your own risk....'

;Variables used in program:
;  mr - ratio of masses in binary
;  m - sum of masses in binary (grams)
;  m1 - mass of star 1 (grams)
;  m2 - mass of star 2 (grams)
;  P - period (seconds)
;  a - semi-major axis (cm)
;  e - eccentricity
;  inc - angle between orbital axis and line of sight (radians)
;  ang - angle between line of sight and x-axis (radians)
;  Msun - mass of sun (g)
;  day - seconds in a solar day (s)
;  AU - astronomical unit (cm)
;  G = Newton's constant of gravitation (cgs: cm^3/(g s^2))
;  n = number of orbit (psi) steps
;  psi - mean anomoly
;  omega - angular frequency of orbit (2*pi/period)
;  t - orbit phase (s)
;  theta - angle from periastron (radians)
;  costh - cosine of theta
;  sinth - cosine of theta
;  t - elapsed time (s)
;  x - coordinate along line connecting periapsis and apoapsis (AU)
;  y - coordinate perpendicular to x (AU)
;  dt - time interval between orbit (psi) steps (s)
;  dx - distance interval along x-axis between orbit (psi) steps (km)
;  dy - distance interval along y-axis between orbit (psi) steps (km)
;  vx - velocity along x-axis (km/s)
;  vy - velocity along y-axis (km/s)

;Constants.
  Msun = 1.9889225d33
  day = 24d0 * 60d0 * 60d0
  AU = 1.49597870662d13
  G = 6.6725985d-8

;Convert arguments to double precision.
  mr = double(mrat)
  m = double(msum) * Msun
  P = double(period) * day
  a = double(semimajor) * AU
  inc = double(inclin) * !dtor
  e = double(eccentric)
  ang = double(viewangle) * !dtor

;Find missing parameter.
  if m le 0 then begin				;no mass function
    m = 4 * (!dpi / P)^2.0 * a / G
;    print,'Sum of masses (solar masses):         ',m/Msun
  endif
  if P le 0 then begin				;no period
    P = 2 * !dpi * a^1.5 / sqrt(G * m)
;    print,'Orbital period (days):                ',P/day
  endif
  if a le 0 then begin				;no semi-major axis
    a = (G * m * (P / !dpi)^2.0 / 4) ^ (1d0/3.0)
;    print,'Semi-major axis (AU):                 ',a/AU
  endif

;Calculate orbit.
  n = 1001
  psi = 2 * !dpi * dindgen(n) / (n-1)
  cospsi = cos(psi)				;compute only once
  omega = 2 * !dpi / P
; t = (psi - e * sin(psi)) / omega		;time
  t = 0.5 * (psi - e * sin(psi)) / omega	;time
  r = a * (1 - e * cospsi)			;radius
  costh = (cospsi - e) / (1 - e*cospsi)
  sinth = sqrt(1 - costh^2.0)
  sinth(n/2:n-1) = -1 * sinth(n/2:n-1)		;fix signs
  theta = acos(costh)
  theta(n/2:n-1) = 2 * !dpi - theta(n/2:n-1)	;grow monotonically

;Compute positions and velocities.
  x = r * costh 
  y = r * sinth
  dt = t(1:n-1) - t(0:n-2)
  dx = (x(1:n-1) - x(0:n-2)) / 1d5
  dy = (y(1:n-1) - y(0:n-2)) / 1d5
  vx = dx / dt
  vy = dy / dt
  v = sqrt(vx^2.0 + vy^2.0)
  vang = atan(vy/vx)
  vang((n-1)/2:n-2) = !dpi + vang((n-1)/2:n-2)	;grow monotonically
  vobs = v * cos(vang - ang)

;Apportion mass and semi-major axes.
  m1 = m / (1 + (mr > (1/mr)))  
  m2 = m - m1
  time = (t(0:n-2)+t(1:n-1))/day
  vel1 = +vobs * m1 / m * sin(inc)
  vel2 = -vobs * m2 / m * sin(inc)

;Plot result.
  if keyword_set(plot) then begin
    ymn = min([vel1,vel2],max=ymx)
    xtit = 'Time (days)'
    ytit = 'Radial Velocity (km/s)'
    plot,time,vel1,xtit=xtit,ytit=ytit,xsty=2,yr=[ymn,ymx],/nodata
    oplot,time,vel1
    oplot,time,vel2,li=2
    oplot,!x.crange,[0,0],line=1
  endif

  max1 = max(vel1)
  max2 = max(vel2)
;  print,'Max velocities (km/s):              ', max1>max2, max1<max2
;  print,'Closest approach (solar radii):     ',min(sqrt(x*x+y*y),imin)/6.96d10
;  print,'Velocity at closest approach (km/s):',v(imin)
;  print,'Time of closet approach (days):     ',t(imin)/3600.0/24.0

end
