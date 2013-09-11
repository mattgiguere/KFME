pro rotint,w,s,vsini,ns,eps,nr,ntheta,dif
;  This routine reads in a spectrum, s, on a wavelength scale, w, and a vsini
;  with which to rotationally broaden the spectrum.  The rotationally broadened
;  spectrum is returned in ns.  Parameters that can be set are the coefficient
;  of the limb darkening law, eps (0.6 default), the number of radial steps on
;  the disk, nr (default = 10), and the maximum number of steps in angle around
;  the disk, ntheta (default = 100).  Final optional parameter dif allows for 
;  differential rotation according to the law Omeg(th)/Omeg(eq) = (1. - dif/2
;  - (dif/2) cos(2 th)), where th is the stellar co-latitude.  Dif = .675 
;  nicely reproduces the law proposed by;  Smith, 1994, A&A, in press. 
;  to unify WTTS and CTTS.  Dif = .23 is similar to;  observed solar 
;  differential rotation.  This is a disk integration routine.
;
;  Only valid for inclination of 90 degrees (equator on).
;
;  11-May-1994: Written CMJ.
;
if n_params() lt 4 then begin
   print,'Syntax is: rotint,w,s,vsini,ns,[eps,nr,ntheta,dif]'
   retall
endif
if n_params() lt 5 then eps=0.6
if n_params() lt 6 then nr=10
if n_params() lt 7 then ntheta=100
if n_params() lt 8 then dif=0

ns=fltarr(n_elements(s))
tarea=0.0

dr=1./nr
for j=0,nr-1 do begin
   r=dr/2.+j*dr
   area=((r+dr/2.)^2-(r-dr/2.)^2)/fix(ntheta*r)*(1.-eps+eps*cos(asin(r)))
   for k=0,fix(ntheta*r)-1 do begin
      th=!pi/fix(ntheta*r)+k*2.*!pi/fix(ntheta*r)
      if dif ne 0 then begin
         vl=vsini*r*sin(th)*(1.-dif/2.-dif/2.*cos(2.*acos(r*cos(th))))
;Equivalent to vl=vsini*r*sin(th)*(1.-dif*r^2*cos(th)^2)
         ns=ns+area*fspline(w+w*vl/3.e5,s,w)
;         ns=ns+area*interpol(s,w+w*vl/3.e5,w) 
         tarea=tarea+area
      endif else begin
         vl=r*vsini*sin(th)
         ns=ns+area*fspline(w+w*vl/3.e5,s,w)
;         ns=ns+area*interpol(s,w+w*vl/3.e5,w) 
         tarea=tarea+area
      endelse
   endfor
endfor
ns=ns/tarea

end
