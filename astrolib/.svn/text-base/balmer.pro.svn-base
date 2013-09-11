pro balmer,w,T,Vturb,levmax,alpha,voigt=voigt
;  This routine reads in a wavelength scale w, and a
;  turbulent velocity Vturb, and a maximum level levmax, and
;  calculates the absorption cross section for the all the
;  balmer transitions through n=2 to n=levmax.  Vturb must be in
;  km/s.  Oscillator strengths now more reliable.  12/17/91 CMJ
;  Selecting voigt will use voigt functions instead of gaussians for the
;  line profile.  The temperature T must now be passed in to calc voigt.
;  2/10/92 CMJ  Voigt profile is now normalized correctly; however, the
;  Vturb used in the Voigt formalization is not the same as in the
;  Gaussian formalization.  3/05/92 CMJ.
;
npar=n_params()
if npar lt 4 then begin
   print,'Syntax is: Balmer,w,T,Vturb,lev,alpha[,voigt=voigt]'
   retall
endif
errcode=check_math(0,1)
f=[6.408e-1$
,1.193e-1 ,4.467e-2 ,2.209e-2 ,1.271e-2 ,8.037e-3 ,5.429e-3 ,3.851e-3$
,2.836e-3 ,2.150e-3 ,1.672e-3 ,1.326e-3 ,1.070e-3 ,8.770e-4 ,7.273e-4$
,6.098e-4 ,5.167e-4 ,4.418e-4 ,3.803e-4 ,3.302e-4 ,2.885e-4 ,2.534e-4$
,2.240e-4 ,1.987e-4 ,1.772e-4 ,1.587e-4 ,1.427e-4 ,1.288e-4 ,1.167e-4$
,1.060e-4 ,9.654e-5 ,8.829e-5 ,8.084e-5]
ndim=n_elements(w)
alpha=fltarr(ndim)
for lev=3,levmax do begin
   if lev le 35 then begin
      osc=f(lev-3)
   endif else begin
      osc=(2.^5/(3.*3.141592*3^(0.5)))*(2.*lev^3 /(lev^2 -4.)^3)
   endelse
   lambda=911.50236/(0.25 - (1./lev^2))
   if keyword_set(voigt) then begin
      dopplerw=sqrt(1.65e8*T + (Vturb*1.e5)^2.)*1.e8/lambda
      v=3.e10*1.e8*(1/w - 1/lambda)/dopplerw
      dampU=0.0e0
      for i=1,lev-1 do begin
        dampU=dampU+1.70399e12/lev^2*(1./i^2-1./lev^2)^2 $
                   *float(i^2*lev^3)/float(lev^2-i^2)^3
      endfor
      damp=(dampU+4.7619e8)/(4*3.14159*dopplerw)
      print,damp
      cprobf,damp,v,nvoigt
      alphao=1.49426e-2*osc/dopplerw
      alpha=alphao*nvoigt
   endif else begin
      ld=lambda*Vturb/3.e5
      alphao=1.0566e-15*lambda*osc/Vturb
      a=where(abs(w-lambda) le 4.*ld)
      if a(0) ne -1 then begin
         alpha(a)=alpha(a)+alphao*exp(-0.5*((w(a)-lambda)/ld)^2)
      endif
   endelse
endfor
errcode=check_math()
end
