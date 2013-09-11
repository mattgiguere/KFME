pro hminus,T,nee,w,alpha
; This routine reads in a temperature, T, an electron number density, nee,
; a wavelength scale, w (in Angstroms), and calculates
; the total H- cross section (free-free plus bound-free) at that temp. and
; wavelength and returns the result in alpha. The result uses the equations
; given in John, T.L. 1988. Astron & Astrophys. 193. 189.  12/10/91  CMJ
npar=n_params()
if npar lt 3 then begin
   print,'Syntax is: hminus,T,nee,w,alpha'
   retall
endif
theta=5040./T
nw=w*1.e-4
sum=fltarr(n_elements(w))
a=where(nw lt 0.1823)
;if a(0) ne -1 then begin
;   print,'Warning: Wavelengths less than 1823 A have no known free-free'
;   print,'Contribution.  Free-free cross section set to zero there'
;   sum(a)=0.0
;endif
b=where(nw ge 0.1823 and nw lt 0.3645)
if b(0) ne -1 then begin
   sum(b)=theta*(nw(b)^2*518.1021-734.8666+1021.1775/nw(b)  $
          -479.0721/nw(b)^2 +93.1373/nw(b)^3-6.4285/nw(b)^4)
   sum(b)=sum(b)+theta^(1.5)*(nw(b)^2*473.2636+1443.4137-1977.3395/nw(b) $
          +922.3575/nw(b)^2-178.9275/nw(b)^3+12.36/nw(b)^4)
   sum(b)=sum(b)+theta^2*(nw(b)^2*(-482.2089)-737.1616+1096.8827/nw(b)  $
          -521.1341/nw(b)^2+101.7963/nw(b)^3-7.0571/nw(b)^4)
   sum(b)=sum(b)+theta^(2.5)*(nw(b)^2*115.5291+169.6374-245.649/nw(b)  $
          +114.243/nw(b)^2-21.9972/nw(b)^3+1.5097/nw(b)^4)
endif
c=where(nw ge 0.3645)
if c(0) ne -1 then begin
   sum(c)=theta^(1.5)*(nw(c)^2*2483.346+285.827-2054.291/nw(c)  $
          +2827.776/nw(c)^2-1341.537/nw(c)^3+208.952/nw(c)^4)
   sum(c)=sum(c)+theta^2*(nw(c)^2*(-3449.889)-1158.3820+8746.523/nw(c)  $
          -11485.632/nw(c)^2+5303.609/nw(c)^3-812.939/nw(c)^4)
   sum(c)=sum(c)+theta^(2.5)*(nw(c)^2*2200.04+2427.719-13651.105/nw(c)  $
          +16755.524/nw(c)^2-7510.494/nw(c)^3+1132.738/nw(c)^4)
   sum(c)=sum(c)+theta^(3)*(nw(c)^2*(-696.271)-1841.4+8624.97/nw(c)  $
          -10051.53/nw(c)^2+4400.067/nw(c)^3-655.02/nw(c)^4)
   sum(c)=sum(c)+theta^(3.5)*(nw(c)^2*88.283+444.517-1863.864/nw(c)  $
          +2095.288/nw(c)^2-901.788/nw(c)^3+132.985/nw(c)^4)
endif
alphaff=1.e-29*sum*nee*1.38e-16*T
a=where(w le 16419.)
b=where(w gt 16419.)
flam=fltarr(n_elements(w))
if a(0) ne -1 then begin
flam(a)=152.519+49.534*(1./nw(a)-1./1.6419)^(0.5)-118.858*(1./nw(a)-1./1.6419)
flam(a)=flam(a)+92.536*(1./nw(a)-1./1.6419)^(1.5)-34.194*(1./nw(a)-1./1.6419)^2
flam(a)=flam(a)+4.982*(1./nw(a)-1./1.6419)^(2.5)
endif
alphabf=fltarr(n_elements(w))
if b(0) ne -1 then begin
   flam(b)=0.0
   alphabf(b)=0.0 
endif
alphabf(a)=1.e-18*nw(a)^3*(1./nw(a)-1./1.6419)^(1.5)*flam(a)*nee*T*1.38e-16
alpha=alphabf+alphaff
end
