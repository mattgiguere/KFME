function redden,Av,w,sin,rv=rv
;Applies extinction correction to (w,sin).
; Av (input scalar) visual extinction (in magnitudes) to apply to sin.
;   May be negative (dereddening) or positive (reddening).
; w (input vector) wavelength scale (in Angstroms).
; sin (input vector) spectrum to be (de)reddened.
; sout  (de)reddened spectrum.
; Rv = Av / E(B-V) (default is 3.1 for diffuse ISM; 5? for dense clouds)
;based on Cardelli,Clayton,Mathis ApJ 345, 245 (1989)

if n_params() lt 3 then begin
  message,/info,'sout=redden(Av(pos/neg=red/dered),w(A),spec,[rv=rv])'
  retall
endif
if not keyword_set(rv) then rv=3.1		;standard ISM value
;Convert to inverse microns and choose spectral region
x=10000./w					;angstroms to inverse microns
ir=where(x ge 0.3 and x le 1.1,nir)		;define IR
opt=where(x gt 1.1 and x le 3.3,nopt)		;define optical
uv=where(x gt 3.3 and x le 8.,nuv)		;define UV
fuv=where(x gt 8. and x le 10.,nfuv)		;define far-UV
bad=where(x lt 0.3 or x gt 10.,nbad)		;bad points (outside range)
if nbad gt 0 then print,'there are points outside the range',nbad
a=w*0. & b=w*0.

if nir gt 0 then begin				;do IR
a(ir)=0.574*x(ir)^1.61
b(ir)=-0.527*x(ir)^1.61
endif
if nopt gt 0 then begin				;do optical
y=x(opt)-1.82
a(opt)=1.+0.17699*y-0.50447*y^2-0.02427*y^3+0.72085*y^4+0.01979*y^5 $
		-0.77530*y^6+0.32999*y^7
b(opt)=1.41338*y+2.28305*y^2+1.07233*y^3-5.38434*y^4-0.62251*y^5     $
		+5.30260*y^6-2.09002*y^7
endif

if nuv gt 0 then begin				;do UV
  xx=x(uv) & fa=-0.04473*(xx-5.9)^2-0.009779*(xx-5.9)^3
  fb=0.2130*(xx-5.9)^2+0.1207*(xx-5.9)^3
  xz=where(xx lt 5.9,nxz)
  if nxz gt 0 then begin
    fa(xz)=0.  &  fb(xz)=0.
  endif
  a(uv)=fa+1.752-0.316*xx-0.104/((xx-4.67)^2+0.341)
  b(uv)=fb-3.090+1.825*xx+1.206/((xx-4.62)^2+0.263)
endif

if nfuv gt 0 then begin				;do far-UV
  xx=x(fuv)-8.
  a(fuv)=-1.073-0.628*xx+0.137*xx^2-0.07*xx^3
  b(fuv)=13.670+4.257*xx-0.420*xx^2+0.374*xx^3
endif

extinct=a+b/rv
;Now return extinguished spectrum.
  sout = sin * 10.0 ^ (-0.4*Av*extinct)		;extinguish spectrum
return,sout
end
