;***********************************************************************
; NAME: 	RM_AMP
;																	   
; PURPOSE:    To calculate the amplitude of the RM-Effect
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
; KEYWORD PARAMETERS:												   
;		MAXRM: THE MAXIMUM AMPLITUDE THAT ONE MIGHT OBSERVE THROUGH
;       THE ROSSITER-MCLAUGHLIN EFFECT.
;
;		RPL: THE RADIUS OF THE PLANET IN R_JUP
;
;		RSTAR: THE RADIUS OF THE STAR IN R_SUN
;
;		VSINI: THE RADIAL ROTATIONAL VELOCITY OF THE STAR AT THE 
;			EQUATOR IN KM/S.
;
; OUTPUTS:															   
;																	   
; OPTIONAL OUTPUTS:													   
;																	   
; COMMON BLOCKS:													   
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
;     c. MJG, Thursday, March 5, 2009
;***********************************************************************
pro rm_amp, $
normanimation=normanimation, $
maxrm = maxrm, $
rstar=rstar, $
rpl=rpl, $
teff=teff, $
vsini=vsini

!p.color = 0
!p.background = 255

;INPUT PARAMETERS:
if ~keyword_set(teff) then Teff = 5800 ;K
if ~keyword_set(rstar) then RSTAR = 1.18 ;R_SUN
if ~keyword_set(rpl) then rpl = 1.42 ;RJUP
if ~keyword_set(vsini) then vsini = 2.46 ;km s^-1
;2.46km/s was obtained by taking the median of all of the vsini
;values in planeten that were greater than 0.

R_SUN = 6.955d8 ;m
R_JUP = 7.1492d7 ;m
;starting parameters:
numbel = 201d ;should always be odd!
sizepl = floor((rpl*r_jup)/(rstar*r_sun)*numbel)
odder= ~(floor(rpl/rstar*numbel) mod 2)
sizepl += odder
;sizepl=9
;sizepl = 101d ;should always be odd to maintain symmetry!
numel2 = 4*sizepl + numbel

;Distance from center of star on disk in stellar radii (NOT SOLAR):
;impact_209458= 0.58006969 R_STAR derived from Cody 2002.
;impact=0.58007d
impact=0d

;R_86264 = 1.88 ;in R_SUN
;M_86264pl = 7 ;M_JUP
;R_86264pl = 0.9 ;R_JUP
;R_JUP2R_SUN = 10d

;mass of a Hydrogen atom:
amu2kg = 1.66054d-27
m_a = 1.00794d * amu2kg ;kg
c = 2.99792458d8 ;m s^-1

kBoltz = 1.3806504d-23 ;JK^-1

nu_0 = c/5000d-10 ;m

;The Doppler width:
DelNuD = nu_0/c*sqrt(2*kBoltz*Teff/m_a) ; Hz
print, 'The Doppler width is: ', DelNuD
print, 'The ratio of Doppler Width/nu_0: ', DelNuD/nu_0


;The collisional frequency:
n_rho = 1.5d23
cross = 3.52d-20 ;C&O p. 240
nu_col = 1d/(n_rho * cross)* sqrt(2*kBoltz*Teff/m_a)
print, 'nu_col is: ', nu_col


;Using these and the definitions of a and u given in Rybicki & 
;Lightman p. 291 one obtains:
BigGam = 1d8 + 2*nu_col
a = BigGam/(4*!dpi*DelNuD)
print, 'nu_0 is: ', nu_0
print, 'a is: ', a

lam1 = 4997.5d-10 & lam2 = 5002.5d-10 & lam = 5d-7
nelvs = 1001
nu = dindgen(1001)/1000*(c/lam1 - c/lam2)
nu -= max(nu)/2 - nu_0
u = (nu - nu_0)/DelNuD

voigtspec = voigt(a, u)

linespec = voigtspec/(DelNuD*sqrt(!dpi))

spec = linespec
dw = reverse(c/nu * 10d10)
lcen = lam *10d10
rotspec = rotbro(dw, spec, lcen, vsini)
loadct, 39
rothaspec = rotbrohalf(dw, spec, lcen, vsini)

plot, u, linespec
oplot, u, rotspec, color = 40, linestyle = 3, thick = 2
oplot, u, rothaspec, color = 80, linestyle=2, thick=2

rotel = 0 & i=0
while rotel le 0 do begin
if (total(rotspec[0:i]) ge total(rotspec)/2d) then rotel = i
i++
endwhile

rothael = 0 & i=0
while rothael le 0 do begin
if (total(rothaspec[0:i]) ge total(rothaspec)/2d) then rothael = i
i++
endwhile

print, 'rotel is: ', rotel
print, 'rothael is: ', rothael
print, 'u[rotel] is: ', u[rotel]
print, 'u[rothael] is: ', u[rothael]

lamshift = c/(DelNuD*u[rothael] + nu_0)

DelLambda = lamshift - lam
vhalf = DelLambda/lam*c

print, 'vhalf is: ', vhalf
;stop


lambda = 5d3 ;A
v = vsini*1d3 ;m s^-1


;WE NEED TO TAKE EVEN STEPS IN SPACE, NOT ANGLE!!!!
;phi = (findgen(numbel)/(numbel-1) - 0.5d)*!dpi
;theta=(findgen(numbel)/(numbel-1))*!dpi
spacevelarr = findgen(numbel) - numbel/2d + 0.5d

;This produces the stellar rotational velocity matrix. 
;The thickness of padded zeroes on sides is 2*sizepl
;see Gray _Photospheres_ p.371
velsmat = dblarr(numel2, numel2)
for col=0, numbel-1 do begin
  velsmat[2*sizepl + col, *] = $
  v*2*spacevelarr[col]/numbel
endfor
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;NOTE: YOU CAN JUST MAKE VELSMAT AN ARRAY!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


print, 'the total of velsmat (should be zero): ', total(velsmat)

rmamp = dblarr(numel2)


window, xsize=700, ysize=700, /free

;Create a planetary circular mask
plcrcrad = double(floor(sizepl/2d))
plcircle = dblarr(sizepl, sizepl)+1
for x=0, sizepl-1 do begin
  for y=0, sizepl-1 do begin
	 if ((x-plcrcrad)^2 +$
	 (y-plcrcrad)^2) le plcrcrad^2 then plcircle[x, y] = 0d
  endfor
endfor
;contour, plcircle, /fill, nlevels=60, title='Planet Mask'
;stop

;create a circular star:
;adopted Limb Darkening using Carroll & Ostlie Eddington Approximation
;method (Chapter 09, p.266):
;I(theta) = 2/5 + 3/5*cos(theta)
;which is the same as I(theta) = 1 - eps + eps*cos(theta) from Gray Obs
;& Analysis of Photo 2nd Ed. p. 373 CH. 17
crcrad = double(floor(numbel/2d))
circle = dblarr(numel2, numel2)
for x=0, numel2-1 do begin
  for y=0, numel2-1 do begin
	 if ((x-crcrad - 2*sizepl)^2 +$
	 (y-crcrad - 2*sizepl)^2) le crcrad^2 then circle[x, y] = $
	 2/5d + 3/5d*$
	 (sqrt(crcrad^2 - (x-crcrad - 2*sizepl)^2 -(y-crcrad - 2*sizepl)^2)/crcrad)
  endfor
endfor


;window, xsize=1400, ysize=700
;filen='Documents/SFSU/research/Thesis/figures/IntensityRows'
;ps_open, filen, /encapsulated, /color
;device, xsize=48, ysize=24
;!p.multi=[0, 2, 1]
;loadct, 0
;contour, circle, /fill, nlevels=60
;LOADCT, 39  
;for i=0,4 do begin
;y=dblarr(numel2)+ i*20 + 50
;oplot, where(circle[*, i*20 + 50] gt 0), y, ps=8, color=i*20
;endfor
;y=dblarr(numel2)+ 149
;oplot, where(circle[*, 149] gt 0), y, ps=8, color=0
;
;plot, circle[*, 149], ps=8, ytitle='I!dc!n/I!dc!u0', $
;    /ysty, yran=[0,1.25]
;loadct, 39, /silent
;for i=0,4 do begin
;oplot, circle[*, i*20 + 50], ps=8, color=i*20
;endfor
;ps_close

;stop


circleones = circle gt 0
window, xsize=700, ysize=700, /free
circvmat = velsmat*circleones

plot, u, linespec

print, 'total of circular velocity matrix: ', total(circvmat)

contour, circvmat, /fill, nlevels=60
window, /free

;Normalizing the values:
halfys = fix(numel2/2) - 1d
circvmattot = double(abs(total(circvmat[0:halfys, *])))
crcvmatn = circvmat/circvmattot * double(abs(vhalf))
;dellambda = y/c*lambda
print, 'the total of a hemisphere of the circular velocity matrix', $
       ' is: ', circvmattot
print, 'minmax(crcvmatn) is: ', minmax(crcvmatn)


;the parameters for the planet:
plarr = dblarr(sizepl)+sizepl
plbase = round(numbel/2d - sizepl/2d)
plbases=dblarr(sizepl) + plbase

;the column index:
farr = dindgen(numel2)

imppix = impact*numbel/2
rowind = farr[(numel2/2. - sizepl/2.+imppix):(numel2/2. + sizepl/2. - 1+imppix)]

nvgt = 1d4
voigtcells = dblarr(nvgt)


vellarrtot = dblarr(numel2-sizepl/2)
loadct, 0
colintense = dblarr(numel2)

;**********************************************************************
; TRANSIT PLANET ACROSS
;**********************************************************************
psplot=0
if ~psplot then begin
  window, 4, xsize=575, ysize=575
  window, 5
  window,6
  window, 7
endif else begin ;psplot
 !p.multi=[0,5,3]  
  window, xsize=1900, ysize=1173
endelse
rm_amp=dblarr(numel2)
  winidx=0

;dir='~/Documents/SFSU/research/Thesis/figures/rm_amp/'
;	 filen=dir+'RM3col '+strt(i)
;	 ps_open, filen, /encap, /color
;	 device, xsize=48, ysize=30
	 
for i=sizepl/2, numel2-sizepl/2 - 1 do begin

;ERASE THE LINE FROM THE LAST PLANETARY POSITION:
voigtcells *=0d

  ;REPLACE ELEMENTS TAKEN OUT BY PLANET FROM LAST LOOP (SEE NEXT LINE):
  circtrans = circle
  ;REMOVE PLANET ELEMENTS FOR THIS LOOP:
  circtrans[(i - (sizepl-1)/2.):(i + (sizepl-1)/2.), rowind] = $
  plcircle*circle[(i - (sizepl-1)/2.):(i + (sizepl-1)/2.), rowind]
  
  if ~keyword_set(normanimation) then begin
  if ~psplot then begin
  wset, 4
  ;filen=dir+'Disk'+strt(numel2)
  ;ps_open, filen, /color, /encapsulated
  ;device, xsize=24, ysize=24
  ;PLOT THE PLANET POSITION ON DISK:
  contour, circtrans, nlevels=60, /fill, title=$
  'Position of Planet at '+strt(i)
  endif;psplot4
  endif;kw:normanimation
  ;ps_close
  ;*********************************************
  ; FOR EACH COLUMN, TOTAL THE INTENSITY
  ;*********************************************
  for col=0, numel2-1 do begin
		colintense[col] = total(circtrans[col, *])
		indeces =dindgen(n_elements(u))+circvmat[col,numel2/2d] + $
					nvgt/2d - n_elements(u)/2d
		voigtcells[indeces] += linespec * colintense[col]
  endfor;columns
  
  ;restore, '/home/mgiguere/Documents/SFSU/research/Thesis/figures/rm_amp/rm_amp.dat'
  if i eq sizepl/2 then totinitvoigt = total(voigtcells)
  ;filen=dir+'IntenseProfile'+strt(numel2)
  ;ps_open, filen, /color, /encapsulated
  ;device, xsize=24, ysize=24
  if ~keyword_set(normanimation) then begin
  if ~psplot then begin 
  wset,5
  plot, colintense, ps=8, title=$
  'Intensity Profile for Each Column with '+strt(fix(numel2))+$
  ' Elements'
  endif;psplot5
  endif;kw:normanimation
  ;ps_close
  
  ;DETERMINe WHERE THE CENTER OF THE LINE IS:
  totvoigt = total(colintense)
  
  numerator=0d
  for col=0, numel2-1 do begin
    numerator += colintense[col]*circvmat[col, numel2/2d]
  endfor;
  
  print, 'Observed Radial Velocity Shift: ', numerator/totvoigt, $
  ' index: ', i
  rm_amp[i]=numerator/totvoigt
  
  if i eq 74 then begin
  print, 'ratio of total now to total without planet: ', $
  totvoigt/totinitvoigt
;  stop
  endif
  
  if ~keyword_set(normanimation) then begin
  if ~psplot then begin
  wset, 6
  plot, rm_amp, title='Rossiter-McLaughlin Effect', $
  ytitl='Velocity (m/s)'
  endif;~psplot6
  endif;kw:normanimation

  ;filen=dir+'VoigtCells'+strt(fix(numel2))
  ;;create x title:
  vtxarr= dindgen(nvgt)-nvgt/2d
  ;ps_open, filen, /color, /encapsulated
  ;device, xsize=24, ysize=24
  
  if ~keyword_set(normanimation) then begin
  if ~psplot then begin
  wset,7
  plot, vtxarr,voigtcells, title='Voigt Line with '+$
  strt(fix(numel2))+' Elements',xtitl='Velocity'
  xyouts, 0.4, 0.18, 'Velocity: '+strt(rm_amp[i]), /norm
  endif;~psplot7
  endif;normanimation
  ;ps_close
  
  
  if ~(i mod 45) and psplot and winidx lt 5 then begin
	 !p.multi=[15-winidx,5,3]
	 if ~keyword_set(normanimation) then begin
	 contour, circtrans, nlevels=60, /fill, title=$
	 'Position of Planet on Disk'
     
	 !p.multi=[10-winidx,5,3]
	 plot, colintense, ps=8, title=$
	 'Intensity Profile for Current Planet Position'
	 
	 !p.multi=[5-winidx,5,3]
	 plot, rm_amp, title='Rossiter-McLaughlin Effect', $
	 ytitl='Velocity (m/s)'

	 oplot, [i,i],[rm_amp[i], rm_amp[i]], ps=8, symsize=2
     endif ;KW:normanimation
    ;stop
	 ;!p.multi=[0,1,1]
    winidx++
  endif;i mod 30
  ;stop
endfor ;i:sizepl/2 --> numel2 (planet transiting star)

plot, rm_amp, ps=8, xtitle='Element Index Across Disk', $
ytitle='Observed Velocity Shift [m s!u-1!n]'

;stop
maxrm = max(rm_amp)
;	 ps_close
print, 'Goodbye'
;stop
end;rm_amp.pro

