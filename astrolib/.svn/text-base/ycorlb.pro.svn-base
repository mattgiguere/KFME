pro  ycorlb, star,temp,shft,plot=plot
 
; cross correlates in the y direction to get the 
; best offset to match continuum.
;
; assumes that xcorlb has already run to match up spectra
; in wavelength.

;	input	star	star, with central line cut out
;		temp	template, with central line cut out

;	output	shft	the vertical shift to be added to star

; Fischer 9-17-99

if n_params() lt 3 then begin
   print,'YCORLB, fixed,test,shft[,plot=plot]' &
   print, 'Uses spline to find extremum'   
return
endif
     
;plot,star
nsteps=40   
delta=fltarr(2*nsteps+1)
chi = fltarr(2*nsteps+1)
for j = -nsteps,nsteps do begin  
   del=1.00+(j/(nsteps*30.))
   delta(j+nsteps)=del
   shftstar = star*del
   dif = (temp - shftstar)^2.
   chi(j+nsteps) = stdev(dif)
endfor

bestshift = where(chi eq min(chi))
shft=delta(bestshift)
if keyword_set(plot) then begin
;   plot,temp
    plot,star+shft
endif
     
print, 'best continuum shift: ',shft
end

