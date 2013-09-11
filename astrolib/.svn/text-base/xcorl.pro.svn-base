Pro  xcorl,star,temp,range,shft,minchi,plot=pl,print=pr
;12-Jun-92 JAV	Added minchi parameter and logic.
;17-Jun-92 JAV	Added "Max. allowable range" error message.
;24-Aug-92 JAV	Supressed output of blank line when print keyword absent.
 
if n_params() lt 4 then begin
   print,'XCORL, fixed,test,range,shft[,minchi,/plot,/print]' &
   print, 'Uses spline to find extremum'   
return
endif
;Measures the shift of temp. relative to star (a shift to rt. is +)
;Accuracy is typically 0.05pxl.   
;G. Marcy 12/88
     ln = n_elements(temp)
     ls = n_elements(star)
     len = min([ln,ls])
     if range gt (len-1)/2 then $
	message,'Maximum allowable "range" for this case is' $
	  + strcompress((len-1)/2)
     newln = len - 2*range    ; Leave "RANGE" on ends for overhang.
     te = temp/(total(temp)/ln)
     st = star/(total(star)/ls); Be normal already!
     newend = range + newln - 1
     x =findgen(2 * range+1) - range
     chi = fltarr(2 * range+1)
     for j = -range,range do begin     ; Goose step, baby.
        dif = te(range:newend) - st(range+j:newend+j)
        chi(j+range) = total(dif^2)  ;Too bad sdev. doesn't work.
     endfor
     xcr = chi
     if keyword_set(pl) then begin
	if pl eq 2 then !p.multi=[0,1,2]
	ymin = min(star,max=ym1) < min(temp,max=ym2)	;min value to plot
	ymax = ym1 > ym2				;max value to plot
	xmax = n_elements(star)>n_elements(temp)
	print,'the x range is:',xmax
	print,'the y range is:',ymin,ymax
	plot,star,tit='Fixed spectrum in solid white.', $
		xr = [0.,xmax],yr=[ymin,ymax]
	oplot,temp +.5,co=3,lines=5
        if pl eq 2 then begin
	   plot, chi,/ynozero
	   !p.multi=0
	endif
     endif
     len = n_elements(x) * 100
     xl = findgen(len)
     xl = xl/100. - range
     xp = xl(0:len-100)
     cp = fspline(x,chi,xp)
     minchi = min(cp,mm)
;    mm = where(cp eq min(cp))		;replaced by preceding line
     shft = xp(mm(0))
;    	print, 'spline', peak
;    	q = where(min(chi)) & peak=q(0) & x=x(peak-2:peak+2) 
;    	chi=chi(peak-2:peak+2)
;    	coef = poly_fit(x,chi,2) & shft = -0.5 * coef(1)/coef(2)
;    	print,'parabola', shft
	if keyword_set(pr) then print
        if keyword_set(pl) then print,'The shift of the dashed (blue) spectrum is: ',shft 
	if keyword_set(pr) then print,'XCORL: The shift is: ',strtrim(string(shft),2)
end

