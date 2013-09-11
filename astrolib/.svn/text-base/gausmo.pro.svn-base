pro   gausmo, w,s,dw,sm,out=ot,plot=pl

if n_params() lt 4 then begin
	print,'GAUSMO, w,s,dw(A),sm,out=ot,plot=pl' 
return
endif
;GAUSSIAN SMOOTHING with fwhm DW(angstroms)
;spectrum,S and wavel.,W (length np),smoothed spec. in SM
;DW is half-width at half max in angstroms
;PLOT:  1 on, 0 off
;new convolution version Basri/Marcy 12/90
;fixed test for constant dispersion to be more general 5/4/93 GB,JV

	npd = n_elements(w)
	dc = 1. /(w(npd/2)-w(npd/2-1))	;1/(dispersion constant)
	dca = 1./(w(1)-w(0))
	if abs(1-dca/dc) gt 0.01 then begin   ;cautionary check that w is evenly spaced
		print,'Should use constant dispersion spectrum, w'
;		return
	endif
	nw = dc*dw  &  wid = nw*nw*1.442695   ;profile parameters
	nwid = fix(nw*4)	;enough pixels to bring Gaussian near 0
;pad out ends
	sec1 = fltarr(nwid+2)	;zero most of full vector
	sec2 = sec1
	sec1 = sec1 + s(0) 
	sec2 = sec2 + s(npd-1)
	sm = [sec1,s,sec2]
	np = n_elements(sm)
;**make GAUSSIAN PROFILE
	pr = fltarr(nwid)
	pr(0) = 1.   &   sum = 1.
	for n=1,nwid-1 do begin
		pr(n) =exp(-n*n/wid)
		sum = sum + pr(n) * 2
	endfor
;normalize and make flip side
	pr = pr/sum
	cen = pr(0)   &   prf = pr(1:nwid-1)
	prb = reverse(prf)	;call reverse(intrinsic)
;**do CONVOLUTION
	gpr = [prb,cen,prf]		;produce full profile
	sms = convol(sm, gpr)
	if keyword_set(ot) then print,'Subscripts of min(gpr):',where(gpr eq min(gpr))
	sm = sms(nwid+2:np-nwid-3)	;back to original length
	if keyword_set(pl) then begin
		plot,w,s
		oplot,w,sm-.5
	endif
end
