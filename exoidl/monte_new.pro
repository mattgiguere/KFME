function monte_new,inpcf,orbpar, Ntrial=Ntrial,star=star,mst=mstar, $
debug = debug, frz_ecc = frz_ecc, frz_per = frz_per, frz_k = frz_k, $
circle = circle

; subtract best fit theoretical velocities from the data
; add noise to the residuals 
; add the theoretical velocities + (residuals + noise)
; re-run rv_fit

if not keyword_set(mst) then mst=1.0


if star eq '179079' then begin
;   mst = 1.087
;   restore,'/mir3/vel/vst179079.dat'
;   jitter=3.5
;   cf3.errvel=sqrt(cf3.errvel^2 + jitter^2)
;   inpcf=cf3
;   orbpar=[14.47, 13199.4, 0.1, 11.2, 6.5, 0.0]
endif


if not keyword_set(Ntrial) then Ntrial=100

errcut = 5

;takes care of high jitter and the first detections:
if (max(inpcf.mnvel) - min(inpcf.mnvel) gt 100) then errcut = 15
if star eq '86264' then errcut = 35

badval = where(inpcf.errvel gt errcut, ct)

if ct eq n_elements(inpcf) then stop
if ct gt 0 then goodcf = rm_elements(inpcf, badval) else goodcf = inpcf




t=goodcf.jd
obvel=goodcf.mnvel
sig=goodcf.errvel
npts=n_elements(t)
  dpar = 0.01d0 + orbpar*0.
  dpar(0) = 0.001d0*orbpar(0)
  dpar(1) = 0.01d0*orbpar(0)    ;d Tp
  dpar(2) = 0.001d0        	;d ecc
  dpar(3) = 10.d0          	;d omega
  dpar(4) = 0.02d0*orbpar(4)    ;d K (m/s)
  dpar(5) = 1.d0          	;d gamma ...step is m/s
;  dpar(6) = 1.d0          	;d gamma ...step is m/s

if orbpar[1] gt 2.44d6 then orbpar[1] -= 2.44d6
if orbpar[1] gt 4d4 then orbpar[1] -= 4d4

if keyword_set(debug) then stop

;thvel1=rv_marq('rv_drive_tr',t,obvel,sig,orbpar,dpar)  ;best fit    thvel = rv_drive_tr(tfine,phpar)theoretical velocities
;thvel1=rv_marq('rv_drive_tr',t,obvel,sig,orbpar,dpar)  ;best fit theoretical velocities

testvel = goodcf.mnvel
newmaxvel = 0.5*(max(testvel)-min(testvel))
dvel = newmaxvel - max(testvel)
testvel += dvel


thvel1 = rv_drive_tr(t,orbpar)
resid = goodcf.mnvel-thvel1
resid2 = testvel-thvel1
resid = resid2

print, 'this version was edited 2010.01.11'

dum = where(orbpar gt 0., nfreepar)
rms = sqrt(total(resid^2)/(n_elements(goodcf) - nfreepar))
print, 'rms is: ', rms
rms2 = sqrt(total(resid2^2)/(n_elements(goodcf) - nfreepar))
print, 'rms2 is: ', rms2
plot, goodcf.jd, resid, ps=8, title='Best Fit Residuals'


err=goodcf.errvel 
new_vel=fltarr(npts,2*Ntrial)
stop

if keyword_set(debug) then stop
for i=0,2*Ntrial-1 do begin
      ;repl = randoma(min=0, max=npts-1, num=npts)
     repl=long(randomu(seed,npts)*n_elements(t)) ;index for replacement
        new_vel[*,i] = resid(repl) + thvel1
        ;print, 'scrambling residuals: ', i+1, ' of ', ntrial
     ;stop
end ;for  

outarr=fltarr(9,Ntrial)
j=0
stop
for idxmte=0,Ntrial-1 do begin

	print, '*********************************************************'
	print, 'Now on run:', j, ' of ', strt(ntrial), ' in MONTE_NEW.PRO'
	print, '*********************************************************'
    goodcf.mnvel=new_vel(*,j)
    rv_fit_old,goodcf,per=orbpar(0),parout=dum,$
    mstar=mst,init_tp=max(goodcf.jd),chi=chi, $
    frz_per = frz_per, frz_k = frz_k, frz_ecc = frz_ecc, $
    circle = circle, /phase, title = star

	;outarr[*, j] = dum
	thsh=0.5d ;threshold
    print, 'CHI SQUARED IS: ', chi
	;if (chi gt 1.4) then outarr[1,j] = !Values.F_NAN
	;BELOW IS THE REAL LOGIC:
	;logic = ((dum[0] lt 11.8 - thsh) OR (dum[0] gt 11.8+thsh) OR $
	;(dum[2] gt 0.7d))
	logic=0
	;if dum[4] gt 50d then stop
	
	;THIS IS JUST A TEST LOGIC:
	;logic = ((dum[0] lt 11.8 - thsh) OR (dum[0] gt 11.8+thsh))
	if ~(logic) then begin
		;if dum[4] gt 10d then stop
		;stop
		outarr[*, idxmte] = dum	
		idxmte++
	endif
	idxmte--
	j++
	;if ((chi lt 1.2) and (dum[0] lt 10) and (dum[2] lt 0.6)) then stop
	;if (j mod 100) eq 0 then stop
end

print, 'Tp Nan?', where(~finite(outarr[1, *]))
nans = where(~finite(outarr[1, *]), nnans)
notnans = where(finite(outarr[1, *]))


newoutarr=fltarr(9, (n_elements(outarr[1, *]) - nnans))
for i=0,8 do begin
    newoutarr[i,*]=outarr[i,notnans]
end

print, 'Nan?', where(~finite(newoutarr))

medianper=strcompress(string(median(newoutarr(0,*))),/remove_all)
pererr=strcompress(string(stdev(newoutarr(0,*))),/remove_all)
mediantp=strcompress(string(median(newoutarr(1,*))),/remove_all)
tperr=strcompress(string(stdev(newoutarr(1,*))),/remove_all)
medianecc=strcompress(string(median(newoutarr(2,*))),/remove_all)
eccerr=strcompress(string(stdev(newoutarr(2,*))),/remove_all)
medianom=strcompress(string(median(newoutarr(3,*))),/remove_all)
omerr=strcompress(string(stdev(newoutarr(3,*))),/remove_all)
mediank=strcompress(string(median(newoutarr(4,*))),/remove_all)
kerr=strcompress(string(stdev(newoutarr(4,*))),/remove_all)

print,'Per: '+medianper+' +/- '+pererr
print,'Tp: '+mediantp+' +/- '+tperr
print,'Ecc: '+medianecc+' +/- '+eccerr
print,'Om: '+medianom+' +/- '+omerr
print,'K: '+mediank+' +/- '+kerr
print,'Mean msini: ',median(newoutarr(5,*))
print,'Mean arel: ',median(newoutarr(6,*))
print,'Mean RMS: ',median(newoutarr(7,*))
print,'Mean chisq: ',median(newoutarr(8,*))

;x=where(newoutarr(8,*) lt 3.2,nx)
;per=outarr(0,x)
;tp=outarr(1,x)
;ecc=outarr(2,x)
;om=outarr(3,x)
;k=outarr(4,x)
plothist, newoutarr[4,*], bin=0.1, title='Distribution of Amplitudes'
print, minmax(newoutarr[4,*])

stop
return, newoutarr
end
