function monte_lin,inpcf,orbpar, Ntrial=Ntrial,star=star,mst=mstar, $
debug = debug, frz_ecc = frz_ecc, frz_per = frz_per, frz_k = frz_k, $
circle = circle, nplanets = nplanets, trend=trend, fixed=fixed, $
frz_om=frz_om

; subtract best fit theoretical velocities from the data
; add noise to the residuals 
; add the theoretical velocities + (residuals + noise)
; re-run rv_fit

if not keyword_set(mst) then mst=1.0


if star eq 179079 then begin
;   mst = 1.087
;   restore,'/mir3/vel/vst179079.dat'
;   jitter=3.5
;   cf3.errvel=sqrt(cf3.errvel^2 + jitter^2)
;   inpcf=cf3
;   orbpar=[14.47, 13199.4, 0.1, 11.2, 6.5, 0.0]
endif


if not keyword_set(Ntrial) then Ntrial=100

errcut = 50

;takes care of high jitter and the first detections:
;if (max(inpcf.mnvel) - min(inpcf.mnvel) gt 100) then errcut = 15
if star eq '86264' then errcut = 35
if star eq '3651' then errcut = 5


badval = where(inpcf.errvel gt errcut, ct)

if ct gt n_elements(inpcf)-5 then stop
if ct gt 0 then goodcf = rm_elements(inpcf, badval) else goodcf = inpcf
;goodcf=inpcf



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

;thvel1=rv_marq('rv_drive_tr',t,obvel,sig,orbpar,dpar)  ;best fit    thvel = rv_drive_tr(tfine,phpar)theoretical velocities
;thvel1=rv_marq('rv_drive_tr',t,obvel,sig,orbpar,dpar)  ;best fit theoretical velocities

testvel = goodcf.mnvel
;newmaxvel = 0.5*(max(testvel)-min(testvel))
;dvel = newmaxvel - max(testvel)
;testvel += dvel


;thvel1 = rv_drive_tr(t,orbpar)
thvel1 = rv_drive_mp(t,orbpar)
resid = goodcf.mnvel-thvel1
resid2 = testvel-thvel1
resid = resid2
;stop
;print, 'this version was edited 2009.02.13'

dum = where(orbpar gt 0., nfreepar)
rms = sqrt(total(resid^2)/(n_elements(goodcf) - nfreepar))
print, 'rms is: ', rms
rms2 = sqrt(total(resid2^2)/(n_elements(goodcf) - nfreepar))
print, 'rms2 is: ', rms2
plot, resid, ps=8
;stop

err=goodcf.errvel 
new_vel=fltarr(npts,Ntrial)


if keyword_set(debug) then stop
for i=0,Ntrial-1 do begin
      ;repl = randoma(min=0, max=npts-1, num=npts)
     repl=long(randomu(seed,npts)*n_elements(t)) ;index for replacement
        new_vel[*,i] = resid(repl) + thvel1
        ;print, 'scrambling residuals: ', i+1, ' of ', ntrial
     ;stop
end ;for  

outarr=dblarr(7*nplanets,Ntrial)
chiarr = dblarr(ntrial)
rmsarr = dblarr(Ntrial)

nplanets=n_elements(orbpar)/7
for j=0,Ntrial-1 do begin
    goodcf.mnvel=new_vel[*,j]
;    rv_fit,goodcf,per=orbpar(0),parout=dum,$
;    mstar=mst,init_tp=max(goodcf.jd),chi=chi, $
;    frz_per = frz_per, frz_k = frz_k, frz_ecc = frz_ecc, $
;    circle = circle, /phase, title = star

    outarr[*, j]=rv_fit_mp(goodcf.jd,goodcf.mnvel,goodcf.errvel, $
									nplanets=nplanets,tps=max(goodcf.jd),$
									chi=chi,rms=rms, orbel=orbpar, /plotfit)
                     
   chiarr[j] = chi
   rmsarr[j] = rms
;stop
;print, 'The old pars:'
;print, [transpose(findgen(9)), transpose(dum)]
;print, 'The new pars: '
;print, [transpose(findgen(5*nplanets+2)), transpose(outarr[*, j])]
;stop
  print, 'Now on run:', j, ' of ', ntrial, ' in MONTE_LIN.PRO'
end

print, 'Tp Nan?', where(~finite(outarr[1, *]))
nans = where(~finite(outarr[1, *]), nnans)
notnans = where(finite(outarr[1, *]))


newoutarr=fltarr(7*nplanets, (n_elements(outarr[1, *]) - nnans))
for i=0,7*nplanets-1 do begin
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
print, 'gamma: ', median(newoutarr(5*nplanets,*))
print, 'dvdt: ', median(newoutarr(5*nplanets+1,*))
;print,'Mean msini: ',median(newoutarr(5,*))
;print,'Mean arel: ',median(newoutarr(6,*))
;print,'Mean RMS: ',median(newoutarr(7,*))
;print,'Mean chisq: ',median(newoutarr(8,*))

;x=where(newoutarr(8,*) lt 3.2,nx)
;per=outarr(0,x)
;tp=outarr(1,x)
;ecc=outarr(2,x)
;om=outarr(3,x)
;k=outarr(4,x)

linstruct = create_struct('outarr', newoutarr, 'chiarr', chiarr, 'rmsarr', rmsarr)

return, linstruct
end
