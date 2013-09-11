pro bv_hk,vin,xbv,h,k
;  
;  This routine returns predicted values of H and K for known values of
;  V and B-V.
;
if n_params() lt 2 then begin
  print,'Syntax is: bv_hk,V,B-V [,H ,K]'
  retall
endif

bv=[0.583000,0.608000,0.625000,0.642000,0.657000,0.672000,$
     0.690000,0.713000,     0.740000,     0.776000,     0.819000,     0.866000,$
     0.912000,0.966000,      1.03000,      1.15000,      1.31000,      1.42000,$
      1.48000,1.51000]
vk=[1.41000,1.43500,      1.46000,      1.49500,      1.53000,      1.58500,$
      1.64000,1.72000,      1.80000,      1.88000,      1.96000,      2.09000,$
      2.22000,2.42500,      2.63000,      2.85000,      3.16000,      3.65000,$
      3.87000,4.11000]
hk=[0.0500000,0.0510000,0.0520000,    0.0535000,    0.0550000,    0.0575000,$
    0.0600000,0.0637500,0.0675000,    0.0712500,    0.0750000,    0.0825000,$
    0.0900000,0.0975000,0.105000,     0.110000,     0.130000,     0.165000,$
     0.200000,0.210000]
vh=vk-hk

;From Berriman and Reid (1987, MNRAS, 227, 315).
b=[16.87,15.54,9.01,12.86,13.40,18.63,11.28,10.56,10.87,14.14,9.25]
v=[14.81,13.53,7.50,11.10,11.77,16.78,9.87,9.13,9.90,12.18,7.86]
h=[7.54,6.44,3.59,5.91,7.07,9.23,4.83,4.93,6.97,5.94,4.66]
k=[7.21,6.08,3.38,5.63,6.83,8.86,4.56,4.68,6.68,5.60,4.50]
;Keep a smooth sequence - why are the other points so far off?
ikeep=where(b-v gt 1.5 and v-k lt 7.9)
b=b(ikeep)
v=v(ikeep)
h=h(ikeep)
k=k(ikeep)
;Sort the new data.
isort=sort(b-v)
b=b(isort)
v=v(isort)
h=h(isort)
k=k(isort)
;Fit a polynomial,shift to give continuity, and use instead of data.
coef=poly_fit(b-v,v-h,2)
; x=1.5+findgen(101)/100
; plot,[bv,b-v],[vh,v-h],ps=2 & oplot,x,poly(x,coef)
vhnew=poly(b-v,coef)
vhnew=vhnew-vhnew(0)+vh(19)
vh=[vh,vhnew(1:*)]
coef=poly_fit(b-v,v-k,2)
; x=1.5+findgen(101)/100
; plot,[bv,b-v],[vk,v-k],ps=2 & oplot,x,poly(x,coef)
vknew=poly(b-v,coef)
vknew=vknew-vknew(0)+vk(19)
vk=[vk,vknew(1:*)]
bv=[bv,b(1:*)-v(1:*)]

h=vin-interpol(vh,bv,xbv)
k=vin-interpol(vk,bv,xbv)
if n_params() le 2 then begin
  print,'H = ',h
  print,'K = ',k
endif

end
