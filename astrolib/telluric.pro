pro telluric

;check telluric contamination in each of the 
;SME wavelength segments

; these wavelength segments are free of 
; telluric contamination:
;	4770.   4790. currently used
;	5290.   5310. currently used
;   	5370.	5390. currently used
;	5600.   5620.   added
;	5620.	5640. currently used
;       5640.   5660.
;	6030.	6050. currently used
;	6050.   6070. currently used
;	6070.	6090.
;	6100.   6120. currently used
;	6120.   6140. currently used
;	6140.	6180.
;	6700.	6720.  (lithium region)


; contaminated regions
;	5040.	5056. currently used
; 	5660.	5680. currently used
; stay away from 5800 - 6000

w0=[6080.,6180.,6200.]
w1=[6100.,6200.,6300.]

!p.multi=[0,1,4]
ps_open,'telluric',/portrait
npl=n_elements(w0)
for i=0,npl-1 do begin
  rdtel,w,s,w0(i),w1(i)
  plot,w,s,yra=[0.9,1.02],ysty=1,xra=[w0(i),w1(i)],xsty=1
endfor

ps_close
end