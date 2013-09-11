pro movie

w1a=5164.00  &  w2a=5190.
w1b=6002.84  &  w2b=6030.00
w1c=6030.00  &  w2c=6050.00
w1d=6050.00  &  w2d=6070.00
w1e=6100.00  &  w2e=6118.00
w1f=6121.00  &  w2f=6140.00
w1g=6143.00  &  w2g=6159.00
w1h=6160.00  &  w2h=6180.00

starlist=findfile('hd*.out',count=nfiles)

check={outfile:'?', checka:0, checkb:0, checkc:0, $
	checkd:0, checke:0, checkf:0, checkg:0, checkh:0}
check=replicate(check,nfiles)

loadct,39
!p.background=255   ;white
!p.charsize=1.7
!x.charsize=1.3
!y.charsize=1.4
!p.charthick=2
!p.font=-1



ans=''  & ansz=''
read,'Sort by stars? (y/n) ',ans
start=0
if ans eq 'y' then begin 
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1a and sme.wave lt w2a)
	tag='- First Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag,/nodata
	x1=where(sme.wave gt w1a and sme.wave lt w2a and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
;	x2=where(sme.wave gt w1a and sme.wave lt w2a and sme.mob eq 2)
	wait,2
	check(i).checka=1

	x=where(sme.wave gt w1b and sme.wave lt w2b)
	tag='- Second Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag,/nodata
	x1=where(sme.wave gt w1b and sme.wave lt w2b and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkb=1
 
	x=where(sme.wave gt w1c and sme.wave lt w2c)
	tag='- Third Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag,/nodata
	x1=where(sme.wave gt w1c and sme.wave lt w2c and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkc=1

	x=where(sme.wave gt w1d and sme.wave lt w2d)
	tag='- Fourth Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag,/nodata
	x1=where(sme.wave gt w1d and sme.wave lt w2d and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkd=1

	x=where(sme.wave gt w1e and sme.wave lt w2e)
	tag='- Fifth Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1e and sme.wave lt w2e and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checke=1

	x=where(sme.wave gt w1f and sme.wave lt w2f)
	tag='- Sixth Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1f and sme.wave lt w2f and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkf=1

	x=where(sme.wave gt w1g and sme.wave lt w2g)
	tag='- Seventh Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1g and sme.wave lt w2g and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkg=1

	x=where(sme.wave gt w1h and sme.wave lt w2h)
	tag='- Eighth Segment'
	plot,sme.wave(x),sme.sob(x),col=0,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1h and sme.wave lt w2h and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0,1],col=190
        oplot,sme.wave(x),sme.sob(x),col=220
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,2
	check(i).checkh=1

	save,check,file='check_spec.dat'
end
end 

if ans eq 'n' then begin
ans2=''  &  waittime=1.   & ans3=0  & start_star=0L
resid1=fltarr(nfiles,1000)
print,'sorting by wavelength segment'

read,'which wavelength segment would you like?(1-8) ',ans2
read,'What wait time do you prefer (in seconds)? ',waittime
start=0
if ans2 eq '1' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1a and sme.wave lt w2a,nx)
	tag='- First Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1a and sme.wave lt w2a and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
        ncts=n_elements(sme.chisq)
;        if sme.chisq(ncts-1) lt 10 then begin
            for j=0,nx-1 do resid1(i,j)=sme.smod(x(j))-sme.sob(x(j))
;        endif
	check(i).checka=1
    endfor
endif
if ans2 eq '2' then begin
resid2=fltarr(nfiles,1000)
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1b and sme.wave lt w2b,nx)
	tag='- Second Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1b and sme.wave lt w2b and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
;        ncts=n_elements(sme.chisq)
;        if sme.chisq(ncts-1) lt 10 then begin
            for j=0,nx-1 do resid2(i,j)=sme.smod(x(j))-sme.sob(x(j))
;        endif
	check(i).checkb=1
    endfor
endif
if ans2 eq '3' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1c and sme.wave lt w2c)
	tag='- Third Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1c and sme.wave lt w2c and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checkc=1
endfor
endif
if ans2 eq '4' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1d and sme.wave lt w2d)
	tag='- Fourth Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1d and sme.wave lt w2d and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checkd=1
    endfor
endif
if ans2 eq '5' then begin
for i=0,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1e and sme.wave lt w2e)
	tag='- Fifth Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1e and sme.wave lt w2e and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checke=1
    endfor
endif
if ans2 eq '6' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1f and sme.wave lt w2f)
	tag='- Sixth Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1f and sme.wave lt w2f and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checkf=1
    endfor
endif
if ans2 eq '7' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1g and sme.wave lt w2g)
	tag='- Seventh Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1g and sme.wave lt w2g and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checkg=1
    endfor
endif
if ans2 eq '8' then begin
for i=start,nfiles-1 do begin
	restore,starlist(i)
	x=where(sme.wave gt w1h and sme.wave lt w2h)
	tag='- Eighth Segment'
	plot,sme.wave(x),sme.sob(x),col=220,$
		yra=[0,1.1],/ysty,/xsty,$
		titl=starlist(i)+' '+tag
	x1=where(sme.wave gt w1h and sme.wave lt w2h and sme.mob eq 1,nx1)
	for j=0,nx1-1 do plots,[sme.wave(x1(j)),sme.wave(x1(j))],[0.05,1.],col=190
	oplot,sme.wave(x),sme.smod(x),col=60
        oplot,sme.wave(x),(sme.smod(x)-sme.sob(x))+0.2,col=120
	wait,waittime
	check(i).checkh=1
endfor
endif
endif

stop
end
