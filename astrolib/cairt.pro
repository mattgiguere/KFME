pro cairt, templ,dewar, fn,sig, S8662, err8662,plot=plot

; a program to determine the chromospheric S_IR values 
; from the Ca IR triplet lines in Lick spectra.
; 
; either a template spectrum or a high s/n 3m spectrum is the 
; taken as the S_IR template.  The spectrum is shifted to rest 
; wavelength with NSO and all other observations are then shifted 
; to the S_IR template.  The sdev of points in the continuum window 
; are used to set the errval for each S_IR. 

; Fischer   9-14-99

; INPUT
;	obnm	string	File of Lick spectrum, e.g. 'rb92.20'
;			No directory path needed. ( uses rdsi.pro)

; OUTPUT
;	sval	fltarr	S8498, S8542, S8662
;     errval	fltarr	errors in S8498, S8542, S8662 (from continuum)

; KEYWORDS
;	plot	makes plot of 3 CaIR triplet lines


;EXAMPLE OF USE:
;                 IDL> cairt,'rb92.20',18,'rb97.25',sval,/plot
;                 IDL> print,sval
;                      0.222917


;Read the reduced Lick spectrum

sval=0.0
errval=0.0
plot='yes'
count=1
  rdsi,ima, templ
  rdsk,wav,'/further/paul/idle/ham_wavarr.dsk'
  rdsi,imb,fn

  ;window the 8662 line, normalize, shift to nso, set continuum windows
  rdnso,w,s,8630,8695

  ;approximate locations of CaII lines, in pixel space 
  ;first, shift the S_IR template w.r.t. the NSO
  snip3a=ima(780:1250,6)        ; snip2=im(*,6) ; snip=[snip1,snip2]
;  snip3a=median(snip3a,7)       ; median smoothing
  snip3a=smooth(smooth(snip3a,3),3)
  wav1=wav(780:1250,6)          ; wav2=wav(*,6)
  junka=median(snip3a,15)
  contf,junka,cona,sbin=30,nord=3
;  contf,snip3a,cona,sbin=30,nord=3
  snip3a=snip3a/cona
  nsospec=spline(w,s,wav1)      ;spline the nso onto the wave-scale of im
  range=80
  xcorlb,nsospec,snip3a,range,shft 
  shfour,snip3a,shft,newsnip3a  ;shift it in fourier space to rest wavelength

  ;now, shift the obs of interest w.r.t. the S_IR template
  snip3b=imb(780:1250,6)
;  snip3b=median(snip3b,5)       ; median smooth 
  snip3b=smooth(snip3b,3)
  junkb=median(snip3b,15)
  contf,junkb,conb,sbin=30,nord=3
;  contf,snip3b,conb,sbin=30,nord=3
  snip3b=snip3b/conb
  xcorlb,newsnip3a,snip3b,range,smshft
  shfour,snip3b,smshft,newsnip3b

  ; find the 8662 line center for measuring the EW
  x=where(wav1 gt 8660. and wav1 lt 8664.5) ;zoom in on the line 
  tempwav=wav1(x)
  y=newsnip3a(x)
  cof = poly_fit(x,y,2)
  center =  -0.5*cof(1)/cof(2)            ;line center pixel
  x3=center
  delpix=5
  coreflux=newsnip3a(x3)

  ;continuum windows
  c1a=fltarr(10)   &   c2a=c1a   &   c1b=c1a   &   c2b=c1a
  c1a=newsnip3a((center-95):(center-86))
  c2a=newsnip3a((center+101):(center+110))
  c1b=newsnip3b((center-95):(center-86))
  c2b=newsnip3b((center+101):(center+110))
  one=-95  & two=-86  & three=101  & four=110
  left=stdev(c1a-c1b)  & right=stdev(c2a-c2b)
;print,'left = ',left
;print,'right = ',right
print,'ratio: ',right/left
  if left gt 5*right then begin
	c1a=newsnip3a((center-125):(center-116))
	c1b=newsnip3b((center-125):(center-116))
	one=-125  
	two=-116
  endif
  if right gt 5*left then begin
	c2a=newsnip3a((center+86):(center+95))
	c2b=newsnip3b((center+86):(center+95))
	three=86
	four=95
print,'ratio: ',right/left
  endif
;  left=stdev(c1a-c1b)  & right=stdev(c2a-c2b)
print,'left = ',left
print,'right = ',right
print,'ratio: ',left/right
  ca=[c1a,c2a]
  cb=[c1b,c2b]
  ;now vertically shift to match the continuum levels for star and templ
  star=cb       &  templ=ca

  ycorlb,star,templ,yshift
  cb=cb+yshift(0)
  newsnip3b=newsnip3b+yshift(0)
  sd_cont=stdev(ca-cb)
;  sd_cont=sd_cont/sqrt(1+(sig)^2)
  ;print,'sd_cont: ',sd_cont
  ;account for sqrt(N) statistical error difference in core and 
  ;continuum because of different flux levels   
  contb=median(cb)
;  err8662=sd_cont/sqrt(coreflux/contb)
  err8662=sqrt((sd_cont/contb)^2 + (sd_cont)^2/(coreflux/contb))
  coreb=total(newsnip3b((x3-delpix):(x3+delpix)))/(2.*delpix+1.)
  S8662=coreb/contb

if dewar eq 18 then S8662=S8662+0.004  ;offset between dewar 13(39) and 6(18)
print,'fn= ',fn
tel=strcompress(sonofchip(fn),/remove_all)
print,tel
if tel eq 'SHANE' then S8662=S8662+0.005

 if Keyword_set(plot) or plot eq 'yes' then begin 
;    window,2
;loadct,13
    plot,wav1,newsnip3a,yr=[0,1.1],xr=[8650.,8675.];,co=89
    oplot,wav1,newsnip3b;,co=309         ;beautiful
    plots,[wav1(center-5),wav1(center-5)],[0,1.1]
    plots,[wav1(center+5),wav1(center+5)],[0,1.1]
    plots,[wav1(center+one),wav1(center+one)],[0,1.1]
    plots,[wav1(center+two),wav1(center+two)],[0,1.1]
    plots,[wav1(center+three),wav1(center+three)],[0,1.1]
    plots,[wav1(center+four),wav1(center+four)],[0,1.1]
;    plots,[wav1(center-125),wav1(center-125)],[0,1.1],linesty=2
;    plots,[wav1(center-66),wav1(center-66)],[0,1.1],linesty=2
;    plots,[wav1(center+91),wav1(center+91)],[0,1.1],linesty=2
;    plots,[wav1(center+130),wav1(center+130)],[0,1.1],linesty=2

print,'err8662: ',err8662
print,'S8662: ',S8662
  endif


end










