pro compfreepar,plot=plot

fn=findfile('hd*out',count=nstars)
restore,'../adK2004A.dat'

;elem=['Na',' Si ',' Ti ',' Fe ',' Ni']
abund=fltarr(5)
f0='(a7,a9,a8,a7,a7,a7,a7,a7,a12)'
f1='(a8,f8.0,f7.3,f7.2,f7.2,f7.2,f7.2,f7.2,a15)'
f2='(5a7)'
blform='(a60)'
j=0

teff=fltarr(nstars)  & feh=fltarr(nstars)  & grav=fltarr(nstars)
vsini=fltarr(nstars) 

  openw,2,'summary.dat'
	printf,2,format=f0,'Star','Teff','[Fe/H]','log g','Vsini','Vmac',$
		'Vmic','Chisq','  Comments'
  for i=0,nstars-1 do begin
        str1=''
        str2=''
        str3=''
        restore,fn(i)
        a=strpos(fn(i),'_')
        star=strmid(fn(i),0,a)
        hd=strmid(star,2,strlen(star)-2)
        hd=long(hd)
;        ntg=n_tags(sme)
;        if ntg lt 50 then goto, jump2 
	sme1=sme
	num=n_elements(sme1.chisq) 
        chisq=sme1.chisq(num-1)
        teff(i)=sme1.teff
        vmic=sme1.vmic
	vmac=sme1.vmac
	vsini(i)=sme1.vsini
        grav(i)=sme1.grav
	c=where(sme1.ab_free eq 1)	
	solar_abund,solar
	fr=where(sme1.glob_free eq 'FEH',nfr)
	if nfr eq 0 then abund(*)=sme1.abund(c)-solar(c)
	if nfr gt 0 then abund(*)=sme1.feh+sme1.abund(c)-solar(c)	
	feh(i)=abund(3)
;add data to ad structure
        x=where(hd eq ad.hd,nx)
        if nx eq 0 then begin 
		print, hd, 'not found'
		str3='Not in ad'
	endif
        if nx gt 0 then begin
            ad(x).fe_spect = feh(i)
            ad(x).teff_spect = teff(i)
            if ad(x).drop eq 'y' then str1=' DROP  '
            if ad(x).comments ne '?' then str2=ad(x).comments
            str3=str1+str2
        endif
;add blank lines every 5th line to improve readability 
            j=j+1
            tst=j/5.
            if (fix(tst) eq tst) then printf,2,'   ',format=blform
        printf,2,format=f1,star,teff(i),feh(i),grav(i),$
		vsini(i),vmac,vmic,chisq, str3
  end
  close,2

save,ad,file='../adK2004A_2.dat'


if keyword_set(plot) then begin
    !x.margin=[14,4]
    !y.margin=[6,4]
  x=where(ad.fe_spect lt 2.,nx)
  d=ad(x)
  ps_open,'Keck2004A_SME',/color
    loadct,39
    !p.background=255
    plot,d.fe_phot,d.fe_spect,ps=8,xra=[-0.1,0.5],yra=[-0.1,0.5],/xsty,/ysty,$
      xtit='[Fe/H]_phot', ytit='[Fe/H]_spect',titl='Analysis of 140 stars',col=0
    plots,[-0.1,0.5],[-0.1,0.5],col=0
    plot,d.teff_phot,d.teff_spect,ps=8,xra=[4800.,6500.],yra=[4800.,6500.],/xsty, $
      /ysty, xtit='Teff from uvby', ytit='Teff from spectroscopy'
   plots,[4800.,6500.], [4800.,6500.]
   plot,teff,grav,ps=8,xra=[6500.,5000.],yra=[5.0,3.5],xtit='Spectroscopic Teff', $
      ytit='log g'
   plothist,vsini,bin=0.5,xtit='Vsini (km/s)',ytit='Num stars',col=0
   plothist,feh,bin=0.05,xtit='[Fe/H] from SME analysis',ytit='Num stars',col=1
   plothist,teff,bin=50., xtit='Teff distribution from SME analysis',col=1
   plot,teff,feh,ps=8,xra=[6500.,4800.],yra=[-0.1,0.5],xtit='Teff (K) Spectroscopic',$
        ytit='[Fe/H] Spectroscopic',col=1
ps_close
endif

stop

end






