pro mg1_check

restore,'/Users/debrafischer/sme/format/5164.gf_nom.inp'
smenom=sme

logg=[4.2,4.3,4.4,4.45,4..48,4.5,4.51,4.52,4.53,4.54,4.55,4.56,$
4.57,4.58,4.59,4.6,4.62,4.65,,4.7,4.8,4.9]

nr=n_elements(logg)
fileout=strarr(nr)

b1=5166.459
b2=5167.803
m1=5171.817
m2=5773.435
u1=5182.463
u2=5184.753
ewb=fltarr(nr)  & ewm=fltarr(nr)  & ewu=fltarr(nr)

for i=0,nr-1 do begin
    sme=smenom
    sme.grav=logg(i)
    sme_main,sme
    fileout(i)='mg1'+strcompress(string(logg(i)),/remove_all)+'.out'
    save,sme,file=fileout(i)
    xx=where(sme.wave ge b1 and sme.wave le b2,nxx)
    dum=fltarr(nxx)
    for j=0,nxx-1 do dum(j)=1.-sme.smod(xx(j))
    ewb(i)=total(dum)
    xx2=where(sme.wave ge m1 and sme.wave le m2,nxx2)
    dum2=fltarr(nxx2)
    for k=0,nxx2-1 do dum(k)=1.-sme.smod(xx(k))
    ewm(i)=total(dum2)
    xx3=where(sme.wave ge u1 and sme.wave le u2,nxx3)
    dum3=fltarr(nxx3)
    for l=0,nxx3-1 do dum(l)=1.-sme.smod(xx(l))
    ewu(i)=total(dum3)
endfor

stop

end


    
