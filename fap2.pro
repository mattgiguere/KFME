pro fap2, star=star, cf1, ntrial=ntrial, fap_out=fap_out

loadct,39
!p.background=255
!p.font=1

if ~keyword_set(star) then star='?'
if star eq '196885' then begin
   restore,'./cfb.dat'
   cf1=cfb
endif

if star eq '30562' then restore,'/mir1/vstbank/vst30562.dat'
if star eq '87883' then restore,'/mir1/vstbank/vst87883.dat'
if star eq '148427' then restore,'/mir1/vstbank/vst148427.dat'
if star eq '86264' then restore,'/mir1/vstbank/vst86264.dat'
if star eq '87883_resid' then begin
   restore,'87883_cfresid.dat'
   cf1=cfresid
endif


pergram,cf1,nu_out,peri_out,pkperiods,pkheights
pkh=max(pkheights)

if ~keyword_set(ntrial) then ntrial=1000
mc_arr=fltarr(ntrial)
num=n_elements(nu_out)
;nu=fltarr(ntrial,num)  & pwr=fltarr(ntrial,num)

mnvel=mean(cf1.mnvel)
;scramble the velocities
ndata=n_elements(cf1) 
for j=0,ntrial-1 do begin
   for jj=0,ndata-1 do begin    ;replacement with redraw
     dum=fix(randomu(seed)*ndata)
     cf1(jj).mnvel=cf1(dum).mnvel-mnvel
   end
   pergram,cf1,nu_out,peri_out,pkperiods,pkheights
;wait,1
;   nu(j,*)=nu_out
;   pwr(j,*)=peri_out
   mc_arr(j)=max(pkheights)
end

gg=sort(mc_arr)
new_mc=mc_arr(gg)
rnew_mc=reverse(new_mc)

ind=where(rnew_mc gt pkh,nind)
if nind eq 0 then nind=1.0
fap=1.0*nind/n_elements(rnew_mc)

print,'FAP: ',fap

fap_out=fap


end
