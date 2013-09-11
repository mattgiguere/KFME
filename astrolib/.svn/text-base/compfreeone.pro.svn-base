pro compfreeone,teff,grav,monh,rotv,abundances,star=star,printout=printout

fn=' '
if not keyword_set(star) then read,'enter the output file (hr5072_2_n13) ',fn
if keyword_set(star) then fn = star

;elem=['C ',' Si ',' Ca ',' Ti ',' V ',' Cr ',' Mn ',' Fe ',' Ni']
  list = $
    [ 'H',  'He', 'Li', 'Be', 'B',  'C',  'N',  'O',  'F',  'Ne' $
    , 'Na', 'Mg', 'Al', 'Si', 'P',  'S',  'Cl', 'Ar', 'K',  'Ca' $
    , 'Sc', 'Ti', 'V',  'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn' $
    , 'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y',  'Zr' $
    , 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn' ]
  list = [ list $
    , 'Sb', 'Te', 'I',  'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd' $
    , 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb' $
    , 'Lu', 'Hf', 'Ta', 'W',  'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg' $
    , 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th' $
    , 'Pa', 'U',  'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es' ]

f0='(a10,a10,a9,a7,a7,a7,a7)'
f1='(a14,f8.2,f7.3,f7.3,f7.2,f7.3,f7.3)'
f2='(8a7)'
f3='(8f7.3)'  ;Open file to append ending flag
	restore,fn+'.out'
        on_ioerror, jump2
	c=where(sme.ab_free eq 1,nc)
	elem=list(c)
        ntg=n_tags(sme)
        if ntg lt 50 then goto, jump2 
	sme1=sme
	num=n_elements(sme1.chisq) 
	abund=fltarr(nc)	
	solar_abund,solar
	fr=where(sme1.glob_free eq 'FEH',nfr)
	if nfr eq 0 then abund(*)=sme1.abund(c)-solar(c)
	if nfr gt 0 then abund(*)=sme1.feh+sme1.abund(c)-solar(c)	
	if keyword_set(printout) then begin
           print,format=f0,'Star','Teff','Log g', '[M/H]','Vsini','Vmac',$
		'Vmic','Chisq'
           print,format=f1,fn,sme1.teff,sme1.grav,sme1.feh,$
		sme1.vsini,sme1.vmac,sme1.vmic,sme1.chisq(num-1)
	   print,format=f2,elem(*)
	   print,format=f3,abund(*)
	end ;if
        teff=sme1.teff
	grav=sme1.grav
	monh=sme1.feh
	rotv=sme1.vsini
	abundances=abund
jump2: 
return
end










