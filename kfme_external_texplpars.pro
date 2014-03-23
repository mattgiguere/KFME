;PURPOSE: To return a letter equivalent to a number for
;	use in variable naming in LaTeX
function num2let, num
num = strt(long(num))
case num of
  '1':l='o'
  '2':l='w'
  '3':l='h'
  '4':l='u'
  '5':l='v'
  '6':l='x'
  '7':l='s'
  '8':l='e'
  '9':l='n'
  '0':l='z'
endcase
return, l
end;num2let.pro

;***********************************************************************
; NAME: KFME_EXTERNAL_TEXPLPARS
;																	   
; PURPOSE: This procedure prints the planetary parameters in a format
;    that can be thrown right into a paper
;																	   
; CATEGORY: EXOPLANETS							   
;																	   
; CALLING SEQUENCE:													   
;																	   
;																	   
; INPUTS:															   
;
; OPTIONAL INPUTS:													   
;																	   
; KEYWORD PARAMETERS:	
;
; OUTPUTS:															   
;																	   
; OPTIONAL OUTPUTS:													   
;																	   
; SIDE EFFECTS:														   
;																	   
; RESTRICTIONS:														   
;																	   
; PROCEDURE:														   
;																	   
; EXAMPLE:			
;																	   
; MODIFICATION HISTORY:												   
;     c. Matt Giguere, Wednesday, September 29 2010
;***********************************************************************
pro kfme_external_texplpars, pstate

xst = size(pstate)
if ~xst[2] then restore, 'widgetprinttest.dat'
;stop

;CONSTANTS:

;Newton's Gravitational Constant:
G = 6.67d*10.d^(-11.d)
;Convert from meters to Astronomical Units:
AU = 1.49598d11		;in meters
;Mass of the Sun:
msun = 1.9891d30	;in kg
;Mass of Jupiter
mjup = 1.8987d27	;in kg
;Mass of Jupiter to Earth Mass:
mjup2earth = 317.816611d
;Radius of the Sun:
rsun = 6.955d8		;in meters

;**********************************************************************
;	PRINTING TO FILE...
;**********************************************************************
;SETTING UP DIRECTORIES:
opdir = (*pstate).outputdir
dfi = file_info(opdir)
if ~dfi.exists then spawn, 'mkdir '+opdir
kfmedir = (*pstate).kfmedir
tdir = opdir+'PDFS/'
dir2 = file_info(tdir)
if ~dir2.exists then begin
  spawn, 'mkdir '+tdir
  spawn, 'cp '+kfmedir+'deluxetable.sty '+tdir
endif
time_stamp = jul2cal(systime(/julian), /dashn)
 pcfname=(*(*pstate).pcfname)

 ;if the filename is a vst structure name, then chop off the beginning and end
 if strmatch(pcfname, '*vst*') then begin
	firstchar=stregex(pcfname, 'vst')
	lastchar=stregex(pcfname, '.dat')
	extitle=strmid(pcfname, firstchar+3, lastchar-firstchar-3)
 endif else extitle = pcfname
 print, 'extitle is: ', extitle

if ~keyword_set(extitle) then begin
read, 'Please enter the name of the star: ', extitle
endif else filen = extitle

fulldir = tdir+extitle
xst = file_info(fulldir)
if ~(xst.exists) then spawn, 'mkdir '+ fulldir

fullnm = tdir+extitle+'/'+extitle+time_stamp+'.tex'

get_lun, fnum
xst = file_info(fullnm)
if ~(xst.exists) then spawn, 'touch '+ fullnm
openw, fnum, fullnm

dtsty = tdir+extitle+'/deluxetable.sty'
xst = file_info(dtsty)
if ~(xst.exists) then spawn, 'cp ~/kfme_output/PDFS/deluxetable.sty '+ fulldir

nplanets = (*(*pstate).pfunctargs).n_planets
n_planets=nplanets

print, 'time_stamp is: ', time_stamp

printf, fnum, '\documentclass[letter, 10pt]{article}'
printf, fnum, '\usepackage{geometry}'
printf, fnum, '\geometry{letterpaper}   '
printf, fnum, '\usepackage{graphicx}'
printf, fnum, '\usepackage{amssymb}'
printf, fnum, '\usepackage{epstopdf}'
printf, fnum, '\usepackage{deluxetable}'
printf, fnum, '\usepackage{rotating}'
printf, fnum, '\usepackage{epsfig}'
printf, fnum, ''
printf, fnum, '\newcommand{\mjup}{$M_{\rm Jup}$}'
printf, fnum, '\title{ORBITAL PARAMETERS FOR '+extitle+'}'
printf, fnum, '%\author{Matt Giguere}'
printf, fnum, '\date{'+time_stamp+'} '
printf, fnum, '\begin{document}'
printf, fnum, '\maketitle'


printf, fnum, ''
;printf, fnum, ''
;printf, fnum, '\begin{center}'
;printf, fnum, '\begin{tabular}{ccc||ccc}'
;printf, fnum, '	 \hline'
;printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
;				'   \textbf{1-$\sigma$}  & '
;printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
;				'   \textbf{1-$\sigma$ } \\'
;printf, fnum, '	\hline \hline'

if (*(*pstate).ptransit) then begin
  p_unc = (*(*pstate).ptransit).unc_per
  tp_unc = (*(*pstate).ptransit).tp_unc
endif

planet = (*pstate).pars.par1
if ~keyword_set(p_unc) then p_unc='0'
p = planet[0].value
;printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
if ~keyword_set(Tp_unc) then Tp_unc='0'
;printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
;stop
;printf, fnum, ''
if ~keyword_set(e_unc) then e_unc='0'
;printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
if ~keyword_set(om_unc) then om_unc='0'
;printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
;printf, fnum, ''
;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
;printf, fnum, ''
;printf, fnum, '\hline'
;printf, fnum, ''
msini1 = planet[1].value
;printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
;printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
;printf, fnum, ''
if ~keyword_set(unc_mstar) then unc_mstar=''
;printf, fnum, 'M$_{\star}$ (M$_{\odot}$)   & ',$
;(*(*pstate).pfunctargs).m_star,' & ',unc_mstar,' & '
rstar = (*(*pstate).pfunctargs).rstar
if ~keyword_set(unc_rstar) then unc_rstar=''
;printf, fnum, 'R$_{\star}$ (R$_{\odot}$)   & ',rstar,' & ',unc_rstar,' \\'
;printf, fnum, ''
if ~keyword_set(unc_gam) then unc_gam=''
gam = (*pstate).pars.par1[5].value
;printf, fnum, '$\gamma$   & ',gam,' & ',unc_gam,' & '
dvdt= (*pstate).pars.par1[6].value
if ~keyword_set(unc_dvdt) then unc_dvdt=''
;printf, fnum, 'dvdt   & ',dvdt,' & ',unc_dvdt,'   \\'
;printf, fnum, ''

psec = p * 24.d * 3600.d
mstar = (*(*pstate).pfunctargs).m_star

;Gravitational Constant:
G = 6.67d-11 ;m^3 kg^-1 
;Convert from meters to Astronomical Units:
AU = 1.49598d11		;in meters
;Mass of the Sun:
msun = 1.9891d30	;in kg
;Mass of Jupiter
mjup = 1.8987d27	;in kg
;Mass of Earth: 
mearth = 5.976d24 ;in kg
;Mass of Jupiter to Earth Mass:
mjup2earth = 317.816611d
;Radius of the Sun:
rsun = 6.955d8		;in meters
restore, kfmedir+'data/solarsystem.dat'
msini1 *= (system.cgs.earth.mass/1d3)
mstar *= (system.cgs.sun.mass/1d3)


lhs = (psec/(2d*!dpi))^2d
arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
print, 'arel is: ', arel/AU

;printf, fnum, ''
;printf, fnum, 'arel (m)   & ',arel,' &  & '
;printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
;printf, fnum, ''

;printf, fnum, '\hline'

period=p
apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
m_pl_earth = msini1
inc = 89.9d
ecc = planet[2].value

k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)

nobs = n_elements((*(*pstate).pcf).cf_rv.jd)
jitter = (*pstate).jitternum
meansig = mean((*(*pstate).pcf).cf_rv.errvel)
snr = k*sqrt(nobs) / (*pstate).rmsresid

;printf, fnum, ''
;printf, fnum, 'k (m/s)   & ',k,' & &  snr & ',snr,' & \\ '
;printf, fnum, ''

;printf, fnum, '\hline'


;printf, fnum, ''
comp='b'
;printf, fnum, 'comp   & ',comp,' &  & '
;printf, fnum, '\# of planets   & ',nplanets,' &    \\'
;printf, fnum, ''
;printf, fnum, '\hline '
;printf, fnum, ''
chi = (*pstate).chisq
;printf, fnum, '$\chi^{2}_{R}$  & ',chi,' &  & '
rms = (*pstate).rmsresid
;printf, fnum, 'RMS   & ',rms,' &    \\'
;printf, fnum, ''
;printf, fnum, '\hline '
;printf, fnum, ''
;printf, fnum, 'jitter  & ',jitter,' &  & '
mdnerr = median((*(*pstate).pcf).cf_rv.errvel)
;printf, fnum, 'Median Err   & ',mdnerr,' &    \\'
;printf, fnum, ''
;printf, fnum, '\hline '
;printf, fnum, ''
;printf, fnum, '\# of observations   & ',nobs,' &    \\'
;printf, fnum, ''
lastob = jul2cal((*(*pstate).pcf).cf_rv[nobs-1].jd +2.44d6, /date)
;printf, fnum, 'Date of Last Obs.  & ',lastob,' &  & '
;printf, fnum, 'Planet ID   & ',extitle,' &    \\'
;printf, fnum, ''
;printf, fnum, '\hline '
;printf, fnum, '\end{tabular}'
;printf, fnum, '\end{center}'
printf, fnum, ''
printf, fnum, ''

if nplanets gt 1 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par2
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet2

if nplanets gt 2 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par3
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet3

if nplanets gt 3 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par4
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet4

if nplanets gt 4 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par5
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet5

if nplanets gt 5 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par6
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet6

if nplanets gt 6 then begin
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\begin{center}'
	printf, fnum, '\begin{tabular}{ccc||ccc}'
	printf, fnum, '	\hline \hline'
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$}  & '
	printf, fnum, '\textbf{parameter} &  \textbf{value}    &', $
					'   \textbf{1-$\sigma$ } \\'
	printf, fnum, '	\hline \hline'
	
	planet = (*pstate).pars.par7
	p = planet[0].value
	if ~keyword_set(p_unc) then p_unc='0'
	printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
	if ~keyword_set(Tp_unc) then Tp_unc='0'
	printf, fnum, 'Tp (d)   & ',planet[4].value,' & ',Tp_unc,'   \\'
	printf, fnum, ''
	if ~keyword_set(e_unc) then e_unc='0'
	printf, fnum, 'e   & ',planet[2].value,' & ',e_unc,' & '
	if ~keyword_set(om_unc) then om_unc='0'
	printf, fnum, '$\omega$   & ',planet[3].value,' & ',om_unc,'   \\'
	printf, fnum, ''
	;printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
	;printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''
	msini1 = planet[1].value
	printf, fnum, 'msini (M$_{\oplus}$)   & ',msini1,' &  & '
	printf, fnum, 'msini (M$_{J}$)   & ',msini1/mjup2earth,' &    \\'
	printf, fnum, ''

	psec = p * 24.d * 3600.d
	mstar = (*(*pstate).pfunctargs).m_star
	
	;Gravitational Constant:
	G = 6.67d-11 ;m^3 kg^-1 
	;Convert from meters to Astronomical Units:
	AU = 1.49598d11		;in meters
	;Mass of the Sun:
	msun = 1.9891d30	;in kg
	;Mass of Jupiter
	mjup = 1.8987d27	;in kg
	;Mass of Jupiter to Earth Mass:
	mjup2earth = 317.816611d
	;Radius of the Sun:
	rsun = 6.955d8		;in meters
    restore, kfmedir+'data/solarsystem.dat'
	msini1 *= (system.cgs.earth.mass/1d3)
	mstar *= (system.cgs.sun.mass/1d3)
	
	
	lhs = (psec/(2d*!dpi))^2d
	arel = (lhs*(G*(msini1+mstar)))^(1d/3d)
	print, 'arel is: ', arel/AU
	
	printf, fnum, ''
	printf, fnum, 'arel (m)   & ',arel,' &  & '
	printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
	printf, fnum, ''

	printf, fnum, ''
	printf, fnum, '\hline'
	printf, fnum, ''

	period=p
	apl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(apl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
	printf, fnum, ''
	printf, fnum, 'k (m/s)   & ',k,' &  \\ '
	printf, fnum, ''
	
	printf, fnum, '\hline'
	
	comp='c'
	printf, fnum, 'comp   & ',comp,' &  & '
	printf, fnum, '\# of planets   & ',nplanets,' &    \\'
	printf, fnum, ''
	printf, fnum, ''
	printf, fnum, '\hline '
	printf, fnum, '\end{tabular}'
	printf, fnum, '\end{center}'
	printf, fnum, ''
	printf, fnum, ''
endif;planet7

  first3 = num2let(strmid(strt(extitle),0,1))+$
		   num2let(strmid(strt(extitle),1,1))+$
		   num2let(strmid(strt(extitle),2,1))
		   
  p1 = (*pstate).pars.par1[0].value
  p1str1 = string(p1, format='(D8.2)')
  punc1 = (*pstate).pars.par1[0].error
  punc1str1 = string(punc1, format='(D8.2)')
  

  Tp1 = (*pstate).pars.par1[4].value
  Tp1str1 = string(Tp1, format='(D11.2)')
  Tpunc1 = (*pstate).pars.par1[4].error
  Tpunc1str1 = string(Tpunc1, format='(D11.2)')

  e1 = (*pstate).pars.par1[2].value
  e1str1 = string(e1, format='(D11.2)')
  eunc1 = (*pstate).pars.par1[2].error
  eunc1str1 = string(eunc1, format='(D11.2)')

  om1 = (*pstate).pars.par1[3].value
  om1str1 = string(om1, format='(D11.2)')
  omunc1 = (*pstate).pars.par1[3].error
  omunc1str1 = string(omunc1, format='(D11.2)')

  m_pl_earth1 = (*pstate).pars.par1[1].value
  m1str1 = string(m_pl_earth1/mjup2earth, format='(D11.2)')
  m1unc = (*pstate).pars.par1[1].error
  munc1str1 = string(m1unc/mjup2earth, format='(D11.2)')

  apl1=((p1/365.2564d)^2*mstar/msun)^(1./3.)
  apl1str1 = string(apl1, format='(D12.4)')
  apl1unc = (*pstate).pars.par1[11].error
  aplunc1str1 =  string(apl1unc, format='(D12.4)')

  k1 = mpf_K(apl1, m_pl_earth1, p1, mstar/msun, inc, e1)
  k1str1 = string(k1, format='(D11.2)')
  k1unc = (*pstate).pars.par1[10].error
  kunc1str1 =  string(k1unc, format='(D11.2)')

  nobs = n_elements((*(*pstate).pcf).cf_rv)
  jitter = (*pstate).jitternum
  snr1 = k1*sqrt(nobs) /  (*pstate).rmsresid
  snr1str1 = strt(snr1, f='(F5.1)')
  ;printf, fnum, '\hline'
  dvdtstr1 = strt((*pstate).pars.par1[6].value*365.25, f='(F7.3)')
  dvdtuncstr1 = strt((*pstate).pars.par1[6].error*365.25, f='(F7.3)')
  printf, fnum, '\def\dvdt'+first3+'{$'+dvdtstr1+'$}'  
  printf, fnum, '\def\dvdtunc'+first3+'{$'+dvdtuncstr1+'$}'  
  dvdtstr2 = '\dvdt'+first3
  dvdtuncstr2 = '\dvdtunc'+first3

  printf, fnum, '\def\nobs'+first3+'{$'+strt(nobs)+'$}'  
  nobsstr = '\nobs'+first3
  printf, fnum, '\def\jitter'+first3+'{$'+strt(jitter, f='(F10.1)')+'$}'  
  jitterstr = '\jitter'+first3
  printf, fnum, '\def\rms'+first3+'{$'+strt((*pstate).rmsresid, f='(D6.2)')+'$}'  
  rmsstr = '\rms'+first3
  printf, fnum, '\def\chinu'+first3+'{$'+strt((*pstate).chisq, f='(D6.2)')+'$}'  
  chinustr = '\chinu'+first3
  printf, fnum, ' '
  
  printf, fnum, '\def\orbperb'+first3+'{$'+strt(p1str1)+'$}'
  p1str2 = '\orbperb'+first3
  printf, fnum, '\def\orbperuncb'+first3+'{$'+strt(punc1str1)+'$}'
  punc1str2 = '\orbperuncb'+first3
  
  printf, fnum, '\def\orbtpb'+first3+'{$'+strt(Tp1str1)+'$}'
  Tp1str2 = '\orbtpb'+first3
  printf, fnum, '\def\orbtpuncb'+first3+'{$'+strt(Tpunc1str1)+'$}'
  Tpunc1str2 = '\orbtpuncb'+first3
  
  printf, fnum, '\def\orbeb'+first3+'{$'+strt(e1str1)+'$}'
  e1str2 = '\orbeb'+first3
  printf, fnum, '\def\orbeuncb'+first3+'{$'+strt(eunc1str1)+'$}'
  eunc1str2 = '\orbeuncb'+first3
  
  printf, fnum, '\def\orbomb'+first3+'{$'+strt(om1str1)+'$}'
  om1str2 = '\orbomb'+first3
  printf, fnum, '\def\orbomuncb'+first3+'{$'+strt(omunc1str1)+'$}'
  omunc1str2 = '\orbomuncb'+first3
  
  printf, fnum, '\def\orbmb'+first3+'{$'+strt(m1str1)+'$}'
  m1str2 = '\orbmb'+first3
  printf, fnum, '\def\orbmuncb'+first3+'{$'+strt(munc1str1)+'$}'
  munc1str2 = '\orbmuncb'+first3
  
  printf, fnum, '\def\orbaplb'+first3+'{$'+strt(apl1str1)+'$}'
  apl1str2 = '\orbaplb'+first3
  printf, fnum, '\def\orbapluncb'+first3+'{$'+strt(aplunc1str1)+'$}'
  aplunc1str2 = '\orbapluncb'+first3
  
  printf, fnum, '\def\orbkb'+first3+'{$'+strt(k1str1)+'$}'
  k1str2 = '\orbkb'+first3
  printf, fnum, '\def\orbkuncb'+first3+'{$'+strt(kunc1str1)+'$}'
  kunc1str2 = '\orbkuncb'+first3
  
  printf, fnum, '\def\orbsnrb'+first3+'{$'+strt(snr1str1)+'$}'
  snr1str2 = '\orbsnrb'+first3
  printf, fnum, ' '
  
if n_planets ge 2 then begin

  p2 = (*pstate).pars.par2[0].value
  p2str1 = string(p2, format='(D8.2)')
  punc2 = (*pstate).pars.par2[0].error
  punc2str1 = string(punc2, format='(D6.2)')

  Tp2 = (*pstate).pars.par2[4].value
  Tp2str1 = string(Tp2, format='(D11.2)')
  Tpunc2 = (*pstate).pars.par2[4].error
  Tpunc2str1 = string(Tpunc2, format='(D11.2)')

  e2 = (*pstate).pars.par2[2].value
  e2str1 = string(e2, format='(D11.2)')
  eunc2 = (*pstate).pars.par2[2].error
  eunc2str1 = string(eunc2, format='(D11.2)')

  om2 = (*pstate).pars.par2[3].value
  om2str1 = string(om2, format='(D11.2)')
  omunc2 = (*pstate).pars.par2[3].error
  omunc2str1 = string(omunc2, format='(D11.2)')


  m_pl_earth2 = (*pstate).pars.par2[1].value
  m2str1 = string(m_pl_earth2/mjup2earth, format='(D11.2)')
  m2unc = (*pstate).pars.par2[1].error
  munc2str1 = string(m2unc/mjup2earth, format='(D11.2)')

  apl2=((p2/365.2564d)^2*mstar/msun)^(1./3.)
  apl2str1 = string(apl2, format='(D12.4)')
  apl2unc = (*pstate).pars.par2[6].error
  aplunc2str1 =  string(apl2unc, format='(D12.4)')

  k2 = mpf_K(apl2, m_pl_earth2, p2, mstar/msun, inc, e2)
  k2str1 = string(k2, format='(D11.2)')
  k2unc = (*pstate).pars.par2[5].error
  kunc2str1 =  string(k2unc, format='(D11.2)')

  snr2 = k2*sqrt(nobs) /  (*pstate).rmsresid
  snr2str1 = strt(snr2, f='(F5.1)')

  printf, fnum, '\def\orbperc'+first3+'{$'+strt(p2str1)+'$}'
  p2str2 = '\orbperc'+first3
  printf, fnum, '\def\orbperuncc'+first3+'{$'+strt(punc2str1)+'$}'
  punc2str2 = '\orbperuncc'+first3
  
  printf, fnum, '\def\orbtpc'+first3+'{$'+strt(Tp2str1)+'$}'
  Tp2str2 = '\orbtpc'+first3
  printf, fnum, '\def\orbtpuncc'+first3+'{$'+strt(Tpunc2str1)+'$}'
  Tpunc2str2 = '\orbtpuncc'+first3
  
  printf, fnum, '\def\orbec'+first3+'{$'+strt(e2str1)+'$}'
  e2str2 = '\orbec'+first3
  printf, fnum, '\def\orbeuncc'+first3+'{$'+strt(eunc2str1)+'$}'
  eunc2str2 = '\orbeuncc'+first3
  
  printf, fnum, '\def\orbomc'+first3+'{$'+strt(om2str1)+'$}'
  om2str2 = '\orbomc'+first3
  printf, fnum, '\def\orbomuncc'+first3+'{$'+strt(omunc2str1)+'$}'
  omunc2str2 = '\orbomuncc'+first3
  
  printf, fnum, '\def\orbmc'+first3+'{$'+strt(m2str1)+'$}'
  m2str2 = '\orbmc'+first3
  printf, fnum, '\def\orbmuncc'+first3+'{$'+strt(munc2str1)+'$}'
  munc2str2 = '\orbmuncc'+first3
  
  printf, fnum, '\def\orbaplc'+first3+'{$'+strt(apl2str1)+'$}'
  apl2str2 = '\orbaplc'+first3
  printf, fnum, '\def\orbapluncc'+first3+'{$'+strt(aplunc2str1)+'$}'
  aplunc2str2 = '\orbapluncc'+first3
  
  printf, fnum, '\def\orbkc'+first3+'{$'+strt(k2str1)+'$}'
  k2str2 = '\orbkc'+first3
  printf, fnum, '\def\orbkuncc'+first3+'{$'+strt(kunc2str1)+'$}'
  kunc2str2 = '\orbkuncc'+first3
  
  printf, fnum, '\def\orbsnrc'+first3+'{$'+strt(snr2str1)+'$}'
  snr2str2 = '\orbsnrc'+first3
  printf, fnum, ' '
  
endif;2nd planet

if n_planets ge 3 then begin

  p3 = (*pstate).pars.par3[0].value
  p3str1 = string(p3, format='(D8.2)')
  punc3 = (*pstate).pars.par3[0].error
  punc3str1 = string(punc3, format='(D6.2)')

  Tp3 = (*pstate).pars.par3[4].value
  Tp3str1 = string(Tp3, format='(D11.2)')
  Tpunc3 = (*pstate).pars.par3[4].error
  Tpunc3str1 = string(Tpunc3, format='(D11.2)')

  e3 = (*pstate).pars.par3[2].value
  e3str1 = string(e3, format='(D11.2)')
  eunc3 = (*pstate).pars.par3[2].error
  eunc3str1 = string(eunc3, format='(D11.2)')

  om3 = (*pstate).pars.par3[3].value
  om3str1 = string(om3, format='(D11.2)')
  omunc3 = (*pstate).pars.par3[3].error
  omunc3str1 = string(omunc3, format='(D11.2)')


  m_pl_earth3 = (*pstate).pars.par3[1].value
  m3str1 = string(m_pl_earth3/mjup2earth, format='(D11.2)')
  m3unc = (*pstate).pars.par3[1].error
  munc3str1 = string(m3unc/mjup2earth, format='(D11.2)')

  apl3=((p3/365.2564d)^2*mstar/msun)^(1./3.)
  apl3str1 = string(apl3, format='(D12.4)')
  apl3unc = (*pstate).pars.par3[6].error
  aplunc3str1 =  string(apl3unc, format='(D12.4)')

  k3 = mpf_K(apl3, m_pl_earth3, p3, mstar/msun, inc, e3)
  k3str1 = string(k3, format='(D11.2)')
  k3unc = (*pstate).pars.par3[5].error
  kunc3str1 =  string(k3unc, format='(D11.2)')

  snr3 = k3*sqrt(nobs) /  (*pstate).rmsresid
  snr3str1 = strt(snr3, f='(F5.1)')

  printf, fnum, '\def\orbperd'+first3+'{$'+strt(p3str1)+'$}'
  p3str2 = '\orbperd'+first3
  printf, fnum, '\def\orbperuncd'+first3+'{$'+strt(punc3str1)+'$}'
  punc3str2 = '\orbperuncd'+first3
  
  printf, fnum, '\def\orbtpd'+first3+'{$'+strt(Tp3str1)+'$}'
  Tp3str2 = '\orbtpd'+first3
  printf, fnum, '\def\orbtpuncd'+first3+'{$'+strt(Tpunc3str1)+'$}'
  Tpunc3str2 = '\orbtpuncd'+first3
  
  printf, fnum, '\def\orbed'+first3+'{$'+strt(e3str1)+'$}'
  e3str2 = '\orbed'+first3
  printf, fnum, '\def\orbeuncd'+first3+'{$'+strt(eunc3str1)+'$}'
  eunc3str2 = '\orbeuncd'+first3
  
  printf, fnum, '\def\orbomd'+first3+'{$'+strt(om3str1)+'$}'
  om3str2 = '\orbomd'+first3
  printf, fnum, '\def\orbomuncd'+first3+'{$'+strt(omunc3str1)+'$}'
  omunc3str2 = '\orbomuncd'+first3
  
  printf, fnum, '\def\orbmd'+first3+'{$'+strt(m3str1)+'$}'
  m3str2 = '\orbmd'+first3
  printf, fnum, '\def\orbmuncd'+first3+'{$'+strt(munc3str1)+'$}'
  munc3str2 = '\orbmuncd'+first3
  
  printf, fnum, '\def\orbapld'+first3+'{$'+strt(apl3str1)+'$}'
  apl3str2 = '\orbapld'+first3
  printf, fnum, '\def\orbapluncd'+first3+'{$'+strt(aplunc3str1)+'$}'
  aplunc3str2 = '\orbapluncd'+first3
  
  printf, fnum, '\def\orbkd'+first3+'{$'+strt(k3str1)+'$}'
  k3str2 = '\orbkd'+first3
  printf, fnum, '\def\orbkuncd'+first3+'{$'+strt(kunc3str1)+'$}'
  kunc3str2 = '\orbkuncd'+first3
  
  printf, fnum, '\def\orbsnrd'+first3+'{$'+strt(snr3str1)+'$}'
  snr3str2 = '\orbsnrd'+first3
  printf, fnum, ' '
  
endif; 3rd planet

if n_planets ge 4 then begin

  p4 = (*pstate).pars.par4[0].value
  p4str1 = string(p4, format='(D8.2)')
  punc4 = (*pstate).pars.par4[0].error
  punc4str1 = string(punc4, format='(D6.2)')

  Tp4 = (*pstate).pars.par4[4].value
  Tp4str1 = string(Tp4, format='(D11.2)')
  Tpunc4 = (*pstate).pars.par4[4].error
  Tpunc4str1 = string(Tpunc4, format='(D11.2)')

  e4 = (*pstate).pars.par4[2].value
  e4str1 = string(e4, format='(D11.2)')
  eunc4 = (*pstate).pars.par4[2].error
  eunc4str1 = string(eunc4, format='(D11.2)')

  om4 = (*pstate).pars.par4[3].value
  om4str1 = string(om4, format='(D11.2)')
  omunc4 = (*pstate).pars.par4[3].error
  omunc4str1 = string(omunc4, format='(D11.2)')


  m_pl_earth4 = (*pstate).pars.par4[1].value
  m4str1 = string(m_pl_earth4/mjup2earth, format='(D11.2)')
  m4unc = (*pstate).pars.par4[1].error
  munc4str1 = string(m4unc/mjup2earth, format='(D11.2)')

  apl4=((p4/365.2564d)^2*mstar/msun)^(1./3.)
  apl4str1 = string(apl4, format='(D12.4)')
  apl4unc = (*pstate).pars.par4[6].error
  aplunc4str1 =  string(apl4unc, format='(D12.4)')

  k4 = mpf_K(apl4, m_pl_earth4, p4, mstar/msun, inc, e4)
  k4str1 = string(k4, format='(D11.2)')
  k4unc = (*pstate).pars.par4[5].error
  kunc4str1 =  string(k4unc, format='(D11.2)')

  snr4 = k4*sqrt(nobs) /  (*pstate).rmsresid
  snr4str1 = strt(snr4, f='(F5.1)')

  printf, fnum, '\def\orbpere'+first3+'{$'+strt(p4str1)+'$}'
  p4str2 = '\orbpere'+first3
  printf, fnum, '\def\orbperunce'+first3+'{$'+strt(punc4str1)+'$}'
  punc4str2 = '\orbperunce'+first3
  
  printf, fnum, '\def\orbtpe'+first3+'{$'+strt(Tp4str1)+'$}'
  Tp4str2 = '\orbtpe'+first3
  printf, fnum, '\def\orbtpunce'+first3+'{$'+strt(Tpunc4str1)+'$}'
  Tpunc4str2 = '\orbtpunce'+first3
  
  printf, fnum, '\def\orbee'+first3+'{$'+strt(e4str1)+'$}'
  e4str2 = '\orbee'+first3
  printf, fnum, '\def\orbeunce'+first3+'{$'+strt(eunc4str1)+'$}'
  eunc4str2 = '\orbeunce'+first3
  
  printf, fnum, '\def\orbome'+first3+'{$'+strt(om4str1)+'$}'
  om4str2 = '\orbome'+first3
  printf, fnum, '\def\orbomunce'+first3+'{$'+strt(omunc4str1)+'$}'
  omunc4str2 = '\orbomunce'+first3
  
  printf, fnum, '\def\orbme'+first3+'{$'+strt(m4str1)+'$}'
  m4str2 = '\orbme'+first3
  printf, fnum, '\def\orbmunce'+first3+'{$'+strt(munc4str1)+'$}'
  munc4str2 = '\orbmunce'+first3
  
  printf, fnum, '\def\orbaple'+first3+'{$'+strt(apl4str1)+'$}'
  apl4str2 = '\orbaple'+first3
  printf, fnum, '\def\orbaplunce'+first3+'{$'+strt(aplunc4str1)+'$}'
  aplunc4str2 = '\orbaplunce'+first3
  
  printf, fnum, '\def\orbke'+first3+'{$'+strt(k4str1)+'$}'
  k4str2 = '\orbke'+first3
  printf, fnum, '\def\orbkunce'+first3+'{$'+strt(kunc4str1)+'$}'
  kunc4str2 = '\orbkunce'+first3
    
  printf, fnum, '\def\orbsnre'+first3+'{$'+strt(snr4str1)+'$}'
  snr4str2 = '\orbsnre'+first3
  printf, fnum, ' '
  
endif; 4th planet

if n_planets ge 5 then begin

  p5 = (*pstate).pars.par5[0].value
  p5str1 = string(p5, format='(D8.2)')
  punc5 = (*pstate).pars.par5[0].error
  punc5str1 = string(punc5, format='(D6.2)')

  Tp5 = (*pstate).pars.par5[4].value
  Tp5str1 = string(Tp5, format='(D11.2)')
  Tpunc5 = (*pstate).pars.par5[4].error
  Tpunc5str1 = string(Tpunc5, format='(D11.2)')

  e5 = (*pstate).pars.par5[2].value
  e5str1 = string(e5, format='(D11.2)')
  eunc5 = (*pstate).pars.par5[2].error
  eunc5str1 = string(eunc5, format='(D11.2)')

  om5 = (*pstate).pars.par5[3].value
  om5str1 = string(om5, format='(D11.2)')
  omunc5 = (*pstate).pars.par5[3].error
  omunc5str1 = string(omunc5, format='(D11.2)')


  m_pl_earth5 = (*pstate).pars.par5[1].value
  m5str1 = string(m_pl_earth5/mjup2earth, format='(D11.2)')
  m5unc = (*pstate).pars.par5[1].error
  munc5str1 = string(m5unc/mjup2earth, format='(D11.2)')

  apl5=((p5/365.2564d)^2*mstar/msun)^(1./3.)
  apl5str1 = string(apl5, format='(D12.4)')
  apl5unc = (*pstate).pars.par5[6].error
  aplunc5str1 =  string(apl5unc, format='(D12.4)')

  k5 = mpf_K(apl5, m_pl_earth5, p5, mstar/msun, inc, e5)
  k5str1 = string(k5, format='(D11.2)')
  k5unc = (*pstate).pars.par5[5].error
  kunc5str1 =  string(k5unc, format='(D11.2)')

  snr5 = k5*sqrt(nobs) /  (*pstate).rmsresid
  snr5str1 = strt(snr5, f='(F5.1)')

  printf, fnum, '\def\orbperf'+first3+'{$'+strt(p5str1)+'$}'
  p5str2 = '\orbperf'+first3
  printf, fnum, '\def\orbperuncf'+first3+'{$'+strt(punc5str1)+'$}'
  punc5str2 = '\orbperuncf'+first3
  
  printf, fnum, '\def\orbtpf'+first3+'{$'+strt(Tp5str1)+'$}'
  Tp5str2 = '\orbtpf'+first3
  printf, fnum, '\def\orbtpuncf'+first3+'{$'+strt(Tpunc5str1)+'$}'
  Tpunc5str2 = '\orbtpuncf'+first3
  
  printf, fnum, '\def\orbef'+first3+'{$'+strt(e5str1)+'$}'
  e5str2 = '\orbef'+first3
  printf, fnum, '\def\orbeuncf'+first3+'{$'+strt(eunc5str1)+'$}'
  eunc5str2 = '\orbeuncf'+first3
  
  printf, fnum, '\def\orbomf'+first3+'{$'+strt(om5str1)+'$}'
  om5str2 = '\orbomf'+first3
  printf, fnum, '\def\orbomuncf'+first3+'{$'+strt(omunc5str1)+'$}'
  omunc5str2 = '\orbomuncf'+first3
  
  printf, fnum, '\def\orbmf'+first3+'{$'+strt(m5str1)+'$}'
  m5str2 = '\orbmf'+first3
  printf, fnum, '\def\orbmuncf'+first3+'{$'+strt(munc5str1)+'$}'
  munc5str2 = '\orbmuncf'+first3
  
  printf, fnum, '\def\orbaplf'+first3+'{$'+strt(apl5str1)+'$}'
  apl5str2 = '\orbaplf'+first3
  printf, fnum, '\def\orbapluncf'+first3+'{$'+strt(aplunc5str1)+'$}'
  aplunc5str2 = '\orbapluncf'+first3
  
  printf, fnum, '\def\orbkf'+first3+'{$'+strt(k5str1)+'$}'
  k5str2 = '\orbkf'+first3
  printf, fnum, '\def\orbkuncf'+first3+'{$'+strt(kunc5str1)+'$}'
  kunc5str2 = '\orbkuncf'+first3
  
  printf, fnum, '\def\orbsnrf'+first3+'{$'+strt(snr5str1)+'$}'
  snr5str2 = '\orbsnrf'+first3
  printf, fnum, ' '
  

endif; 5th planet

if n_planets ge 6 then begin

  p6 = (*pstate).pars.par6[0].value
  p6str1 = string(p6, format='(D8.2)')
  punc6 = (*pstate).pars.par6[0].error
  punc6str1 = string(punc6, format='(D6.2)')

  Tp6 = (*pstate).pars.par6[4].value
  Tp6str1 = string(Tp6, format='(D11.2)')
  Tpunc6 = (*pstate).pars.par6[4].error
  Tpunc6str1 = string(Tpunc6, format='(D11.2)')

  e6 = (*pstate).pars.par6[2].value
  e6str1 = string(e6, format='(D11.2)')
  eunc6 = (*pstate).pars.par6[2].error
  eunc6str1 = string(eunc6, format='(D11.2)')

  om6 = (*pstate).pars.par6[3].value
  om6str1 = string(om6, format='(D11.2)')
  omunc6 = (*pstate).pars.par6[3].error
  omunc6str1 = string(omunc6, format='(D11.2)')


  m_pl_earth6 = (*pstate).pars.par6[1].value
  m6str1 = string(m_pl_earth6/mjup2earth, format='(D11.2)')
  m6unc = (*pstate).pars.par6[1].error
  munc6str1 = string(m6unc/mjup2earth, format='(D11.2)')

  apl6=((p6/365.2564d)^2*mstar/msun)^(1./3.)
  apl6str1 = string(apl6, format='(D12.4)')
  apl6unc = (*pstate).pars.par6[6].error
  aplunc6str1 =  string(apl6unc, format='(D12.4)')

  k6 = mpf_K(apl6, m_pl_earth6, p6, mstar/msun, inc, e6)
  k6str1 = string(k6, format='(D11.2)')
  k6unc = (*pstate).pars.par6[5].error
  kunc6str1 =  string(k6unc, format='(D11.2)')

  snr6 = k6*sqrt(nobs) /  (*pstate).rmsresid
  snr6str1 = strt(snr6, f='(F5.1)')

  printf, fnum, '\def\orbperg'+first3+'{$'+strt(p6str1)+'$}'
  p6str2 = '\orbperg'+first3
  printf, fnum, '\def\orbperuncg'+first3+'{$'+strt(punc6str1)+'$}'
  punc6str2 = '\orbperuncg'+first3
  
  printf, fnum, '\def\orbtpg'+first3+'{$'+strt(Tp6str1)+'$}'
  Tp6str2 = '\orbtpg'+first3
  printf, fnum, '\def\orbtpuncg'+first3+'{$'+strt(Tpunc6str1)+'$}'
  Tpunc6str2 = '\orbtpuncg'+first3
  
  printf, fnum, '\def\orbeg'+first3+'{$'+strt(e6str1)+'$}'
  e6str2 = '\orbeg'+first3
  printf, fnum, '\def\orbeuncg'+first3+'{$'+strt(eunc6str1)+'$}'
  eunc6str2 = '\orbeuncg'+first3
  
  printf, fnum, '\def\orbomg'+first3+'{$'+strt(om6str1)+'$}'
  om6str2 = '\orbomg'+first3
  printf, fnum, '\def\orbomuncg'+first3+'{$'+strt(omunc6str1)+'$}'
  omunc6str2 = '\orbomuncg'+first3
  
  printf, fnum, '\def\orbmg'+first3+'{$'+strt(m6str1)+'$}'
  m6str2 = '\orbmg'+first3
  printf, fnum, '\def\orbmuncg'+first3+'{$'+strt(munc6str1)+'$}'
  munc6str2 = '\orbmuncg'+first3
  
  printf, fnum, '\def\orbaplg'+first3+'{$'+strt(apl6str1)+'$}'
  apl6str2 = '\orbaplg'+first3
  printf, fnum, '\def\orbapluncg'+first3+'{$'+strt(aplunc6str1)+'$}'
  aplunc6str2 = '\orbapluncg'+first3
  
  printf, fnum, '\def\orbkg'+first3+'{$'+strt(k6str1)+'$}'
  k6str2 = '\orbkg'+first3
  printf, fnum, '\def\orbkuncg'+first3+'{$'+strt(kunc6str1)+'$}'
  kunc6str2 = '\orbkuncg'+first3
  
  printf, fnum, '\def\orbsnrg'+first3+'{$'+strt(snr6str1)+'$}'
  snr6str2 = '\orbsnrg'+first3
  printf, fnum, ' '
  
endif; 6th planet

if n_planets ge 7 then begin

  p7 = (*pstate).pars.par7[0].value
  p7str1 = string(p7, format='(D8.2)')
  punc7 = (*pstate).pars.par7[0].error
  punc7str1 = string(punc7, format='(D6.2)')

  Tp7 = (*pstate).pars.par7[4].value
  Tp7str1 = string(Tp7, format='(D11.2)')
  Tpunc7 = (*pstate).pars.par7[4].error
  Tpunc7str1 = string(Tpunc7, format='(D11.2)')

  e7 = (*pstate).pars.par7[2].value
  e7str1 = string(e7, format='(D11.2)')
  eunc7 = (*pstate).pars.par7[2].error
  eunc7str1 = string(eunc7, format='(D11.2)')

  om7 = (*pstate).pars.par7[3].value
  om7str1 = string(om7, format='(D11.2)')
  omunc7 = (*pstate).pars.par7[3].error
  omunc7str1 = string(omunc7, format='(D11.2)')


  m_pl_earth7 = (*pstate).pars.par7[1].value
  m7str1 = string(m_pl_earth7/mjup2earth, format='(D11.2)')
  m7unc = (*pstate).pars.par7[1].error
  munc7str1 = string(m7unc/mjup2earth, format='(D11.2)')

  apl7=((p7/365.2564d)^2*mstar/msun)^(1./3.)
  apl7str1 = string(apl7, format='(D12.4)')
  apl7unc = (*pstate).pars.par7[6].error
  aplunc7str1 =  string(apl7unc, format='(D12.4)')

  k7 = mpf_K(apl7, m_pl_earth7, p7, mstar/msun, inc, e7)
  k7str1 = string(k7, format='(D11.2)')
  k7unc = (*pstate).pars.par7[5].error
  kunc7str1 =  string(k7unc, format='(D11.2)')

  snr7 = k7*sqrt(nobs) /  (*pstate).rmsresid
  snr7str1 = strt(snr7, f='(F5.1)')
	
  printf, fnum, '\def\orbperh'+first3+'{$'+strt(p7str1)+'$}'
  p7str2 = '\orbperh'+first3
  printf, fnum, '\def\orbperunch'+first3+'{$'+strt(punc7str1, f='(F15.3)')+'$}'
  punc7str2 = '\orbperunch'+first3
  
  printf, fnum, '\def\orbtph'+first3+'{$'+strt(Tp7str1)+'$}'
  Tp7str2 = '\orbtph'+first3
  printf, fnum, '\def\orbtpunch'+first3+'{$'+strt(Tpunc7str1)+'$}'
  Tpunc7str2 = '\orbtpunch'+first3
  
  printf, fnum, '\def\orbeh'+first3+'{$'+strt(e7str1)+'$}'
  e7str2 = '\orbeh'+first3
  printf, fnum, '\def\orbeunch'+first3+'{$'+strt(eunc7str1)+'$}'
  eunc7str2 = '\orbeunch'+first3
  
  printf, fnum, '\def\orbomh'+first3+'{$'+strt(om7str1)+'$}'
  om7str2 = '\orbomh'+first3
  printf, fnum, '\def\orbomunch'+first3+'{$'+strt(omunc7str1)+'$}'
  omunc7str2 = '\orbomunch'+first3
  
  printf, fnum, '\def\orbmh'+first3+'{$'+strt(m7str1)+'$}'
  m7str2 = '\orbmh'+first3
  printf, fnum, '\def\orbmunch'+first3+'{$'+strt(munc7str1)+'$}'
  munc7str2 = '\orbmunch'+first3
  
  printf, fnum, '\def\orbaplh'+first3+'{$'+strt(apl7str1)+'$}'
  apl7str2 = '\orbaplh'+first3
  printf, fnum, '\def\orbaplunch'+first3+'{$'+strt(aplunc7str1)+'$}'
  aplunc7str2 = '\orbaplunch'+first3
  
  printf, fnum, '\def\orbkh'+first3+'{$'+strt(k7str1)+'$}'
  k7str2 = '\orbkh'+first3
  printf, fnum, '\def\orbkunch'+first3+'{$'+strt(kunc7str1)+'$}'
  kunc7str2 = '\orbkunch'+first3
  
  printf, fnum, '\def\orbsnrh'+first3+'{$'+strt(snr7str1)+'$}'
  snr7str2 = '\orbsnrh'+first3
  printf, fnum, ' '
  
endif; 7th planet

if n_planets eq 1 then begin
  printf, fnum, '\begin{deluxetable}{lr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the companion detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif


if n_planets eq 2 then begin
  printf, fnum, '\begin{deluxetable}{lrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the two companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 3 then begin
  printf, fnum, '\begin{deluxetable}{lrrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the three companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c} & '
  printf, fnum, '\colhead{HD '+extitle+'d}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 4 then begin
  printf, fnum, '\begin{deluxetable}{lrrrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the four companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c} & '
  printf, fnum, '\colhead{HD '+extitle+'d} & '
  printf, fnum, '\colhead{HD '+extitle+'e}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 5 then begin
  printf, fnum, '\begin{deluxetable}{lrrrrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the five companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c} & '
  printf, fnum, '\colhead{HD '+extitle+'d} & '
  printf, fnum, '\colhead{HD '+extitle+'e} & '
  printf, fnum, '\colhead{HD '+extitle+'f}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 6 then begin
  printf, fnum, '\begin{deluxetable}{lrrrrrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the six companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c} & '
  printf, fnum, '\colhead{HD '+extitle+'d} & '
  printf, fnum, '\colhead{HD '+extitle+'e} & '
  printf, fnum, '\colhead{HD '+extitle+'f} & '
  printf, fnum, '\colhead{HD '+extitle+'g}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 7 then begin
  printf, fnum, '\begin{deluxetable}{lrrrrrrr}'
  printf, fnum, '\tablenum{5}'
  printf, fnum, '\tablecaption{Orbital Parameters for the seven companions detected orbiting HD~'+extitle+' \label{tab:orb}}'
  printf, fnum, '\tablewidth{0pt}'
  printf, fnum, '\tablehead{'
  printf, fnum, '\colhead{Parameter}  & '
  printf, fnum, '\colhead{HD '+extitle+'b} & '
  printf, fnum, '\colhead{HD '+extitle+'c} & '
  printf, fnum, '\colhead{HD '+extitle+'d} & '
  printf, fnum, '\colhead{HD '+extitle+'e} & '
  printf, fnum, '\colhead{HD '+extitle+'f} & '
  printf, fnum, '\colhead{HD '+extitle+'g} & '
  printf, fnum, '\colhead{HD '+extitle+'h}'
  printf, fnum, '} '
  printf, fnum, '\startdata'
endif

if n_planets eq 1 then begin

  printf, fnum, 'P(d) & ',p1str2,' $\pm$ ', punc1str2, '    \\'

  printf, fnum, 'T$_{P}$(JD) & ', Tp1str2, ' $\pm$ ', Tpunc1str2, ' \\'

  printf, fnum, 'e & ', e1str2, ' $\pm$ ', eunc1str2, '\\'

  printf, fnum, '$\omega$ & ', om1str2, '$\pm$ ', omunc1str2, ' \\'

  printf, fnum, 'K  (m s$^{-1}$) & ', k1str2, ' $\pm$ ', kunc1str2, ' \\'

  printf, fnum, 'a (AU) & ', apl1str2, ' $\pm$ ', aplunc1str2, ' \\'

  printf, fnum, 'M $\sin{i}$  (\mjup) & ', m1str2, ' $\pm$ ',munc1str2, ' \\'

  printf, fnum, 'SNR & ', snr1str2, ' \\'

endif;1 planet total

if n_planets eq 2 then begin

  printf, fnum, 'P(d) & ',p1str2,' $\pm$ ', punc1str2, ' & ', p2str2, ' $\pm$ ', punc2str2, '    \\'

  printf, fnum, 'T$_{P}$(JD) & ', Tp1str2, ' $\pm$ ', Tpunc1str2, '& ', Tp2str2, ' $\pm$', Tpunc2str2, ' \\'

  printf, fnum, 'e & ', e1str2, ' $\pm$ ', eunc1str2, ' & ', e2str2,' $\pm$ ', eunc2str2, '\\'

  printf, fnum, '$\omega$ & ', om1str2, '$\pm$ ', omunc1str2, ' & ', om2str2, ' $\pm$ ', omunc2str2, ' \\'

  printf, fnum, 'K  (m s$^{-1}$) & ', k1str2, ' $\pm$ ', kunc1str2, ' & ', k2str2, ' $\pm$ ', kunc2str2, ' \\'

  printf, fnum, 'a (AU) & ', apl1str2, ' $\pm$ ', aplunc1str2, ' & ', apl2str2, ' $\pm$ ', aplunc2str2, ' \\'

  printf, fnum, 'M $\sin{i}$  (\mjup) & ', m1str2, ' $\pm$ ',munc1str2, ' & ',m2str2, ' $\pm$ ', munc2str2, ' \\'

  printf, fnum, 'SNR & ', snr1str2, ' & ',snr2str2, ' \\'

endif;2 planets total

if n_planets eq 3 then begin

  printf, fnum, 'P(d) & ',p1str2,' $\pm$ ', punc1str2, ' & ', p2str2, ' $\pm$ ', punc2str2, ' & ', p3str2, ' $\pm$ ', punc3str2, '    \\'

  printf, fnum, 'T$_{P}$(JD) & ', Tp1str2, ' $\pm$ ', Tpunc1str2, '& ', Tp2str2, ' $\pm$', Tpunc2str2, '& ', Tp3str2, ' $\pm$', Tpunc3str2, ' \\'

  printf, fnum, 'e & ', e1str2, ' $\pm$ ', eunc1str2, ' & ', e2str2,' $\pm$ ', eunc2str2, ' & ', e3str2,' $\pm$ ', eunc3str2, '\\'

  printf, fnum, '$\omega$ & ', om1str2, '$\pm$ ', omunc1str2, ' & ', om2str2, ' $\pm$ ', omunc2str2, ' & ', om3str2, ' $\pm$ ', omunc3str2, ' \\'

  printf, fnum, 'K  (m s$^{-1}$) & ', k1str2, ' $\pm$ ', kunc1str2, ' & ', k2str2, ' $\pm$ ', kunc2str2, ' & ', k3str2, ' $\pm$ ', kunc3str2, ' \\'

  printf, fnum, 'a (AU) & ', apl1str2, ' $\pm$ ', aplunc1str2, ' & ', apl2str2, ' $\pm$ ', aplunc2str2, ' & ', apl3str2, ' $\pm$ ', aplunc3str2, ' \\'

  printf, fnum, 'M $\sin{i}$  (\mjup) & ', m1str2, ' $\pm$ ',munc1str2, ' & ',m2str2, ' $\pm$ ', munc2str2, ' & ',m3str2, ' $\pm$ ', munc3str2, ' \\'

  printf, fnum, 'SNR & ', snr1str2, ' & ',snr2str2, ' & ',snr3str2, ' \\'

endif;3 planets total

if n_planets eq 4 then begin

  printf, fnum, 'P(d) & ',p1str2,' $\pm$ ', punc1str2, ' & ', p2str2, ' $\pm$ ', punc2str2, ' & ', p3str2, ' $\pm$ ', punc3str2, ' & ', p4str2, ' $\pm$ ', punc4str2, '    \\'

  printf, fnum, 'T$_{P}$(JD) & ', Tp1str2, ' $\pm$ ', Tpunc1str2, '& ', Tp2str2, ' $\pm$', Tpunc2str2, '& ', Tp3str2, ' $\pm$', Tpunc3str2, '& ', Tp4str2, ' $\pm$', Tpunc4str2, ' \\'

  printf, fnum, 'e & ', e1str2, ' $\pm$ ', eunc1str2, ' & ', e2str2,' $\pm$ ', eunc2str2, ' & ', e3str2,' $\pm$ ', eunc3str2, ' & ', e4str2,' $\pm$ ', eunc4str2, '\\'

  printf, fnum, '$\omega$ & ', om1str2, '$\pm$ ', omunc1str2, ' & ', om2str2, ' $\pm$ ', omunc2str2, ' & ', om3str2, ' $\pm$ ', omunc3str2, ' & ', om4str2, ' $\pm$ ', omunc4str2, ' \\'

  printf, fnum, 'K  (m s$^{-1}$) & ', k1str2, ' $\pm$ ', kunc1str2, ' & ', k2str2, ' $\pm$ ', kunc2str2, ' & ', k3str2, ' $\pm$ ', kunc3str2, ' & ', k4str2, ' $\pm$ ', kunc4str2, ' \\'

  printf, fnum, 'a (AU) & ', apl1str2, ' $\pm$ ', aplunc1str2, ' & ', apl2str2, ' $\pm$ ', aplunc2str2, ' & ', apl3str2, ' $\pm$ ', aplunc3str2, ' & ', apl4str2, ' $\pm$ ', aplunc4str2, ' \\'

  printf, fnum, 'M $\sin{i}$  (\mjup) & ', m1str2, ' $\pm$ ',munc1str2, ' & ',m2str2, ' $\pm$ ', munc2str2, ' & ',m3str2, ' $\pm$ ', munc3str2, ' & ',m4str2, ' $\pm$ ', munc4str2, ' \\'

  printf, fnum, 'SNR & ', snr1str2, ' & ',snr2str2, ' & ',snr3str2, ' & ',snr4str2, ' \\'

endif;4 planets total

  if dvdt ne 0 then begin
  printf, fnum, '\textit{dv/dt} (m s$^{-1}$ yr$^{-1}$) & ',dvdtstr2,' $\pm$ ',dvdtuncstr2, ' \\'
  endif;include linear trend

  printf, fnum, 'N$_{\textrm{obs}}$   &   ',nobsstr,' \\'
  printf, fnum, 'Jitter (m s$^{-1}$)      & ', jitterstr, ' \\'
  printf, fnum, 'rms  (m s$^{-1}$) & ', rmsstr ,' \\'
  printf, fnum, '$\chi_{\nu}^{2}$ & ', chinustr,' \\'
  printf, fnum, '\enddata                        '
  printf, fnum, '\end{deluxetable}   '                       

printf, fnum, '\end{document}'

close, fnum
spawn, 'open '+fullnm
;stop
end; kfme_external_texplpars.pro
