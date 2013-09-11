;***********************************************************************
; NAME: KFME_EXTERNAL_TEXPARS
;																	   
; PURPOSE: This procedure is designed to print the information from both 
;		on the screen and in a LaTeX file 
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
;     c. Matt Giguere, Monday, January 18, 2010
;***********************************************************************
pro kfme_external_texpars, pstate

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

 firstchar=stregex(pcfname, 'vst')
 lastchar=stregex(pcfname, '.dat')
 extitle=strmid(pcfname, firstchar+3, lastchar-firstchar-3)
 print, 'extitle is: ', extitle
 ;stop

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
printf, fnum, '\title{ORBITAL PARAMETERS FOR '+extitle+'}'
printf, fnum, '%\author{Matt Giguere}'
printf, fnum, '\date{'+time_stamp+'} '
printf, fnum, '\begin{document}'
printf, fnum, '\maketitle'


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

planet = (*pstate).pars.par1
if ~keyword_set(p_unc) then p_unc='0'
p = planet[0].value
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
if ~keyword_set(unc_mstar) then unc_mstar=''
printf, fnum, 'M$_{\star}$ (M$_{\odot}$)   & ',$
(*(*pstate).pfunctargs).m_star,' & ',unc_mstar,' & '
rstar = (*(*pstate).pfunctargs).rstar
if ~keyword_set(unc_rstar) then unc_rstar=''
printf, fnum, 'R$_{\star}$ (R$_{\odot}$)   & ',rstar,' & ',unc_rstar,' \\'
printf, fnum, ''
if ~keyword_set(unc_gam) then unc_gam=''
gam = (*pstate).pars.par1[5].value
printf, fnum, '$\gamma$   & ',gam,' & ',unc_gam,' & '
dvdt= (*pstate).pars.par1[6].value
if ~keyword_set(unc_dvdt) then unc_dvdt=''
printf, fnum, 'dvdt   & ',dvdt,' & ',unc_dvdt,'   \\'
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

printf, fnum, ''
printf, fnum, 'arel (m)   & ',arel,' &  & '
printf, fnum, 'arel (AU)   & ',arel/AU,' &    \\'
printf, fnum, ''

printf, fnum, '\hline'

period=p
a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
m_pl_earth = msini1
inc = 89.9d
ecc = planet[2].value

k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)

printf, fnum, ''
printf, fnum, 'k (m/s)   & ',k,' &  \\ '
printf, fnum, ''

printf, fnum, '\hline'


printf, fnum, ''
comp='b'
printf, fnum, 'comp   & ',comp,' &  & '
printf, fnum, '\# of planets   & ',nplanets,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, ''
chi = (*pstate).chisq
printf, fnum, '$\chi^{2}_{R}$  & ',chi,' &  & '
rms = (*pstate).rmsresid
printf, fnum, 'RMS   & ',rms,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, ''
jitter = (*pstate).jitternum
printf, fnum, 'jitter  & ',jitter,' &  & '
mdnerr = median((*(*pstate).pcf).cf_rv.errvel)
printf, fnum, 'Median Err   & ',mdnerr,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, ''
nobs = n_elements((*(*pstate).pcf).cf_rv.jd)
printf, fnum, '\# of observations   & ',nobs,' &    \\'
printf, fnum, ''
lastob = jul2cal((*(*pstate).pcf).cf_rv[nobs-1].jd +2.44d6, /date)
printf, fnum, 'Date of Last Obs.  & ',lastob,' &  & '
printf, fnum, 'Planet ID   & ',extitle,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, '\end{tabular}'
printf, fnum, '\end{center}'
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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
	restore, '/exolib/KFME/solarsystem.dat'
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
	a_pl=((period/365.2564d)^2*mstar/msun)^(1./3.)
	m_pl_earth = msini1
	inc = 89.9d
	ecc = planet[2].value
	
	k = mpf_K(a_pl, m_pl_earth/mearth, period, mstar/msun, inc, ecc)
	
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



printf, fnum, '\end{document}'

close, fnum
spawn, 'open '+fullnm
;stop
end; kfme_external_texpars.pro
