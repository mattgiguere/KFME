;***********************************************************************
; NAME: TRANSIT_PRINT_TEX
;																	   
; PURPOSE: This procedure is designed to print the information from about
;		ephemeris both on the screen, and it will create a LaTeX 
;		file (and hopefully soon a PDF file) that I can send to Greg
;		Henry
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
pro transit_print_tex, p, Tp, e, om, k, $
prob_t, $
prob_st, $
tcen, $
stcen, $
a1sini = a1sini, $
arel = arel, $
unc_arel = unc_arel, $
chi = chi, $
class=class, $
comp = comp, $
depthmmag = depthmmag, $
depth_st_mmag = depth_st_mmag, $
dvdt = dvdt, $
unc_dvdt = unc_dvdt, $
e_unc = e_unc, $
extitle = extitle, $
gam = gam, $
unc_gam = unc_gam, $
iter = iter, $
k_unc = k_unc, $
lastob = lastob, $
msini = msini, $
unc_msini = unc_msini, $
mstar = mstar, $
unc_mstar = unc_mstar, $
nobs = nobs, $
nplanets = nplanets, $
om_unc = om_unc, $
p_unc = p_unc, $
planetid=planetid, $
rmamp = rmamp, $
unc_rmamp_unc = unc_rmamp, $
rms = rms, $
rstar = rstar, $
unc_rstar = unc_rstar, $
stddvtrans_no = stddvtrans_no, $
stdepth = stdepth, $
stdevwarn = stdevwarn, $
stdur_unc = stdur_unc, $
sting = sting, $
stegr = stegr, $
tdepth = tdepth, $
tdur_unc = tdur_unc, $
tegr = tegr, $
time_stamp = time_stamp, $
ting = ting, $
Tp_unc = Tp_unc, $
transit_stdev = transit_stdev, $
transit_st_stdev = transit_st_stdev, $
vsini = vsini, $
uvsini = uvsini

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

;Number of days to print ephemeris for:
ndays = 90


if ~keyword_set(p_unc) then p_unc=p
if ~keyword_set(k_unc) then k_unc=k
if ~keyword_set(e_unc) then e_unc=e
if ~keyword_set(om_unc) then om_unc=om
if ~keyword_set(Tp_unc) then Tp_unc=Tp
if ~keyword_set(chi) then chi=0
if ~keyword_set(rms) then rms = 0
if ~keyword_set(iter) then iter = 0
if ~keyword_set(nobs) then nobs = 0
if ~keyword_set(LastOb) then LastOb = 0
if ~keyword_set(stddvtrans_no) then stddvtrans_no = 0

;Calculate a1sini:
 psec = 24.d * 3600.d * p
 puncsec = 24.d * 3600.d * p_unc
 a1sini = k*psec*sqrt(1.-e^2)/(2.*!pi)   ;in meters
 

;Calculate the uncertainty in the semi major axis:
a1max = (k + k_unc)*(psec + puncsec)*sqrt(1.d - $
((e-e_unc) > 0.)^2.)/(2.d*!pi)

a1sini_unc = a1max - a1sini

;stop

;Calculate the mass:
 massfn = 4.*!pi^2*a1sini^3/(G*psec^2)/1.989d30 ;in Msun

if tp lt 2.4d6 then tp += 2.44d6
if tcen lt 2.4d6 then tcen += 2.44d6
if ting lt 2.4d6 then ting += 2.44d6
if tegr lt 2.4d6 then tegr += 2.44d6
if stcen lt 2.4d6 then stcen += 2.44d6
if sting lt 2.4d6 then sting += 2.44d6
if stegr lt 2.4d6 then stegr += 2.44d6
dum = jul2cal(tcen)

print, '---------------------------------------------------------------'
print, '         FINAL ORBITAL PARAMETERS FOR ', extitle
print, '---------------------------------------------------------------'
print, '       P = ', p
print, '       Tp = ', tp, ' JD = ', jul2cal(tp)
print, '       e = ', e
print, '       om = ', om, ' rad = ', om*!radeg, ' degrees'
print, '       k = ', k
if keyword_set(vst) then print, '       gam = ', gam
print, '       a1sini = ', a1sini, ' m = ', a1sini/AU, ' AU'
if keyword_set(vst) then print, '       Mass_Fn = ', massfn
if keyword_set(vst) then print, '       M2sini = ', strt(msini), $
	' M_JUP = ', strt(msini*mjup2earth), ' M_EARTH'
if keyword_set(vst) then print, '       N_obs = ', nobs

print, ''
print, 'PROBABILITY OF TRANSIT: ', strt(prob_t*100.d), ' %'
print, ''

print, '---------------------------------------------------------------'
print, '         EPHEMERIS (UTC) FOR ', extitle
print, '---------------------------------------------------------------'
print, '     BEGIN WINDOW      |          CENTER         |      ',$
'END WINDOW  '
N = 8.d
if keyword_set(transit_stdev) then begin
  print, jul2cal(double(tcen - transit_stdev - p)), ' | ', $
  jul2cal(double(tcen - p)),' | ', $
  jul2cal(double(tcen + transit_stdev - p))
  for i=0.d, N-1.d do begin
	print, jul2cal(double(tcen - transit_stdev + i*p)), ' | ', $
	jul2cal(double(tcen + i*p)),$
	' | ', jul2cal(double(tcen + transit_stdev + i*p))
  endfor ;i->N-1
endif else begin
  print, '                       | ', jul2cal(double(tcen - p)), ' | '
  for i=0.d, N-1.d do begin
	print, '                       | ', jul2cal(double(tcen + i*p)),' | '
  endfor ;i->N-1
endelse
print, '---------------------------------------------------------------'

print, ''

u2p = 8.d / 24.d
print, '---------------------------------------------------------------'
print, '         EPHEMERIS (PST) FOR ', extitle
print, '---------------------------------------------------------------'
print, '     BEGIN WINDOW        |          CENTER         |      ',$
'END WINDOW  '
N = 8.d
if keyword_set(transit_stdev) then begin
print, jul2cal(double(tcen - transit_stdev - p - u2p)), ' | ', $
jul2cal(double(tcen - p  - u2p)), ' | ', $
jul2cal(double(tcen + transit_stdev - p  - u2p))
for i=0.d, N-1.d do begin
print, jul2cal(double(tcen - transit_stdev + i*p  - u2p)), ' | ', $
jul2cal(double(tcen + i*p -  u2p)),$
' | ', jul2cal(double(tcen + transit_stdev + i*p  - u2p))
endfor ;i->N-1
endif else begin
print, '                         | ', $
jul2cal(double(tcen - p  - u2p)), ' | '
for i=0.d, N-1.d do begin
print, '                         | ', $
jul2cal(double(tcen + i*p -  u2p)),' | '
endfor ;i->N-1

endelse
print, '---------------------------------------------------------------'

if keyword_set(stdevwarn) then begin
print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
print, '   WARNING! THE TRANSIT WINDOW WAS CREATED ARTIFICIALLY       '
PRINT, '                     AS 0.125 DAYS                            '
print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
print, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
endif;stdevwarn
;**********************************************************************
;	PRINTING TO FILE...
;**********************************************************************
;spawn, 'networksetup -getcomputername', cname
;if cname ne 'ARGO' then begin
 tdir = '~/transit/PDFS/'
;endif else begin
; tdir = '~/Sites/tauceti/secure/transit/ephemerides/'
;endelse

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
if ~(xst.exists) then spawn, 'cp ~/transit/PDFS/deluxetable.sty '+ fulldir


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
printf, fnum, '\title{EPHEMERIS FOR '+extitle+comp+'}'
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
if ~keyword_set(p_unc) then p_unc='0'
printf, fnum, 'p (d)   & ',p,' & ',p_unc,' & '
if ~keyword_set(Tp_unc) then Tp_unc='0'
printf, fnum, 'Tp (d)   & ',Tp,' & ',Tp_unc,'   \\'
printf, fnum, ''
if ~keyword_set(e_unc) then e_unc='0'
printf, fnum, 'e   & ',e,' & ',e_unc,' & '
if ~keyword_set(om_unc) then om_unc='0'
printf, fnum, '$\omega$   & ',om,' & ',om_unc,'   \\'
printf, fnum, ''
printf, fnum, 'k(m/s)   & ',k,' & ',k_unc,' & '
printf, fnum, 'arel (AU)   & ',arel/au,' & ',unc_arel/au,'   \\'
printf, fnum, ''
printf, fnum, '\hline'
printf, fnum, ''
if ~keyword_set(unc_msini) then unc_msini='0'
printf, fnum, 'msini (M$_{JUP}$)   & ',msini,' &',unc_msini,'  & '
printf, fnum, 'msini (M$_{\oplus}$)   & ',msini*mjup2earth,' &    \\'
printf, fnum, ''
if ~keyword_set(unc_mstar) then unc_mstar=''
printf, fnum, 'M$_{\star}$ (M$_{\odot}$)   & ',mstar,' & ',unc_mstar,' & '
if ~keyword_set(unc_rstar) then unc_rstar=''
printf, fnum, 'R$_{\star}$ (R$_{\odot}$)   & ',rstar,' & ',unc_rstar,' \\'
printf, fnum, ''
if ~keyword_set(unc_gam) then unc_gam=''
printf, fnum, '$\gamma$   & ',gam,' & ',unc_gam,' & '
if ~keyword_set(unc_dvdt) then unc_dvdt=''
printf, fnum, 'dvdt   & ',dvdt,' & ',unc_dvdt,'   \\'
printf, fnum, ''
printf, fnum, '\hline'
printf, fnum, ''
printf, fnum, 'comp   & ',comp,' &  & '
printf, fnum, '\# of planets   & ',nplanets,' &    \\'
printf, fnum, ''
if ~keyword_set(uvsini) then uvsini=''
printf, fnum, 'vsini   & ',vsini,' & ',uvsini,' & '
printf, fnum, 'RM AMP (m/s)   & ',rmamp,' & ',unc_rmamp,'   \\'
printf, fnum, ''
printf, fnum, 'STDDEV Ratio   & ', stddvtrans_no, ' &  & '
printf, fnum, 'Transit Probability (\%)  & ',prob_t * 100d,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, ''
if ~keyword_set(transit_st_stdev) then transit_st_stdev=''
printf, fnum, 'Transit Center (d)  & ',Tcen,' & ',transit_stdev,' & '
 if ~keyword_set(tdur_unc) then tdur_unc=''
printf, fnum, 'Transit Duration (hr)   & ',(tegr - ting) * 24d,$
		' & ',tdur_unc,'   \\'
printf, fnum, ''
printf, fnum, 'Transit Depth (\%)  & ', tdepth * 100., ' &  & '
printf, fnum, 'Transit Depth (mmag)   & ',depthmmag,' &    \\'
printf, fnum, ''
printf, fnum, '$\chi^{2}_{R}$  & ',chi,' &  & '
printf, fnum, 'RMS   & ',rms,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, ''
printf, fnum, '\# of iterations  & ',iter,' &  & '
printf, fnum, '\# of observations   & ',nobs,' &    \\'
printf, fnum, ''
printf, fnum, 'Date of Last Obs.  & ',lastob,' &  & '
printf, fnum, 'Planet ID   & ',planetid,' &    \\'
printf, fnum, ''
printf, fnum, '\hline '
printf, fnum, '\end{tabular}'
printf, fnum, '\end{center}'
printf, fnum, ''
printf, fnum, ''



;**********************************************************************
;					LOOP TO MAKE UTC TABLE:
;**********************************************************************

printf, fnum, '\begin{center}'
printf, fnum, '\begin{sidewaystable}[!htbp]'
printf, fnum, '\begin{tabular}{c|ccc|c}'
printf, fnum, '	\hline \hline'
printf, fnum, '	& & ',strt(extitle),strt(comp),' \\'
printf, fnum, 'BEGIN WINDOW &  INGRESS    &   CENTRAL TRANSIT  & '
printf, fnum, 'EGRESS &  END WINDOW     \\'
printf, fnum, '	\hline \hline'


printf, fnum, jul2cal(double(ting - transit_stdev - p)), $
'   & ', jul2cal(double(ting - p)), $
' & ', jul2cal(double(tcen - p)), ' & '
printf, fnum,  jul2cal(double(tegr - p)), $
'   & ', jul2cal(double(tegr + transit_stdev - p)), '    \\'

printf, fnum, ''

N = 6

for i=0.d, N-1.d do begin
printf, fnum, jul2cal(double(ting - transit_stdev + 2*i*p)), $
'   & ', jul2cal(double(ting + 2*i*p)), $
' & ', jul2cal(double(tcen + 2*i*p)), ' & '
printf, fnum, jul2cal(double(tegr + 2*i*p)), $
'   & ', jul2cal(double(tegr + transit_stdev + 2*i*p)), '   \\'

printf, fnum, ''

printf, fnum, jul2cal(double(ting - transit_stdev + (2*i + 1)*p)), $
' & ', jul2cal(double(ting + (2*i + 1)*p)), $
' & ', jul2cal(double(tcen + (2*i + 1)*p)), ' & '
printf, fnum, jul2cal(double(tegr + (2*i + 1)*p)), $
'   & ', jul2cal(double(tegr + transit_stdev + (2*i + 1)*p)), '   \\'
printf, fnum, ''

endfor ;i->N-1

printf, fnum, ''
printf, fnum, ''
printf, fnum, ''
printf, fnum, '\hline'
printf, fnum, '\end{tabular}'
printf, fnum, '\end{sidewaystable}'
printf, fnum, '\end{center}'
printf, fnum, ''
printf, fnum, ''

printf, fnum, '\begin{figure}'
printf, fnum, '\epsfig{file=',extitle+'ek'+time_stamp+$
'.eps,width=0.95\linewidth,clip=}'
printf, fnum, '\caption{The eccentricity vs. amplitude resulting ',$
'from ',iter,' bootstrap MC realizations of the ',extitle,' system.}'
printf, fnum, '\label{fig',extitle,'MCek}'
printf, fnum, '\end{figure}'


printf, fnum, '\begin{figure}'
printf, fnum, '\epsfig{file=',extitle+'rvfit'+time_stamp+$
'.eps,width=0.95\linewidth,clip=}'
printf, fnum, '\caption{Phase-folded radial velocities for ',extitle,'.}'
printf, fnum, '\label{fig',extitle,'rvfit}'
printf, fnum, '\end{figure}'


printf, fnum, '\begin{figure}'
printf, fnum, '\epsfig{file=',extitle+'_pos'+time_stamp+$
'bw.eps,width=0.95\linewidth,clip=}'
printf, fnum, '\caption{A view of the ',extitle,comp,$
' planetary orbit as seen from the plane of the sky and ',$
'perndicular to the line of nodes. The direction',$
' of the observer is indicated in the bottom center of the',$
' illustration.}'
printf, fnum, '\label{fig',extitle,'pos}'
printf, fnum, '\end{figure}'



printf, fnum, '\end{document}'

close, fnum

end; transit_print_TeX.pro
