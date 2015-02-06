;***********************************************************************
; NAME: TRANSIT_PRINT
;																	   
; PURPOSE: This procedure is designed to print the information from about
;		ephemeris times both on the screen, and it will create an html 
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
;     c. Matt Giguere, Sunday, February 10, 2008		
;***********************************************************************
pro transit_print, p, Tp, e, om, k, $
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
 tdir = '~/Sites/tauceti/secure/transit/ephemerides/'
;endif else begin
; tdir = '~/Sites/tauceti/secure/transit/ephemerides/'
;endelse

if ~keyword_set(extitle) then begin
read, 'Please enter the name of the star: ', extitle
endif else filen = extitle

fullnm = tdir+extitle+time_stamp+'.html'

get_lun, fnum
xst = file_info(fullnm)
if ~(xst.exists) then spawn, 'touch '+ fullnm
openw, fnum, fullnm

print, 'time_stamp is: ', time_stamp

printf, fnum, '<HTML>'
printf, fnum, '<HEAD>'
printf, fnum, '<TITLE>'
printf, fnum, 'EPHEMERIS TIMES FOR ', extitle
printf, fnum, ''
printf, fnum, '</TITLE>'
printf, fnum, '<style type="text/css">'
printf, fnum, ''
printf, fnum, 'body {'
printf, fnum, '	font: normal 11px auto "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '	color: #4f6b72;'
printf, fnum, '	background: #E6EAE9;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'a {'
printf, fnum, '	color: #c75f3e;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, '#partable {'
printf, fnum, '	width: 400px;'
printf, fnum, '	padding: 0;'
printf, fnum, '	margin: 0;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, '#ephtable {'
printf, fnum, '	width: 1000px;'
printf, fnum, '	padding: 0;'
printf, fnum, '	margin: 0;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'captionp {'
printf, fnum, '	padding: 0 0 5px 0;'
printf, fnum, '	width: 400px;	 '
printf, fnum, '	font: italic 11px "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '	text-align: right;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'caption {'
printf, fnum, '	padding: 0 0 5px 0;'
printf, fnum, '	width: 650px;	 '
printf, fnum, '	font: italic 11px "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '	text-align: right;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'th {'
printf, fnum, '	font: bold 11px "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '	color: #4f6b72;'
printf, fnum, '	border-right: 1px solid #C1DAD7;'
printf, fnum, '	border-bottom: 1px solid #C1DAD7;'
printf, fnum, '	border-top: 1px solid #C1DAD7;'
printf, fnum, '	letter-spacing: 2px;'
printf, fnum, '	text-transform: uppercase;'
printf, fnum, '	text-align: left;'
printf, fnum, '	padding: 6px 6px 6px 12px;'
printf, fnum, '	background: #CAE8EA url(html_images/bg_header.jpg) no-repeat;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'th.nobg {'
printf, fnum, '	border-top: 0;'
printf, fnum, '	border-left: 0;'
printf, fnum, '	border-right: 1px solid #C1DAD7;'
printf, fnum, '	background: none;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'td {'
printf, fnum, '	border-right: 1px solid #C1DAD7;'
printf, fnum, '	border-bottom: 1px solid #C1DAD7;'
printf, fnum, '	background: #fff;'
printf, fnum, '	padding: 6px 6px 6px 12px;'
printf, fnum, '	color: #4f6b72;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, ''
printf, fnum, 'td.alt {'
printf, fnum, '	background: #F5FAFA;'
printf, fnum, '	color: #797268;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'th.spec {'
printf, fnum, '	border-left: 1px solid #C1DAD7;'
printf, fnum, '	border-top: 0;'
printf, fnum, '	background: #fff url(html_images/bullet1.gif) no-repeat;'
printf, fnum, '	font: bold 10px "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, 'th.specalt {'
printf, fnum, '	border-left: 1px solid #C1DAD7;'
printf, fnum, '	border-top: 0;'
printf, fnum, '	background: #f5fafa url(html_images/bullet2.gif) no-repeat;'
printf, fnum, '	font: bold 10px "Trebuchet MS", Verdana, Arial, Helvetica, sans-serif;'
printf, fnum, '	color: #797268;'
printf, fnum, '}'
printf, fnum, ''
printf, fnum, '</style>'
printf, fnum, '</HEAD>'
printf, fnum, ''
printf, fnum, '<BODY>'
printf, fnum, '<Center>'
printf, fnum, 'EPHEMERIS TIMES FOR ', extitle
printf, fnum, '<BR>'
printf, fnum, '<BR>'
printf, fnum, '<BR>'
printf, fnum, ''
printf, fnum, ''
printf, fnum, '<table id="partable" cellspacing="0" summary="Orbital ', $
		'Parameters for ', extitle, ' ">'
printf, fnum, '<captionp>Table 1: Orbital Parameters </captionp>'
printf, fnum, '  <tr>'
printf, fnum, ''
printf, fnum, '    <th scope="col" abbr="Parameter">Parameter</th>'
printf, fnum, '    <th scope="col" abbr="Value">Value</th>'
printf, fnum, '    <th scope="col" abbr="Value">1-sigma</th>'
printf, fnum, '    <th scope="col" abbr="Spacer"> </th>'
printf, fnum, '    <th scope="col" abbr="Parameter">Parameter</th>'
printf, fnum, '    <th scope="col" abbr="Value">Value</th>'
printf, fnum, '    <th scope="col" abbr="Value">1-sigma</th>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td>p (d)</td>'
printf, fnum, '    <td> ', p, ' </td>'
if keyword_set(p_unc) then printf, fnum, '    <td> ', p_unc, ' </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '    <td> Tp (d)</td>'
printf, fnum, '    <td> ', Tp, ' </td>'
if keyword_set(Tp_unc) then printf, fnum, '    <td> ', Tp_unc, ' </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">e</td>'
printf, fnum, '    <td class="alt"> ', e, ' </td>'
if keyword_set(e_unc) then printf, fnum, '    <td class="alt"> ', e_unc, ' </td>'
printf, fnum, '    <td class="alt"> </td>'
printf, fnum, '    <td class="alt">om (deg)</td>'
printf, fnum, '    <td class="alt"> ', om, ' </td>'
if keyword_set(om_unc) then printf, fnum, '    <td class="alt"> ', om_unc, ' </td>'
printf, fnum, ''
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td>k (m/s)</td>'
printf, fnum, '    <td> ', k, ' </td>'
if keyword_set(k_unc) then printf, fnum, '    <td> ', k_unc, ' </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '    <td>arel (AU)</td>'
printf, fnum, '    <td> ', arel/au, ' </td>'
if keyword_set(unc_arel) then begin 
printf, fnum, '    <td> ', unc_arel/au, ' </td>'
endif else printf, fnum, '    <td> --- </td>'
printf, fnum, '  </tr>'


printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">msini (M_JUP)</td>'
printf, fnum, '    <td class="alt"> ', msini, ' </td>'
if keyword_set(unc_msini) then begin
printf, fnum, '    <td class="alt"> ', unc_msini, ' </td>'
endif else begin
printf, fnum, '    <td class="alt"> ',class,' </td>'
endelse
printf, fnum, '    <td class="alt"> </td>'
printf, fnum, '    <td class="alt"> vsini (km/s) </td>'
printf, fnum, '    <td class="alt"> ',vsini, '</td>'
if keyword_set(uvsini) then begin
printf, fnum, '    <td class="alt"> ', uvsini, ' </td>'
endif else begin
printf, fnum, '    <td class="alt">  </td>'
endelse
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td >mstar (M_SUN)</td>'
printf, fnum, '    <td > ', mstar, ' </td>'
if keyword_set(unc_mstar) then begin
printf, fnum, '    <td > ', unc_mstar, ' </td>'
endif else begin
printf, fnum, '    <td > </td>'
endelse
printf, fnum, '    <td > </td>'
printf, fnum, '    <td > rstar (R_SUN) </td>'
printf, fnum, '    <td > ',rstar, '</td>'
if keyword_set(unc_rstsar) then begin
printf, fnum, '    <td > ', unc_rstar, ' </td>'
endif else begin
printf, fnum, '    <td>  </td>'
endelse
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">gamma </td>'
printf, fnum, '    <td class="alt"> ', gam, ' </td>'
if keyword_set(unc_gam) then begin
printf, fnum, '    <td class="alt"> ', unc_gam, ' </td>'
endif else begin
printf, fnum, '    <td class="alt">  </td>'
endelse
printf, fnum, '    <td class="alt"> </td>'
printf, fnum, '    <td class="alt"> dvdt </td>'
printf, fnum, '    <td class="alt"> ',dvdt, '</td>'
if keyword_set(unc_dvdt) then begin
printf, fnum, '    <td class="alt"> ', unc_dvdt, ' </td>'
endif else begin
printf, fnum, '    <td class="alt">  </td>'
endelse
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">comp </td>'
printf, fnum, '    <td class="alt"> ', comp, ' </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '    <td class="alt"> </td>'
printf, fnum, '    <td class="alt"> # of planets </td>'
printf, fnum, '    <td class="alt"> ',nplanets, '</td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td >STDDEV Ratio</td>'
printf, fnum, '    <td > ', stddvtrans_no, ' </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '    <td>  </td>'
printf, fnum, '    <td> RM Amp (m/s) </td>'
printf, fnum, '    <td> ',rmamp,' </td>'
printf, fnum, '    <td> ',unc_rmamp,'* </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">transit center (d)</td>'
printf, fnum, '    <td class="alt"> ', Tcen, ' </td>'
if keyword_set(transit_stdev) then printf, fnum, '    <td class="alt"> ', transit_stdev, ' </td>'
printf, fnum, '    <td class="alt"></td>'
printf, fnum, '    <td class="alt">secondary center (d)</td>'
printf, fnum, '    <td class="alt"> ', STcen, ' </td>'
if keyword_set(transit_st_stdev) then printf, fnum, '    <td class="alt"> ', transit_st_stdev, ' </td>'
printf, fnum, '  </tr>'

;this will print the transit duration:
if keyword_set(ting) then begin
 printf, fnum, '  <tr>'
 printf, fnum, '    <td>transit duration (hr)</td>'
 printf, fnum, '    <td> ', (tegr - ting) * 24d, ' </td>'
 if keyword_set(tdur_unc) then printf, fnum, '    <td> ', tdur_unc, ' </td>'
if ~keyword_set(tdur_unc) then printf, fnum, '<td> ',' </td>'
 printf, fnum, '    <td></td>'
 printf, fnum, '    <td>secondary duration (hr)</td>'
 printf, fnum, '    <td> ', (stegr - sting) * 24d, ' </td>'
 if keyword_set(stdur_unc) then printf, fnum, '    <td> ', stdur_unc, ' </td>'
if ~keyword_set(stdur_unc) then printf, fnum, '<td> ',' </td>'
 printf, fnum, '  </tr>'
endif;transit_duration

if keyword_set(tdepth) then begin
printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">transit depth (%)</td>'
printf, fnum, '    <td class="alt"> ', tdepth * 100., ' </td>'
if keyword_set(transit_stdev) then printf, fnum, '    <td class="alt"> ',' </td>'
printf, fnum, '    <td class="alt"></td>'
printf, fnum, '    <td class="alt">secondary depth (%)</td>'
printf, fnum, '    <td class="alt"> ', stdepth * 100., ' </td>'
if keyword_set(transit_st_stdev) then printf, fnum, '    <td class="alt"> ',' </td>'
printf, fnum, '  </tr>'
endif;KW(transit_depth)

if keyword_set(depthmmag) then begin
printf, fnum, '    <td>transit depth (mmag)</td>'
printf, fnum, '    <td> ', depthmmag, ' </td>'
if keyword_set(depthmmag) then printf, fnum, '    <td> </td>'
printf, fnum, '    <td></td>'
printf, fnum, '    <td>secondary depth (mmag)</td>'
printf, fnum, '    <td> ', depth_st_mmag, ' </td>'
if keyword_set(depth_st_mmag) then printf, fnum, '    <td> </td>'
printf, fnum, '  </tr>'
endif;KW(mmag depth)

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt">Transit Probability(%)</td>'
printf, fnum, '    <td class="alt"> ', prob_t * 100d, ' </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '    <td class="alt">Secondary Probability(%)</td>'
printf, fnum, '    <td class="alt"> ', prob_st * 100d, ' </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td>Chi Squared</td>'
printf, fnum, '    <td> ', chi, ' </td>'
printf, fnum, '    <td>  </td>'
printf, fnum, '    <td>  </td>'
printf, fnum, '    <td> RMS </td>'
printf, fnum, '    <td> ', rms, ' </td>'
printf, fnum, '    <td>  </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt"> # of Iterations </td>'
printf, fnum, '    <td class="alt"> ', iter, ' </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '    <td class="alt"># of Observations</td>'
printf, fnum, '    <td class="alt"> ', nobs, ' </td>'
printf, fnum, '    <td class="alt">  </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td > Date of Last Obs. </td>'
printf, fnum, '    <td > ', lastob, ' </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '    <td> Planet ID </td>'
printf, fnum, '    <td>', planetid, ' </td>'
printf, fnum, '    <td> </td>'
printf, fnum, '  </tr>'

printf, fnum, ''
printf, fnum, '</table>'
printf, fnum, ''
printf, fnum, '<BR>'
printf, fnum, '<BR>'
printf, fnum, '<BR>'
printf, fnum, ''



;**********************************************************************
;					LOOP TO MAKE UTC TABLE:
;**********************************************************************

printf, fnum, '<table id="ephtable" cellspacing="0" summary="The ephemeris for ',$
		extitle, ' (UTC)">'
printf, fnum, '<caption>Table 1: Ephemeris for ', extitle, ' (UTC) </caption>'
printf, fnum, '  <tr>'
printf, fnum, ''
printf, fnum, '    <th scope="col" abbr="Begin">Begin Window</th>'
printf, fnum, '    <th scope="col" abbr="Ingress">Ingress</th>'
printf, fnum, '    <th scope="col" abbr="Center">Central Transit</th>'
printf, fnum, '	   <th scope="col" abbr="Egress">Egress</th>'
printf, fnum, '	   <th scope="col" abbr="End">End Window</th>'
printf, fnum, '  </tr>'

N = 6
printf, fnum, '  <tr>'
printf, fnum, '    <td> ', jul2cal(double(ting - transit_stdev - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(ting - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tcen - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tegr - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tegr + transit_stdev - p)), ' </td>'
printf, fnum, '  </tr>'


for i=0.d, N-1.d do begin
printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(ting - transit_stdev + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(ting + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(tcen + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(tegr + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(tegr + transit_stdev + 2*i*p)), ' </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td> ', jul2cal(double(ting - transit_stdev + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(ting + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tcen + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tegr + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(tegr + transit_stdev + (2*i + 1)*p)), ' </td>'
printf, fnum, '  </tr>'
endfor ;i->N-1
printf, fnum, '</table>'
printf, fnum, ''

;**********************************************************************
;					LOOP TO MAKE SECONDARY TRANSIT UTC TABLE:
;**********************************************************************

printf, fnum, '<BR>'
printf, fnum, '<BR>'
printf, fnum, ''


printf, fnum, '<table id="ephtable" cellspacing="0" summary="The secondary ephemeris for ',$
		extitle, ' (UTC)">'
printf, fnum, '<caption>Table 2: Secondary Transit Ephemeris for ', extitle, ' (UTC) </caption>'
printf, fnum, '  <tr>'
printf, fnum, ''
printf, fnum, '    <th scope="col" abbr="Begin">Begin Window</th>'
printf, fnum, '    <th scope="col" abbr="Ingress">Ingress</th>'
printf, fnum, '    <th scope="col" abbr="Center">Secondary Center</th>'
printf, fnum, '	   <th scope="col" abbr="Egress">Egress</th>'
printf, fnum, '	   <th scope="col" abbr="End">End Window</th>'
printf, fnum, '  </tr>'

N = 6
printf, fnum, '  <tr>'
printf, fnum, '    <td> ', jul2cal(double(sting - transit_st_stdev - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(sting - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stcen - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stegr - p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stegr + transit_st_stdev - p)), ' </td>'
printf, fnum, '  </tr>'


for i=0.d, N-1.d do begin
printf, fnum, '  <tr>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(sting - transit_st_stdev + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(sting + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(stcen + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(stegr + 2*i*p)), ' </td>'
printf, fnum, '    <td class="alt"> ', jul2cal(double(stegr + transit_st_stdev + 2*i*p)), ' </td>'
printf, fnum, '  </tr>'

printf, fnum, '  <tr>'
printf, fnum, '    <td> ', jul2cal(double(sting - transit_st_stdev + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(sting + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stcen + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stegr + (2*i + 1)*p)), ' </td>'
printf, fnum, '    <td> ', jul2cal(double(stegr + transit_st_stdev + (2*i + 1)*p)), ' </td>'
printf, fnum, '  </tr>'
endfor ;i->N-1
printf, fnum, '</table>'
printf, fnum, ''



pngdir = 'pngs/'
png_file = pngdir + extitle + '_' + time_stamp + '.png'

printf, fnum, '</br></br>'
printf, fnum, '</br></br>'
printf, fnum, '</br></br>'

printf, fnum, '<img src = "',png_file, '"/>'

png_file = pngdir + extitle + '_orbit_' + time_stamp + '.png'

printf, fnum, '</br></br>'
printf, fnum, '</br></br>'
printf, fnum, '</br></br>'

printf, fnum, '<img src = "',png_file, '"/>'


printf, fnum, ''
printf, fnum, '</CENTER>'
printf, fnum, ''
printf, fnum, ''
printf, fnum, ''
printf, fnum, 'Previous Calculations: <br>'

oldvrsns = reverse(file_search('Sites/tauceti/secure/transit/ephemerides/'+extitle+'*'))

for v=0, nume(oldvrsns) do begin
  vrsn = strmid(oldvrsns[v], 41, strlen(oldvrsns[v])-1)
  thistime = strmid(vrsn, strlen(extitle), strlen(vrsn)-13)
  printf, fnum, '<a href="',vrsn,'">', thistime, '</a> <br>'
  
endfor;link to old versions.


printf, fnum, '</BODY>'
printf, fnum, '</HTML>'
printf, fnum, ''
printf, fnum, ''
printf, fnum, ''


close, fnum

spawn, 'open -a Safari '+ fullnm
end; transit_print.pro