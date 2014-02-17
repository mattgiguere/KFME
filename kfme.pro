;***********************************************************************
; NAME: KFME
;																	   
; PURPOSE: The interface for multi-planet fitting
;																	   
; CATEGORY: KFME							   
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
; REQUISITES:
;	MPF_K: TO GET THE AMPLITUDE
;  MPF_RV_THVEL: TO GET THE THEORETICAL RADIAL VELOCITY CURVE
;  MPF_MPFIT_AST: TO GET THE THEORETICAL ASTROMETRY CURVE
;  MPF_MPFIT_RV: TO GENERATE THE THEORETICAL RADIAL VELOCITY CURVE FOR
;						MPFIT.PRO.
;																	   
; PROCEDURE:														   
;																	   
; EXAMPLE:			
;																	   
; MODIFICATION HISTORY:												   
;     c. Matt Giguere, May 28, 2009
;***********************************************************************
function kfme_create_par, pstate

 n_planets = (*(*pstate).pfunctargs).n_planets
 
; ; 7 parameters for each planet and 9 for the trend:
; n_parameters = 7.*n_planets + 9 + 3
 
 ; 5 parameters for each planet and 3 for the trend:
 n_parameters = 5.*n_planets + 5
 
 pararr=replicate({parname:'', value:0.d, fixed:0, $
					 limited:[0,0], limits:[0.d, 0.d], $
					 step:0., error:0.d},n_parameters)
 
 q = 0
 if (*pstate).fitplarr[0] then begin
  pararr[q] = (*pstate).pars.par1[0] & q++
  pararr[q] = (*pstate).pars.par1[1] & q++
  pararr[q] = (*pstate).pars.par1[2] & q++
  pararr[q] = (*pstate).pars.par1[3] & q++
  pararr[q] = (*pstate).pars.par1[4] & q++
 endif;FITPLANET1 CHECKED
 
 if (*pstate).fitplarr[1] then begin
  pararr[q] = (*pstate).pars.par2[0] & q++
  pararr[q] = (*pstate).pars.par2[1] & q++
  pararr[q] = (*pstate).pars.par2[2] & q++
  pararr[q] = (*pstate).pars.par2[3] & q++
  pararr[q] = (*pstate).pars.par2[4] & q++
 endif;FITPLANET2 CHECKED
 
 if (*pstate).fitplarr[2] then begin
  pararr[q] = (*pstate).pars.par3[0] & q++
  pararr[q] = (*pstate).pars.par3[1] & q++
  pararr[q] = (*pstate).pars.par3[2] & q++
  pararr[q] = (*pstate).pars.par3[3] & q++
  pararr[q] = (*pstate).pars.par3[4] & q++
 endif;FITPLANET3 CHECKED
 
 if (*pstate).fitplarr[3] then begin
  pararr[q] = (*pstate).pars.par4[0] & q++
  pararr[q] = (*pstate).pars.par4[1] & q++
  pararr[q] = (*pstate).pars.par4[2] & q++
  pararr[q] = (*pstate).pars.par4[3] & q++
  pararr[q] = (*pstate).pars.par4[4] & q++
 endif;FITPLANET4 CHECKED
 
 if (*pstate).fitplarr[4] then begin
  pararr[q] = (*pstate).pars.par5[0] & q++
  pararr[q] = (*pstate).pars.par5[1] & q++
  pararr[q] = (*pstate).pars.par5[2] & q++
  pararr[q] = (*pstate).pars.par5[3] & q++
  pararr[q] = (*pstate).pars.par5[4] & q++
 endif;FITPLANET5 CHECKED
 
 if (*pstate).fitplarr[5] then begin
  pararr[q] = (*pstate).pars.par6[0] & q++
  pararr[q] = (*pstate).pars.par6[1] & q++
  pararr[q] = (*pstate).pars.par6[2] & q++
  pararr[q] = (*pstate).pars.par6[3] & q++
  pararr[q] = (*pstate).pars.par6[4] & q++
 endif;FITPLANET6 CHECKED
 
 if (*pstate).fitplarr[6] then begin
  pararr[q] = (*pstate).pars.par7[0] & q++
  pararr[q] = (*pstate).pars.par7[1] & q++
  pararr[q] = (*pstate).pars.par7[2] & q++
  pararr[q] = (*pstate).pars.par7[3] & q++
  pararr[q] = (*pstate).pars.par7[4] & q++
 endif;FITPLANET7 CHECKED
 
 ;Now for the trends and offsets
 pararr[5.*n_planets: n_parameters-1] = $
	 (*pstate).pars.par1[5:9]
 
 return, pararr
end;kfme_create_par.pro

pro kfme_retrieve_par, pstate, pararr, perror

 n_planets = (*(*pstate).pfunctargs).n_planets
 
 ; 5 parameters for each planet and 3 for the trend:
 n_parameters = 5.*n_planets + 5
 
 
 q = 0
 if (*pstate).fitplarr[0] then begin
  (*pstate).pars.par1[0] = pararr[q] & q++
  (*pstate).pars.par1[1] = pararr[q] & q++
  (*pstate).pars.par1[2] = pararr[q] & q++
  (*pstate).pars.par1[3] = pararr[q] & q++
  (*pstate).pars.par1[4] = pararr[q] & q++
 endif;FITPLANET1 CHECKED
 
 if (*pstate).fitplarr[1] then begin
  (*pstate).pars.par2[0] = pararr[q] & q++
  (*pstate).pars.par2[1] = pararr[q] & q++
  (*pstate).pars.par2[2] = pararr[q] & q++
  (*pstate).pars.par2[3] = pararr[q] & q++
  (*pstate).pars.par2[4] = pararr[q] & q++
 endif;FITPLANET2 CHECKED
 
 if (*pstate).fitplarr[2] then begin
  (*pstate).pars.par3[0] = pararr[q] & q++
  (*pstate).pars.par3[1] = pararr[q] & q++
  (*pstate).pars.par3[2] = pararr[q] & q++
  (*pstate).pars.par3[3] = pararr[q] & q++
  (*pstate).pars.par3[4] = pararr[q] & q++
 endif;FITPLANET3 CHECKED
 
 if (*pstate).fitplarr[3] then begin
  (*pstate).pars.par4[0] = pararr[q] & q++
  (*pstate).pars.par4[1] = pararr[q] & q++
  (*pstate).pars.par4[2] = pararr[q] & q++
  (*pstate).pars.par4[3] = pararr[q] & q++
  (*pstate).pars.par4[4] = pararr[q] & q++
 endif;FITPLANET4 CHECKED
 
 if (*pstate).fitplarr[4] then begin
  (*pstate).pars.par5[0] = pararr[q] & q++
  (*pstate).pars.par5[1] = pararr[q] & q++
  (*pstate).pars.par5[2] = pararr[q] & q++
  (*pstate).pars.par5[3] = pararr[q] & q++
  (*pstate).pars.par5[4] = pararr[q] & q++
 endif;FITPLANET5 CHECKED
 
 if (*pstate).fitplarr[5] then begin
  (*pstate).pars.par6[0] = pararr[q] & q++
  (*pstate).pars.par6[1] = pararr[q] & q++
  (*pstate).pars.par6[2] = pararr[q] & q++
  (*pstate).pars.par6[3] = pararr[q] & q++
  (*pstate).pars.par6[4] = pararr[q] & q++
 endif;FITPLANET6 CHECKED
 
 if (*pstate).fitplarr[6] then begin
  (*pstate).pars.par7[0] = pararr[q] & q++
  (*pstate).pars.par7[1] = pararr[q] & q++
  (*pstate).pars.par7[2] = pararr[q] & q++
  (*pstate).pars.par7[3] = pararr[q] & q++
  (*pstate).pars.par7[4] = pararr[q] & q++
 endif;FITPLANET7 CHECKED
 
 
 ;Now for the trends and offsets
 (*pstate).pars.par1[5:9] = $
	 pararr[5.*n_planets: n_parameters-1]


 
 if n_params() gt 2 then begin
 ;This will tack on the uncertainties for each parameter:
 
 q = 0
 if (*pstate).fitplarr[0] then begin
  (*pstate).pars.par1[0].error = perror[q] & q++
  (*pstate).pars.par1[1].error = perror[q] & q++
  (*pstate).pars.par1[2].error = perror[q] & q++
  (*pstate).pars.par1[3].error = perror[q] & q++
  (*pstate).pars.par1[4].error = perror[q] & q++
 endif;FITPLANET1 CHECKED
 
 if (*pstate).fitplarr[1] then begin
  (*pstate).pars.par2[0].error = perror[q] & q++
  (*pstate).pars.par2[1].error = perror[q] & q++
  (*pstate).pars.par2[2].error = perror[q] & q++
  (*pstate).pars.par2[3].error = perror[q] & q++
  (*pstate).pars.par2[4].error = perror[q] & q++
 endif;FITPLANET2 CHECKED
 
 if (*pstate).fitplarr[2] then begin
  (*pstate).pars.par3[0].error = perror[q] & q++
  (*pstate).pars.par3[1].error = perror[q] & q++
  (*pstate).pars.par3[2].error = perror[q] & q++
  (*pstate).pars.par3[3].error = perror[q] & q++
  (*pstate).pars.par3[4].error = perror[q] & q++
 endif;FITPLANET3 CHECKED
 
 if (*pstate).fitplarr[3] then begin
  (*pstate).pars.par4[0].error = perror[q] & q++
  (*pstate).pars.par4[1].error = perror[q] & q++
  (*pstate).pars.par4[2].error = perror[q] & q++
  (*pstate).pars.par4[3].error = perror[q] & q++
  (*pstate).pars.par4[4].error = perror[q] & q++
 endif;FITPLANET4 CHECKED
 
 if (*pstate).fitplarr[4] then begin
  (*pstate).pars.par5[0].error = perror[q] & q++
  (*pstate).pars.par5[1].error = perror[q] & q++
  (*pstate).pars.par5[2].error = perror[q] & q++
  (*pstate).pars.par5[3].error = perror[q] & q++
  (*pstate).pars.par5[4].error = perror[q] & q++
 endif;FITPLANET5 CHECKED
 
 if (*pstate).fitplarr[5] then begin
  (*pstate).pars.par6[0].error = perror[q] & q++
  (*pstate).pars.par6[1].error = perror[q] & q++
  (*pstate).pars.par6[2].error = perror[q] & q++
  (*pstate).pars.par6[3].error = perror[q] & q++
  (*pstate).pars.par6[4].error = perror[q] & q++
 endif;FITPLANET6 CHECKED
 
 if (*pstate).fitplarr[6] then begin
  (*pstate).pars.par7[0].error = perror[q] & q++
  (*pstate).pars.par7[1].error = perror[q] & q++
  (*pstate).pars.par7[2].error = perror[q] & q++
  (*pstate).pars.par7[3].error = perror[q] & q++
  (*pstate).pars.par7[4].error = perror[q] & q++
 endif;FITPLANET7 CHECKED
 
 
 ;Now for the trends and offsets
 (*pstate).pars.par1[5:9].error = $
	 perror[5.*n_planets: n_parameters-1]
 endif ;perror passed in


end;kfme_retrieve_par.pro

pro kfme_dofit, pstate
  ;This first procedure is what plots the data and overplots
  ;the initial guess blue lines. 
  
  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
   

  !p.multi=[0,1,1]
  !p.charsize = 2.
  !p.thick = 2.
  !p.font=-1

   pararr = kfme_create_par(pstate)

   ;Take care of the zoom and scroll:
   rvra = where( ( (*(*pstate).pcf).cf_rv.jd ge (*pstate).xmin) $
            AND ( (*(*pstate).pcf).cf_rv.jd le (*pstate).xmax) )

   ;rvra = dindgen(n_elements((*(*pstate).pcf).cf_rv.jd))
   rv_arr =      (*(*pstate).pcf).cf_rv[rvra].jd
   		
   rvran = max(rv_arr)-min(rv_arr)
   
	fitrv = rv_mpfit_rv((*(*pstate).pcf).cf_rv.jd, pararr.value, $
		_EXTRA = (*(*pstate).pfunctargs))
				
	  ;The fit for JUST RV:
     fitobs = rv_arr
	  fity = fitrv[rvra]
	  fitdat = (*(*pstate).pcf).cf_rv[rvra].mnvel
	  
     errarr = (*(*pstate).pcf).cf_rv.errvel
   			 
  ;Plot the data:
  result = label_date(date_format=['%M %Y'], offset=2.44d6)
  
  if (*pstate).psplot then begin
  	 !x.margin=[12,5]
     !p.font=1
     if ~file_test('~/kfme_output', /directory) then spawn, 'mkdir ~/kfme_output'
     if ~file_test('~/kfme_output/plots', /directory) then spawn, 'mkdir ~/kfme_output/plots'
     
	 filen=nextnameeps('~/kfme_output/plots/kfmeplot', '')
	 ps_open, filen, /encaps, /color
  endif;postscript
  
  xrng = max(fitobs) - min(fitobs)
  
  ;use user-defined y range if specified
  if (*pstate).yminmax then begin
	 yrng = [(*pstate).yminval, (*pstate).ymaxval]
  endif else begin
	 yrng = [min(fitdat), max(fitdat)]
  endelse
  
    if (*pstate).titleflag then begin
    plttitle = (*(*pstate).pfunctargs).extitle
    endif else plttitle=''
  

  if n_elements(fitobs) gt 1 then begin
  plot, fitobs, fitdat, $
	 linestyle=(*pstate).linestyle, $
	 color = 0, psym=(*pstate).psym*(*pstate).connect, $
	 XTickFormat='label_date', xminor=4, XTICKINTERVAL = rvran/5, $
	 xticks = 4, $
	 xtitle = 'Time of Observation', $
	 ytitl='Radial Velocity [m s!u-1!n]', $
	 title=plttitle, $
	 ;xran = minmax(fitobs), $
	 xran  = [min(fitobs) - 0.02d * xrng, $
	 max(fitobs) + 0.02d * xrng], $
	 /xsty, ysty=(*pstate).yminmax, yrange = yrng
	 ;/xstyle, xran = [min(fitobs), max(fitobs)], $
  
  if (*pstate).psym eq 8 then symsize = 1 else symsize = 1
  if ~keyword_set((*pstate).tfine) then begin
	 oplot, fitobs, fity, color = 90, linestyle = 4, $
		psym=(*pstate).psym*(*pstate).connect, $
		symsize = symsize
  endif
  
  if (*pstate).togerr then oploterr, fitobs, fitdat, errarr, 8
  
  ;CHECKED 2010.06.28 BECAUSE OF HI CHI SQ VALUES, BUT
  ;IT IS CORRECT. I'M USING THE EQUATION THAT CAN BE
  ;FOUND HERE:
  ;http://en.wikipedia.org/wiki/Goodness_of_fit
  ;(*PSTATE).NDOF IS TAKING THE "-1" INTO ACCOUNT
  ;ALREADY. 2 OTHER FREE PARAMETERS COME FROM THE
  ;DIFFERENT DEWARDS WHICH ARE ALSO BEING FIT FOR. 
  ;ACTUALLY, FOR THE KECK OBSERVATIONS OF 163607, 
  ;ONLY 1 DEWAR WAS USED, SO THIS SHOULDN'T BE FIT
  ;FOR.
   
  chisq = total(((fitdat-fity)/errarr)^2)/$
	 (n_elements(fitobs) - (*pstate).ndof)
;stop	 
  ;print, '# fitobs: ', n_elements(fitobs)
  ;print, 'ndof: ', (*pstate).ndof
	 
  if n_elements(fitobs) le (*pstate).ndof then begin
  xyouts, 0.6, 0.2, 'N <= n !!', $
  /normal, color=240, charthick = 2
  print, '******************************************'
  print, '             !!WARNING!!                 '
  print, 'The number of data points is less than or'
  print, 'equal to the number of degrees of freedom!'
  print, '******************************************'
  ;stop
  endif
  
  ;rmsresid = stddev(fitdat-fity)
  nfree = (*pstate).ndof
  rmsresid = sqrt(total((fitdat - fity)^2)/(n_elements(fitobs)-nfree))


if ~(*pstate).psplot then begin

  xyouts, .1, .01, 'real '+Greek('chi')+'!d'+Greek('nu')+$
  '!u2!n'+': '+strt(chisq, f='(F9.2)'), /norm
  ;rmsresid = stddev(fitdat-fity)
  xyouts, .4, .01, 'RMS: '+strt(rmsresid, f='(F9.2)'), /norm
  xyouts, .7, .01, 'STDDEV RV: '+$
	 strt(stddev((*(*pstate).pcf).cf_rv.mnvel), f='(F9.2)'), /norm
;	 stop
endif


;OVERPLOT TFINE IF THE BOX IS CHECKED:
if (*pstate).tfine then begin
	minmaxels = minmax((*(*pstate).pcf).cf_rv.jd)

	tfine = (minmaxels[1] - minmaxels[0])*dindgen(1d4+1d)/1d4 + $
		minmaxels[0]
		
  tfitrv = rv_mpfit_rv(tfine, pararr.value, $
			  _EXTRA = (*(*pstate).pfunctargs))

  oplot, tfine, tfitrv, color = 80, linestyle = 0, $
  		thick=4.

  oplot, fitobs, fitdat, $
	 color = 0, psym=(*pstate).psym*(*pstate).connect

  if (*pstate).togerr then oploterr, fitobs, fitdat, errarr, 8

endif

 ;FINALLY TO SAVE THE RESIDUALS FOR BOTH RV & ASTROMETRY 
 ;TO THEIR RIGHTFUL PLACE:
 
 (*pstate).chisq = chisq
 (*pstate).rmsresid = rmsresid
 (*(*pstate).pcf_resid) = (*(*pstate).pcf)
 
  (*(*pstate).pcf_resid).cf_rv.mnvel = $
 (*(*pstate).pcf).cf_rv.mnvel - fitrv

 if (*pstate).multith then kfme_multiline, pstate
 
  if (*pstate).psplot then begin
	 ps_close
	 spawn, 'open '+filen+'.eps'
	  widget_control, (*pstate).controlbar.psplotbutton, set_button = 0
	 (*pstate).psplot = 0
  endif

  endif else begin
  ;fitobs is less than 2 (can't plot)
  plot, findgen(50), /nodata
  xyouts, 0.5, 0.5, 'Decrease zoom level', /normal
  endelse


end;kfme_dofit.pro

pro kfme_multiline, pstate


   pararr = kfme_create_par(pstate)
   n_plnt = (*(*pstate).pfunctargs).n_planets
   n_parameters = 5.*n_plnt + 5
   times = (*(*pstate).pcf).cf_rv.jd

	minmaxels = minmax(times)
	tfres = 1d3
	tfine = (minmaxels[1] - minmaxels[0])*dindgen(tfres+1d)/tfres + $
		minmaxels[0]
   
   ;OVERPLOT TFINE IF THE BOX IS CHECKED:
if (*pstate).tfine then times = tfine


   
   ;Take care of the zoom and scroll:
	rvra = where( ( times $
                ge (*pstate).xmin) $
            AND ( times $
					 le (*pstate).xmax) )

   rv_arr = times[rvra]
   


 loadct, 39, /silent
 plnts_chckd = where((*pstate).fitplarr eq 1)
 q = 0
;Now cycle through the planets:   
for zz=0, n_plnt-1 do begin
 
 multipar = dblarr(10)*0d
 case plnts_chckd[zz] of
  0: begin
		clr = 49
		multipar[0:4] = pararr[0:4].value
		multipar[5:9] = pararr[5.*n_plnt: n_parameters-1].value
		q += 5
	  end
  1: begin
		clr = 43
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
  2: begin
		clr = 37.5
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
  3: begin
		clr = 36
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
  4: begin
		clr = 29
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
  5: begin
		clr = 11
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
  6: begin
		clr = 5
		multipar[0:4] = pararr[q:q + 4].value
		q+=5
	  end
 endcase

!p.background = 255
!p.color = clr*5

   newfunc = (*(*pstate).pfunctargs)
   newfunc.n_planets = 1
	fitrv = rv_mpfit_rv(times, multipar, $
		_EXTRA = newfunc)
				

	  ;The fit for JUST RV:
     fitobs = rv_arr
	  fity = fitrv[rvra]

 ;Make the background black outline:
 oplot, fitobs, fity, $
 	psym = (*pstate).psym * (*pstate).connect, $
 	linestyle = (*pstate).linestyle, color = 0, $
 	symsize = 2.0
 	
 ;Overplot the color dot:
 oplot, fitobs, fity, $
 	psym = (*pstate).psym * (*pstate).connect, $
 	linestyle = (*pstate).linestyle, symsize = 1.0
 	
endfor;cycle through planets

  !p.color = 0

end ;kfme_multiline.pro

pro kfme_update_fields, pstate

  ;CONTROLBAR:
  widget_control, (*pstate).controlbar.jitterbox, $
  set_value = strt((*pstate).jitternum)
  
  
  ;UPDATE THE TEXT AND SLIDER FIELDS WITH NEW PARS:
  ;PLANET 1:
  newpar = (*pstate).pars.par1[0].value
  widget_control, (*pstate).txtflds.pertextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl1, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl1, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par1[1].value
  widget_control, (*pstate).txtflds.masstextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl1, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par1[2].value
  widget_control, (*pstate).txtflds.ecctextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl1, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par1[3].value
  widget_control, (*pstate).txtflds.lomtextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl1, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par1[4].value
  widget_control, (*pstate).txtflds.toptextpl1, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par1[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par1[0].value

  newpar = (*pstate).pars.par1[5].value
  widget_control, (*pstate).txtflds.rvotextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderrvopl1, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par1[6].value
  widget_control, (*pstate).txtflds.srvtextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidersrvpl1, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par1[7].value
  widget_control, (*pstate).txtflds.crvtextpl1, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidercrvpl1, $
  set_value=double(newpar)


  ;PLANET 2:
  
  newpar = (*pstate).pars.par2[0].value
  widget_control, (*pstate).txtflds.pertextpl2, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl2, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl2, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par2[1].value
  widget_control, (*pstate).txtflds.masstextpl2, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl2, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par2[2].value
  widget_control, (*pstate).txtflds.ecctextpl2, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl2, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par2[3].value
  widget_control, (*pstate).txtflds.lomtextpl2, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl2, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par2[4].value
  widget_control, (*pstate).txtflds.toptextpl2, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par2[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par2[0].value

  ;PLANET 3:
  
  newpar = (*pstate).pars.par3[0].value
  widget_control, (*pstate).txtflds.pertextpl3, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl3, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl3, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par3[1].value
  widget_control, (*pstate).txtflds.masstextpl3, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl3, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par3[2].value
  widget_control, (*pstate).txtflds.ecctextpl3, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl3, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par3[3].value
  widget_control, (*pstate).txtflds.lomtextpl3, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl3, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par3[4].value
  widget_control, (*pstate).txtflds.toptextpl3, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl3, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par3[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par3[0].value

  ;PLANET 4:
  
  newpar = (*pstate).pars.par4[0].value
  widget_control, (*pstate).txtflds.pertextpl4, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl4, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl4, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par4[1].value
  widget_control, (*pstate).txtflds.masstextpl4, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl4, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par4[2].value
  widget_control, (*pstate).txtflds.ecctextpl4, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl4, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par4[3].value
  widget_control, (*pstate).txtflds.lomtextpl4, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl4, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par4[4].value
  widget_control, (*pstate).txtflds.toptextpl4, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl4, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par4[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par4[0].value

  ;PLANET 5:
  
  newpar = (*pstate).pars.par5[0].value
  widget_control, (*pstate).txtflds.pertextpl5, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl5, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl5, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par5[1].value
  widget_control, (*pstate).txtflds.masstextpl5, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl5, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par5[2].value
  widget_control, (*pstate).txtflds.ecctextpl5, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl5, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par5[3].value
  widget_control, (*pstate).txtflds.lomtextpl5, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl5, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par5[4].value
  widget_control, (*pstate).txtflds.toptextpl5, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl5, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par5[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par5[0].value

  ;PLANET 6:
  
  newpar = (*pstate).pars.par6[0].value
  widget_control, (*pstate).txtflds.pertextpl6, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl6, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl6, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par6[1].value
  widget_control, (*pstate).txtflds.masstextpl6, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl6, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par6[2].value
  widget_control, (*pstate).txtflds.ecctextpl6, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl6, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par6[3].value
  widget_control, (*pstate).txtflds.lomtextpl6, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl6, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par6[4].value
  widget_control, (*pstate).txtflds.toptextpl6, $
  set_value=strt(newpar)

  widget_control, (*pstate).sliderflds.slidertoppl6, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par6[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par6[0].value

  ;PLANET 7:
  
  newpar = (*pstate).pars.par7[0].value
  widget_control, (*pstate).txtflds.pertextpl7, $
  set_value=strt(newpar)
  widget_control, (*pstate).txtflds.pertextypl7, $
  set_value=strt(newpar/365.2564)
  widget_control, (*pstate).sliderflds.sliderperpl7, $
  set_value=double(newpar)

  newpar = (*pstate).pars.par7[1].value
  widget_control, (*pstate).txtflds.masstextpl7, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidermasspl7, $
  set_value=double(newpar)*100.

  newpar = (*pstate).pars.par7[2].value
  widget_control, (*pstate).txtflds.ecctextpl7, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.slidereccpl7, $
  set_value=double(newpar*1000.)

  newpar = (*pstate).pars.par7[3].value
  widget_control, (*pstate).txtflds.lomtextpl7, $
  set_value=strt(newpar)
  widget_control, (*pstate).sliderflds.sliderlompl7, $
  set_value=double(newpar)
  
  newpar = (*pstate).pars.par7[4].value
  widget_control, (*pstate).txtflds.toptextpl7, $
  set_value=strt(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl7, $
  set_value=double(newpar), $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par7[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par7[0].value

  widget_control, (*pstate).fitplbttns.fitpar1bttn, $
  	set_button = (*pstate).fitplarr[0], /update

  widget_control, (*pstate).fitplbttns.fitpar2bttn, $
  	set_button = (*pstate).fitplarr[1], /update

  widget_control, (*pstate).fitplbttns.fitpar3bttn, $
  	set_button = (*pstate).fitplarr[2], /update

  widget_control, (*pstate).fitplbttns.fitpar4bttn, $
  	set_button = (*pstate).fitplarr[3], /update

  widget_control, (*pstate).fitplbttns.fitpar5bttn, $
  	set_button = (*pstate).fitplarr[4], /update

  widget_control, (*pstate).fitplbttns.fitpar6bttn, $
  	set_button = (*pstate).fitplarr[5], /update

  widget_control, (*pstate).fitplbttns.fitpar7bttn, $
  	set_button = (*pstate).fitplarr[6], /update

;  widget_control, (*pstate).controlbar.datname, $
;  	set_value = (*(*pstate).pcfname), /update

  widget_control, (*pstate).controlbar.zoomslide, $
  	set_value = (*pstate).zoomplot, /update

  widget_control, (*pstate).controlbar.scrollslide, $
  	set_value = (*pstate).scroll, /update

;  widget_control, (*pstate).controlbar.zoomrvslide, $
;  	set_value = (*pstate).zoomrv, /update

;  widget_control, (*pstate).controlbar.tiebutton, $
;  	set_button = (*pstate).tiemi, /update

  widget_control, (*pstate).controlbar.multithbutton, $
  	set_button = (*pstate).multith, /update

  widget_control, (*pstate).controlbar.togerrbutton, $
  	set_button = (*pstate).togerr, /update

  if (*pstate).connect eq 1 then cnct = 1 else cnct = 0
  widget_control, (*pstate).controlbar.connectbutton, $
  	set_button = cnct, /update


;Replot the data in the draw widget with the "do" routine:
 kfme_dofit, pstate

end;kfme_update_fields.pro

pro kfme_resize, event
  ;This procedure will resize the draw (plot) window when the 
  ;top-level base (tlb widget, which is the whole x-window) is
  ;resized.
  
  widget_control, event.top, get_uvalue=pstate
  dum = where(tag_names(event) eq 'TAB', notabsct)
  if ~notabsct then begin
	 ;To properly resize a draw widget, the geometry of the bases
	 ;surrounding it must be determined and subtracted.
	 toprowg = widget_info((*pstate).toprow, /geometry)
	 
	 botrowg = widget_info((*pstate).botrow, /geometry)
	 
	 controlg = widget_info((*pstate).controlbase, /geometry)
	 tlbg = widget_info(event.top, /geometry)
	 
	 ;Subtract out the xpadding of the tlb for the xsize:
	 newx = event.x - 2*tlbg.xpad - controlg.scr_xsize $
	  - 2*controlg.xpad - 2*toprowg.xpad - 17
	 
	 ;Subtract the ysize and ypadding of the bases:
	 newy = event.y - botrowg.scr_ysize $
	  - 2*tlbg.ypad - 2*tlbg.space $
	  - 2*botrowg.ypad - 2*botrowg.space $
	  - 2*toprowg.ypad - 2*toprowg.space - 36
	 
	 ;Now adjust the size of the planet bar in the bottom:
	 
	 ;Resize the draw widget:
	 widget_control, (*pstate).draw, xsize=newx, ysize=newy
	 
	 ;Resize the planet bar:
	 widget_control, (*pstate).draw, xsize=planx, ysize=plany
	 
	 ;Call the "do" routine to replot the graphics:
	 kfme_dofit, pstate
  endif;no tabs

end;kfme_resize.pro

pro kfme_par, event
print, 'event.value is: ', event.value

end ;kfme_par.pro

pro kfme_cleanup, tlb
 widget_control, tlb, get_uvalue=pstate
 ptr_free, (*pstate).pcf
 ptr_free, pstate
end ;kfme_cleanup.pro



 ;**************************************************************
 ;**************************************************************
 ;           CONTROLBASE PROCEDURES
 ;**************************************************************
 ; These procedures are associated with the widgets in the 
 ; far-right column (the controlbase) in descending order.
 ;**************************************************************
pro kfme_openlickvst, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate

  ;User select the name of the residuals to restore:
  newname = dialog_pickfile(/read, filter='*.dat', title= $
  'Enter Filename to Restore...', $
  path='/mir1/vel/')
 
  if newname ne '' then begin ;the CASE where CANCEL is clicked
  restore, newname

  cf3 = cf1 ;THIS EXCLUDES PREFIX DATA. 
  ;DO NOT RESTORE CF1 ON KECK!!! (CF 1/3 ONLY MATTERS BEFORE 2004)
  
  ;x = where(cf3.errvel lt 1.5*median(cf3.errvel), errct)
  ;cf3 = cf3[x]
  
  velplot, cf3, '', 4./24., dates, speed, errv, cai, nav, bincf, /noplot
  cf3=bincf
  loadct, 39, /silent
  
  cf = create_struct('cf_ast', (*(*pstate).pcf).cf_ast, $
                     'cf_rv', cf3, $
                     'm_star', 1d, $
                     'plx', (*(*pstate).pcf).plx, $
                     'prpr', (*(*pstate).pcf).prpr, $
                     'coords', (*(*pstate).pcf).coords, $
                     'midtime', 0d, $max(cf3.jd) - min(cf3.jd), $
                     'time_offset', 0d);(*(*pstate).pcf).time_offset)
  (*(*pstate).pcf) = cf
  (*(*pstate).pcf).cf_ast.jd *=0d
  (*pstate).xmin = min(cf3.jd)
  (*pstate).xmax = max(cf3.jd)
  (*(*pstate).pcfname) = newname
  
  dew24 = where(cf3.dewar eq 24)
  ;pdew24 = ptr_new(dew24, /no_copy, /allocate)
  dew39 = where(cf3.dewar eq 39)
  ;pdew39 = ptr_new(dew39, /no_copy, /allocate)

  (*(*(*pstate).pfunctargs).dew24) = dew24
  (*(*(*pstate).pfunctargs).dew39) = dew39

  endif;IF CANCEL NOT CLICKED, DO THE ABOBE
  
  ;Update all the fields and plot:    
  kfme_update_fields, pstate

  ;Save the pointer to the state structure:
  widget_control, event.top, set_uvalue=pstate

  print, 'File '+newname+' Restored.'
  
end ;kfme_openlickvst.pro

pro kfme_openkeckvst, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate

  ;User select the name of the residuals to restore:
  newname = dialog_pickfile(/read, filter='**.dat', title= $
  'Enter Filename to Restore...', $
  path=(*pstate).datadir)
  
kfme_restore_keck, newname, pstate

end ;kfme_openkeckvst.pro


pro kfme_n2ktargs, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
 fn = '/mir3/vel/vst'+(*pstate).n2ktargs[event.index]+'.dat'
 print, 'filename is: ', fn
 ;stop
kfme_restore_keck, fn, pstate
 
end;kfme_n2ktargs.pro

pro kfme_restore_keck, newname, pstate

  if newname ne '' then begin ;the CASE where CANCEL is clicked
   
  restore, newname

  ;x = where(cf3.errvel lt 2.5*median(cf3.errvel), errct)
  ;cf3 = cf3[x]
  printjds, cf3
  
  print, '# of Observations: ', n_elements(cf3)
  ;stop
  ;velplot, cf3, '', 4./24., dates, speed, errv, cai, nav, bincf, /noplot
  ;cf3=bincf
  ;stop
  ;if max(cf3.jd) lt 2.44d6 then cf3.jd += 2.44d6
	loadct, 39, /silent
	!p = (*pstate).p_orig
  
  cf = create_struct('cf_ast', (*(*pstate).pcf).cf_ast, $
                     'cf_rv', cf3, $
                     'm_star', 1d, $
                     'plx', (*(*pstate).pcf).plx, $
                     'prpr', (*(*pstate).pcf).prpr, $
                     'coords', (*(*pstate).pcf).coords, $
                     'midtime', 0d, $max(cf3.jd) - min(cf3.jd), $
                     'time_offset', 0d);(*(*pstate).pcf).time_offset)
  cf.cf_rv.mnvel = cf3.mnvel-median(cf3.mnvel)
  (*(*pstate).pcf) = cf
  (*(*pstate).pcf).cf_ast.jd *=0d
  (*pstate).xmin = min(cf3.jd)
  (*pstate).xmax = max(cf3.jd)
  (*(*pstate).pcfname) = newname
  print, '# of Observations: ', n_elements(cf3)
  printjds, cf3
  
  dew24 = where(cf3.dewar eq 24)
  ;pdew24 = ptr_new(dew24, /no_copy, /allocate)
  dew39 = where(cf3.dewar eq 39)
  ;pdew39 = ptr_new(dew39, /no_copy, /allocate)

  (*(*(*pstate).pfunctargs).dew24) = dew24
  (*(*(*pstate).pfunctargs).dew39) = dew39
  ;stop
  
  ;restore, '~/idl/exoidl/data/planeten.dat'
  print, 'newname is: ', newname
  firstchar=stregex(newname, 'vst')
  lastchar=stregex(newname, '\.dat')
  extitle=strmid(newname, firstchar+3, lastchar-firstchar-3)
  if stregex(strmid(extitle, 0,1), '[0-9]', /boolean) then begin
  extitle='HD'+extitle
  endif
  print, 'extitle is: ', extitle
  norbs = stardat(extitle, pstate=pstate)  
  ;stop
  if norbs.mstar gt 0 then begin
	 (*(*pstate).pfunctargs).m_star = norbs.mstar
	 endif else (*(*pstate).pfunctargs).m_star = 1d
	 (*(*pstate).pfunctargs).rstar = norbs.knownstarrad
	 (*(*pstate).pfunctargs).extitle=extitle
	 (*pstate).ndof = 5d
;stop
	 widget_control, (*pstate).controlbar.smassval, $
	 set_value=strt(norbs.mstar)
	 widget_control, (*pstate).controlbar.smassuncval, $
	 set_value=strt(norbs.unc_mstar)
	 widget_control, (*pstate).controlbar.sradiusval, $
	 set_value=strt(norbs.knownstarrad)
	 widget_control, (*pstate).controlbar.sradiusuncval, $
	 set_value=strt(norbs.unc_rstar)

  endif;IF CANCEL NOT CLICKED, DO THE ABOBE
  
  
  ;Update all the fields and plot:    
  kfme_update_fields, pstate

  ;Save the pointer to the state structure:
;  widget_control, event.top, set_uvalue=pstate
   ;stop

  print, 'File '+newname+' Restored.'
  
end ;kfme_restore_keck.pro

pro kfme_importtxt, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate

  ;User select the name of the residuals to restore:
  newname = dialog_pickfile(/read, filter='**.txt', title= $
  'Enter Filename to Restore...', $
  path=(*pstate).datadir)
  
  if newname ne '' then begin ;the CASE where CANCEL is clicked
   
  readcol, newname, jd, mnvel, errvel, $
  	delimiter=(*pstate).import_delimiter, $
  	skipline=(*pstate).import_skiplines, f='D, D, D'
  
  jd += double((*pstate).import_jdoff)
  jd -= 2.44d6
  if (*pstate).import_rvunit eq 'kms' then mnvel *= 1d3
  if (*pstate).import_errunit eq 'kms' then errvel *= 1d3
  
  cfi = create_struct('jd', 0d, $
  	'mnvel', 0d, $
  	'errvel', 0d, $
  	'dewar', 0d)
  cf3 = replicate(cfi, n_elements(jd))
  cf3.jd = jd
  cf3.mnvel = mnvel - median(mnvel)
  cf3.errvel = errvel
  cf3.dewar = intarr(n_elements(jd))

  ;x = where(cf3.errvel lt 2.5*median(cf3.errvel), errct)
  ;cf3 = cf3[x]
  printjds, cf3
  
  print, '# of Observations: ', n_elements(cf3)
  ;stop
  ;velplot, cf3, '', 4./24., dates, speed, errv, cai, nav, bincf, /noplot
  ;cf3=bincf
  ;stop
  ;if max(cf3.jd) lt 2.44d6 then cf3.jd += 2.44d6
	loadct, 39, /silent
	!p = (*pstate).p_orig
  
  cf = create_struct('cf_ast', (*(*pstate).pcf).cf_ast, $
                     'cf_rv', cf3, $
                     'm_star', 1d, $
                     'plx', (*(*pstate).pcf).plx, $
                     'prpr', (*(*pstate).pcf).prpr, $
                     'coords', (*(*pstate).pcf).coords, $
                     'midtime', 0d, $max(cf3.jd) - min(cf3.jd), $
                     'time_offset', 0d);(*(*pstate).pcf).time_offset)
  cf.cf_rv.mnvel = cf3.mnvel-median(cf3.mnvel)
  (*(*pstate).pcf) = cf
  (*(*pstate).pcf).cf_ast.jd *=0d
  (*pstate).xmin = min(cf3.jd)
  (*pstate).xmax = max(cf3.jd)
  (*(*pstate).pcfname) = newname
  printjds, cf3
  print, '# of Observations Restored: ', n_elements(cf3)
  
  dew24 = where(cf3.dewar eq 24)
  ;pdew24 = ptr_new(dew24, /no_copy, /allocate)
  dew39 = where(cf3.dewar eq 39)
  ;pdew39 = ptr_new(dew39, /no_copy, /allocate)

  (*(*(*pstate).pfunctargs).dew24) = dew24
  (*(*(*pstate).pfunctargs).dew39) = dew39
  ;stop
  
  ;restore, '~/idl/exoidl/data/planeten.dat'
  print, 'newname is: ', newname
  firstchar=stregex(newname, 'vst')
  lastchar=stregex(newname, '\.dat')
  extitle=strmid(newname, firstchar+3, lastchar-firstchar-3)
  if stregex(strmid(extitle, 0,1), '[0-9]', /boolean) then begin
  extitle='HD'+extitle
  endif
  print, 'extitle is: ', extitle
  norbs = stardat(extitle, pstate=pstate)  
  ;stop
  if norbs.mstar gt 0 then begin
	 (*(*pstate).pfunctargs).m_star = norbs.mstar
	 endif else (*(*pstate).pfunctargs).m_star = 1d
	 (*(*pstate).pfunctargs).rstar = norbs.knownstarrad
	 (*(*pstate).pfunctargs).extitle=extitle
	 (*pstate).ndof = 5d
;stop
	 widget_control, (*pstate).controlbar.smassval, $
	 set_value=strt(norbs.mstar)
	 widget_control, (*pstate).controlbar.smassuncval, $
	 set_value=strt(norbs.unc_mstar)
	 widget_control, (*pstate).controlbar.sradiusval, $
	 set_value=strt(norbs.knownstarrad)
	 widget_control, (*pstate).controlbar.sradiusuncval, $
	 set_value=strt(norbs.unc_rstar)

  endif;IF CANCEL NOT CLICKED, DO THE ABOVE
  
  
  ;Update all the fields and plot:    
  kfme_update_fields, pstate

  ;Save the pointer to the state structure:
;  widget_control, event.top, set_uvalue=pstate
   ;stop

  print, 'File '+newname+' Restored.'
  
end ;kfme_importtxt.pro

pro kfme_importdelimiter, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).import_delimiter = newpar
  
  print, 'New delimiter for importing data: ', newpar
end;kfme_importtxt.pro

pro kfme_importskiplines, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).import_skiplines = strt(newpar)
  
  print, 'New # lines to skip when importing data: ', newpar
end;kfme_importskiplines.pro

pro kfme_importjdoff, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).import_jdoff = strt(newpar)
  
  print, 'New BJD Offset when importing data: ', newpar
end;kfme_importjdoff.pro

pro kfme_importrvunit, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).import_rvunit = strt(newpar)
  
  print, 'New RV unit when importing data: ', newpar
end;kfme_importrvunit.pro

pro kfme_importerrunit, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).import_errunit = strt(newpar)
  
  print, 'New RV unit when importing data: ', newpar
end;kfme_importerrunit.pro

pro kfme_saveall, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate
  
  ;new directory
  if ~file_test((*pstate).outputdir+'save_files', /directory) $
	then spawn, 'mkdir '+(*pstate).outputdir+'save_files'

  
  
  ;User select the name of the file to save to:
  newname = dialog_pickfile(filter='*.dat', title= $
  'Enter Filename to Save Everything As...', $
  path=(*pstate).outputdir+'save_files/', /write)
 
  state = (*pstate)
  print, 'Filename chosen is: ', newname
  
  save, state, filename = newname

  print, 'File ',newname,' Saved.'
  
end ;kfme_saveall.pro

pro kfme_restoreall, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate

  ;User select the name of the residuals to restore:
  newname = dialog_pickfile(filter='*.dat', title= $
  'Enter Filename...', $
  path=(*pstate).outputdir+'save_files/')
 
  if newname ne '' then begin
  print, 'you restored ', newname
  restore, newname
  
 state = {ast:state.ast, $
 			 botrow:(*pstate).botrow, $
 			 chisq:state.chisq, $
 			 combperg:state.combperg, $
 			 controlbar:(*pstate).controlbar, $
 			 connect:state.connect, $
 			 controlbase:(*pstate).controlbase, $
 			 ndof:state.ndof, $
 			 datadir:state.datadir, $
 			 dew24:state.dew24, $
 			 dew39:state.dew39, $
 			 draw:(*pstate).draw, $
 			 fapiter:state.fapiter, $
 			 fixbttns:(*pstate).fixbttns, $
 			 fitplarr:state.fitplarr, $
 			 jitternum:state.jitternum, $
 			 kfmedir:state.kfmedir, $
 			 fitplbttns:(*pstate).fitplbttns, $
 			 linestyle:state.linestyle, $
 			 multith:state.multith, $
 			 outputdir:state.outputdir, $
 			 p_orig:state.p_orig, $
 			 par1:state.par1, $
 			 pars:state.pars, $
 			 pcf:state.pcf, $
 			 pcf_resid:state.pcf_resid, $
 			 pcfname:state.pcfname, $
 			 pdatls:(*pstate).pdatls, $
 			 pdata:state.pdata, $
 			 pdatname:state.pdatname, $
 			 perghi:state.perghi, $
 			 perglow:state.perglow, $
 			 pergres:state.pergres, $
 			 ppergfap:(*pstate).ppergfap, $
 			 pergfapbool:(*pstate).pergfapbool, $ 			 
 			 pfunctargs:state.pfunctargs, $
 			 planettabs:(*pstate).planettabs, $
 			 plot_time_study:(*state.pfunctargs).time_study, $
 			 previousjitter:state.previousjitter, $
 			 printpars:(*pstate).printpars, $
 			 psplot:state.psplot, $
 			 psym:state.psym, $
 			 ptransit:state.ptransit, $
 			 resetbtns:(*pstate).resetbtns, $
 			 rmsresid:state.rmsresid, $
 			 rv:state.rv, $
 			 scroll:(*pstate).scroll, $
 			 sliderflds:(*pstate).sliderflds, $
 			 tfine:state.tfine, $
 			 titleflag:(*pstate).titleflag, $
 			 togerr:state.togerr, $
 			 txtflds:(*pstate).txtflds, $
 			 toprow:(*pstate).toprow, $
 			 win_id:(*pstate).win_id, $
 			 xmin:state.xmin, $
 			 xmax:state.xmax, $
 			 zoomplot:state.zoomplot}

  pstate = ptr_new(state, /no_copy, /allocate)
  
  ;Update all the fields and plot:    
  kfme_update_fields, pstate

  ;Save the pointer to the state structure:
  widget_control, event.top, set_uvalue=pstate
 endif;user didn't cancel
end ;kfme_restoreall.pro

pro kfme_list, event
   
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate

  widget_control, event.id, get_value=cfname
  print, cfname[event.index]
  ;stop
  
 restore, '/mir1/mpflib/data/datstructures/'+cfname[event.index]
 
 astx = cf.cf_ast.astx
 asty = cf.cf_ast.asty
 data = [astx, asty, cf.cf_rv.mnvel]
 jd_ast = (*(*pstate).pcf).cf_ast.jd

 max_time=max(cf.cf_ast.jd)
 n_obs=n_elements(cf.cf_ast.jd)
 time_study= fix(max_time * 1.1)
 n_planets = (*(*pstate).pfunctargs).n_planets
 cf_plx = cf
 obs_times_ast = [jd_ast, jd_ast + time_study]
 obs_times_rv = 2*time_study + (*(*pstate).pcf).cf_rv.jd

 xmin = min(obs_times_rv)
 xmax = max(obs_times_rv)

 functargs={m_star:cf.m_star, parallax:cf.plx, time_study:time_study, $
 				n_planets:n_planets, scaling:1, time_offset:cf.time_offset,$
 				cf_plx:cf_plx}
 				
  ;Create a structure of data for the application:
  (*(*pstate).pfunctargs) = functargs
  (*(*pstate).pcf) = cf
  (*(*pstate).pdata) = data
  (*(*pstate).pcfname) = cfname[event.index]
  (*(*pstate).pcf).time_offset = cf.time_offset
  (*(*pstate).pcf_resid)=cf[0]
  (*pstate).plot_time_study = time_study
  
 kfme_xminxmax, pstate
end ;kfme_list.pro

pro kfme_data, event

  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate
  widget_control, event.id, get_value=val
  
  case val of
  'PRE3' : begin
       print, 'Now using Preview3 data.'
       datls = $
       ['cfp001s03_111.dat', $ 
		  'cfp009s03_111.dat']
     end
  'RAND': begin
       print, 'Now using RANDbatch1 data.'
       datls = $
       ['cfp301s03_111.dat', $
		  'cfp533s03_111.dat']
     end
  'BLIND21': begin
       print, 'Now using BLIND2 First Half data.'
       datls = $
		 ['cf1_001bl21.dat', $
		  'cf1_020bl21.dat']
     end
  'BLIND22': begin
       print, 'Now using BLIND2 Second Half data.'
       datls = $
		 ['cf1_001bl22.dat', $
		  'cf1_020bl22.dat']
     end
  endcase ;val

  pdatls = ptr_new(datls, /no_copy, /allocate)
  (*pstate).pdatls = pdatls
  widget_control, (*(*pstate).pdatname), /update
  widget_control, (*(*pstate).pdatname), set_value=(*pdatls)
end ;kfme_data.pro
  
pro kfme_plotdata, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  

  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id

  ;Call the "do" routine:
  kfme_dofit, pstate
end ;kfme_plotdata.pro

pro kfme_plotresid, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
   
  !p.multi=[0,1,1]


	  ;The fit for JUST RV:
     fitobs = (*(*pstate).pcf).cf_rv.jd
	  fitdat = (*(*pstate).pcf_resid).cf_rv.mnvel
	  errarr = (*(*pstate).pcf).cf_rv.errvel

   			 
  ;Plot the data:
  result = label_date(date_format=['%M %Y'], offset=2.44d6)
  
  if (*pstate).psplot then begin
	 filen=nextnameeps('kfmeresidplot', '')
	 ps_open, filen, /encaps, /color
  endif;postscript
  
  rvran = max(fitobs)-min(fitobs)

  ;use user-defined y range if specified
  if (*pstate).yminmax then begin
	 yrng = [(*pstate).yminval, (*pstate).ymaxval]
  endif else begin
	 yrng = [min(fitdat), max(fitdat)]
  endelse
  
  plot, fitobs, fitdat, $
	 linestyle=(*pstate).linestyle, $
	 color = 0, psym=(*pstate).psym*(*pstate).connect, $
	 XTickFormat='label_date', xminor=4, XTICKINTERVAL = rvran/7, $
	 xtitle = 'Time of Observation', $
	 ytitl='Residual Radial Velocity [m s!u-1!n]', $
	 ysty=(*pstate).yminmax, yrange = yrng
  
  ;Plot the data:
  ;plot, fitobs, fitdat, $
  ;	 linestyle=(*pstate).linestyle, $
  ;  psym=(*pstate).psym*(*pstate).connect, color = 0
  if (*pstate).togerr then oploterr, fitobs, fitdat, errarr, 8

  fity = dblarr(n_elements(fitdat))
  chisq = total(((fitdat-fity)/errarr)^2)/$
	 (n_elements(fitobs) - (*pstate).ndof)
  fity = dblarr(n_elements(fitdat))
;stop	 
  ;print, '# fitobs: ', n_elements(fitobs)
  ;print, 'ndof: ', (*pstate).ndof
	 
  if n_elements(fitobs) le (*pstate).ndof then begin
  xyouts, 0.6, 0.2, 'N <= n !!', $
  /normal, color=240, charthick = 2
  print, '******************************************'
  print, '             !!WARNING!!                 '
  print, 'The number of data points is less than or'
  print, 'equal to the number of degrees of freedom!'
  print, '******************************************'
  ;stop
  endif
  
  ;rmsresid = stddev(fitdat-fity)
  nfree = (*pstate).ndof
  rmsresid = sqrt(total((fitdat - fity)^2)/(n_elements(fitobs)-nfree))

  if ~(*pstate).psplot then begin
  
	xyouts, .1, .01, 'real '+Greek('chi')+'!d'+Greek('nu')+$
	'!u2!n'+': '+strt(chisq, f='(F9.2)'), /norm
	;rmsresid = stddev(fitdat-fity)
	xyouts, .4, .01, 'RMS: '+strt(rmsresid, f='(F9.2)'), /norm
	xyouts, .7, .01, 'STDDEV RV: '+$
	   strt(stddev((*(*pstate).pcf).cf_rv.mnvel), f='(F9.2)'), /norm
  ;	 stop
  endif
  
  if (*pstate).psplot then begin
	 ps_close
	  widget_control, (*pstate).controlbar.psplotbutton, set_button = 0
	 (*pstate).psplot = 0
  endif

end ;kfme_plotresid.pro

pro kfme_set_y_range, event

  ;This procedure will set the y range if the Y_MINMAX button
  ;is pressed.
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).yminmax = event.select
  
  ;print
  print, 'User-defined Y range engaged?', (*pstate).yminmax
end ;kfme_set_y_range.pro

pro kfme_ymax, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y max value:
  (*pstate).ymaxval = double(newpar)
  
  print, 'New Y max: ', newpar

  ;Call the "do" routine:
  kfme_dofit, pstate
end;kfme_ymax.pro

pro kfme_ymin, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the y min value:
  (*pstate).yminval = double(newpar)
  
  print, 'New Y min: ', newpar

  ;Call the "do" routine:
  kfme_dofit, pstate
end;kfme_ymin.pro

pro kfme_stellarmass, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the stellar mass:
  (*(*pstate).pfunctargs).m_star = double(newpar)
  
  print, 'New Stellar Mass: ', newpar
end;kfme_stellarmass.pro

pro kfme_stellarmassunc, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the stellar mass uncertainty:
  ;(*(*pstate).pfunctargs) = double(newpar)
  
  print, 'WARNING! : KFME CANNOT CURRENTLY HANDLE STELLAR MASS'
  print, 'UNCERTAINTY! THIS VALUE WILL BE IGNORED!'
  print, 'TYPE ".c" TO CONTINUE.'
  stop
end;kfme_stellarmassunc.pro

pro kfme_stellarradius, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the stellar radius:
  (*(*pstate).pfunctargs).rstar = double(newpar)
  
  print, 'New Stellar Radius: ', newpar
end;kfme_stellarradius.pro

pro kfme_stellarradiusunc, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the stellar radius uncertainty:
  (*(*pstate).pfunctargs) = double(newpar)
  
  print, 'WARNING! : KFME CANNOT CURRENTLY HANDLE STELLAR RADIUS'
  print, 'UNCERTAINTY! THIS VALUE WILL BE IGNORED!'
  print, 'TYPE ".c" TO CONTINUE.'
  stop
end;kfme_stellarradiusunc.pro

pro kfme_planetalone, event

  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the periodogram resolution (# of frequencies sampled):
  (*pstate).controlbar.aloneplanet = newpar
  
  print, 'The planet to be plotted alone is: ', newpar

end;kfme_planetalone.pro

pro kfme_aloneplot, event

  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate


	wset, (*pstate).win_id
	
  !p.multi=[0,1,1]
  !p.charsize = 2.
  !p.thick = 2.


	pararr = kfme_create_par(pstate)
	
	alpl = (*pstate).controlbar.aloneplanet
	
	pararrrest = pararr
	pararrrest[(5*(alpl-1d)):(5*(alpl-1d)+4)].value = 0
	
	jds = (*(*pstate).pcf).cf_rv.jd
	errvels = (*(*pstate).pcf).cf_rv.errvel
	
	tfitrest = rv_mpfit_rv(jds, pararrrest.value, $
		_EXTRA = (*(*pstate).pfunctargs))
	
	

	minmaxels = minmax((*(*pstate).pcf).cf_rv.jd)
	finres = 2d4
	tfine = 1.2*(minmaxels[1] - minmaxels[0])*dindgen(finres+1d)/finres $
	+ minmaxels[0] - 0.1*(minmaxels[1] - minmaxels[0])
		
    functargsloner = {n_planets:1, $
    m_star : (*(*pstate).pfunctargs).m_star, $
    dew24 : (*(*pstate).pfunctargs).dew24, $
    dew39 : (*(*pstate).pfunctargs).dew39    }
		
    pararrloner = pararr[(5*(alpl-1d)):(5*(alpl-1d)+4)]
    pararrlonervals = [pararrloner.value, dblarr(5)]
    
	tfitloner = rv_mpfit_rv(tfine, pararrlonervals, $
		_EXTRA = functargsloner)
		
	mnvels = (*(*pstate).pcf).cf_rv.mnvel

   rv_arr =      (*(*pstate).pcf).cf_rv.jd
   rvran = max(rv_arr)-min(rv_arr)

  if (*pstate).psplot then begin
     !p.font=1
     if ~file_test('~/kfme_output', /directory) then spawn, 'mkdir ~/kfme_output'
     if ~file_test('~/kfme_output/ALONES', /directory) then spawn, 'mkdir ~/kfme_output/ALONES'
     
	 filen=nextnameeps('~/kfme_output/ALONES/aloneplot', '', /nosuf)
     print, 'the name is: ', filen
	 ps_open, filen, /encaps, /color
  endif;postscript

  ;Plot the data:
  result = label_date(date_format=['%M %Y'], offset=2.44d6)
  

	 ;use user-defined y range if specified
	 if (*pstate).yminmax then begin
		yrng = [(*pstate).yminval, (*pstate).ymaxval]
	 endif else begin
		yrng = [min(tfitloner), max(tfitloner)]
	 endelse
  
    loadct, 39, /silent
if ~(*pstate).controlbar.phasebool then begin
  plot,tfine, tfitloner, $
	linestyle=(*pstate).linestyle, $
	color = 0., psym=(*pstate).psym*(*pstate).connect, $
	XTickFormat='label_date', xminor=4, XTICKINTERVAL = rvran/4, $
	xtitle = 'Time of Observation', $
	ytitl='Radial Velocity [m s!u-1!n]', $
	symsize = 0.25, xran = minmax(tfine), /xsty, thick=9., /nodata , $
	ysty = (*pstate).yminmax, yrange=yrng

  oplot,tfine, tfitloner, $
	linestyle=(*pstate).linestyle, $
	color = 80, thick=9.	 

endif else begin
  pstart = double((*pstate).controlbar.phasestart)/100d * $
	pararrloner[0].value
  tfinph = ((tfine + pstart) mod pararrloner[0].value)/pararrloner[0].value
  jds = ((jds + pstart) mod pararrloner[0].value)/pararrloner[0].value
  
  orderedph = sort(tfinph)
  
  rvels = mnvels - tfitrest
  ;This section will calculate the range for the data:
  range = max(tfitloner) - min(tfitloner)
  
  ;use user-defined y range if specified
  if (*pstate).yminmax then begin
	 yrng = [(*pstate).yminval, (*pstate).ymaxval]
  endif else begin
	 yrng = [min(tfitloner) - 0.25*range, max(tfitloner) + 0.5*range] > $
           [min(rvels) - 0.25*range, max(rvels) + 0.25*range]
  endelse

  plot,tfinph, tfitloner, $
	linestyle=(*pstate).linestyle, $
	color = 0, psym=(*pstate).psym*(*pstate).connect, $
	xminor=4, $
	xtitle = 'Orbital Phase', $
	ytitl='Radial Velocity [m s!u-1!n]', $
	symsize = 0.25, $
	xran = [-0.2, 1.2], /xsty, $
	yran = yrng, ysty=(*pstate).yminmax, thick=9., /nodata

  oplot,tfinph[orderedph], tfitloner[orderedph], $
	linestyle=(*pstate).linestyle, $
	color = 80, thick=9.
  
  innerphase = jds
  fluffl = where(innerphase gt 0.8) & fluffr = where(innerphase lt 0.2)
  fluffedvs = [rvels[fluffl], rvels[fluffr]]
  fluffedts = [innerphase[fluffl]-1d, innerphase[fluffr]+1d] 
  loadct, 0, /silent
  
  bufclr = 0.
  bufthk = 7.
  ploterrbrs, innerphase, rvels, errvels
  fluffederrs = [errvels[fluffl], errvels[fluffr]]
  ploterrbrs, fluffedts, fluffedvs, fluffederrs, color=bufclr
  usersymbol, 'circle', thick=bufthk
  oplot, fluffedts, fluffedvs, ps=8, color=bufclr, thick=9

  ffluffl = where(tfinph gt 0.8) & ffluffr = where(tfinph lt 0.2)
  ordrflfl = sort(tfinph[ffluffl])
  ordrflfr = sort(tfinph[ffluffr]) 
  
  usersymbol, 'circle', /fill, thick=2
  oplot, tfinph[ffluffl[ordrflfl]]-1d, $
  tfitloner[ffluffl[ordrflfl]], color=bufclr, thick=bufthk;, $
	;ps=8;, symsize=0.25
  oplot, tfinph[ffluffr[ordrflfr]]+1d, $
  tfitloner[ffluffr[ordrflfr]], color=bufclr, thick=bufthk;, $
	;ps=8;, symsize=0.25
	
  ;THIS PART SHADES THE TRANSIT WINDOW IF "MONTE..." HAS BEEN
  ;RUN:
  ptrsz = size((*(*pstate).ptransit))
  ;the third elements, [2], of the result of the size()
  ;procedure is the type code. Initially, the ptransit
  ;pointer is a byte. if it's a structure (type =8) then
  ;it means that there's transit data and this will be 
  ;overplotted:
  if (ptrsz[2] gt 7) then begin
    tbegjd = (*(*pstate).ptransit).tcen - $		  
			 (*(*pstate).ptransit).tdur/48d - $		  
			 (*(*pstate).ptransit).unc_tcen
    tendjd = (*(*pstate).ptransit).tcen + $
			 (*(*pstate).ptransit).tdur/48d + $
			 (*(*pstate).ptransit).unc_tcen
	trnstjds = [tbegjd, tendjd]
	trnstphs = ((trnstjds + pstart) mod $
	  pararrloner[0].value)/pararrloner[0].value

			 
	polyx = [trnstphs[0], trnstphs[1], trnstphs[1], trnstphs[0]]
	;yrange = [-1d3, 1d3]
	polyy = [yrange[0]- 5d-3 * yrange[0], yrange[0]- 5d-3 * yrange[0], $
	yrange[1] - 5d-3 * yrange[1], yrange[1] - 5d-3 * yrange[1]]
	loadct, 0, /silent
	polyfill, polyx, polyy, color = 200
	;stop
  endif;shading the transit window
  
endelse

  usersymbol, 'circle', size = 1.5, /fill
  ;80 is a nice color if you're using color, 0 is for the b&w
	oplot,  jds, mnvels - tfitrest, ps=8, color=0
  errarr = (*(*pstate).pcf).cf_rv.errvel
  if (*pstate).togerr then oploterr, $
  jds, mnvels - tfitrest, errarr
  
  ;CHECKED 2010.06.28 BECAUSE OF HI CHI SQ VALUES, BUT
  ;IT IS CORRECT. I'M USING THE EQUATION THAT CAN BE
  ;FOUND HERE:
  ;http://en.wikipedia.org/wiki/Goodness_of_fit
  ;(*PSTATE).NDOF IS TAKING THE "-1" INTO ACCOUNT
  ;ALREADY. 2 OTHER FREE PARAMETERS COME FROM THE
  ;DIFFERENT DEWARDS WHICH ARE ALSO BEING FIT FOR. 
  ;ACTUALLY, FOR THE KECK OBSERVATIONS OF 163607, 
  ;ONLY 1 DEWAR WAS USED, SO THIS SHOULDN'T BE FIT
  ;FOR.
   

  if (*pstate).psplot then begin
    if (*(*pstate).pfunctargs).extitle eq 'HD163607' then begin
       if (*pstate).controlbar.aloneplanet eq 2 then xyouts, $
           0.24, 0.85, 'HD 163607b (OUTER PLANET REMOVED)', /norm
           
       if (*pstate).controlbar.aloneplanet eq 1 then xyouts, $
           0.24, 0.85, 'HD 163607c (INNER PLANET REMOVED)', /norm
    endif;163607
	 ps_close
	  widget_control, (*pstate).controlbar.psplotbutton, set_button = 0
	 (*pstate).psplot = 0

  spawn, 'open '+filen+'.eps'
	 
  endif else begin
  
  chisq = (*pstate).chisq
  rmsresid = (*pstate).rmsresid

  xyouts, .1, .01, 'real '+Greek('chi')+'!d'+Greek('nu')+$
  '!u2!n'+': '+strt(chisq, f='(F9.2)'), /norm
  ;rmsresid = stddev(fitdat-fity)
  xyouts, .4, .01, 'RMS: '+strt(rmsresid, f='(F9.2)'), /norm
  xyouts, .7, .01, 'STDDEV RV: '+$
	 strt(stddev((*(*pstate).pcf).cf_rv.mnvel), f='(F9.2)'), /norm

  endelse
  

	loadct, 39, /silent
end;kfme_aloneplot.pro

pro kfme_orbitphase, event
  ;This procedure will tie m & i for the case where the radial velocity
  ; data looks really good and you want to adjust the astrometry. 
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).controlbar.phasebool = event.select
  
  if event.select then print, 'phase ENGAGED.' else print, $
  'phase disengaged.'
  
end;kfme_orbitphase.pro

pro kfme_sliderphase, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).controlbar.phasestart = double(event.value)

  print, 'Orbital phase will now start ', strt(event.value), $
        ' % of the period past the periasttron passage.'

end ;kfme_sliderphase.pro

pro kfme_pergram, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
  
  if (*pstate).psplot then begin
  	 ;!x.margin=[12,5]
     !p.font=1
     if ~file_test('~/kfme_output', /directory) then spawn, 'mkdir ~/kfme_output'
     if ~file_test('~/kfme_output/PERGRAMS', /directory) then spawn, 'mkdir ~/kfme_output/PERGRAMS'
     
	 filen=nextnameeps('~/kfme_output/PERGRAMS/pergram', '')
	 ps_open, filen, /encaps, /color
  endif;postscript
  
  if (*pstate).titleflag then begin
  pergtitle = (*(*pstate).pcfname)+' RV '
  endif else pergtitle=' '
  
  if ~(*pstate).pergfapbool then begin
		  fap = 0
		  multiple =0
  endif else begin
		  fap = (*(*pstate).ppergfap)
		  multiple = 10d / min(fap)
  endelse
  
  kfme_pergram_min, (*(*pstate).pcf).cf_rv, $
  /verbose, lowper=(*pstate).perglow, pmax=(*pstate).perghi, $
  numper=(*pstate).pergres, title = pergtitle, $
  fap = fap, multiple=multiple, simsigni = simsigni, $
  pergstruct=pergstruct
  ;uncomment this stop if you want to save the periodogram information
  ;STOP
  
  if (*pstate).psplot then begin
	 ps_close
    spawn, 'open '+filen+'.eps'

	  widget_control, (*pstate).controlbar.psplotbutton, set_button = 0
	 (*pstate).psplot = 0
  endif
end;kfme_pergram.pro


pro kfme_residpergram, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
  
  if (*pstate).psplot then begin
  	 !x.margin=[12,5]
     !p.font=1
     if ~file_test('~/kfme_output', /directory) then spawn, 'mkdir ~/kfme_output'
     if ~file_test('~/kfme_output/PERGRAMS', /directory) then spawn, 'mkdir ~/kfme_output/PERGRAMS'
     
	 filen=nextnameeps('~/kfme_output/PERGRAMS/residpergram', '')
	 ps_open, filen, /encaps, /color
  endif;postscript
  
  if (*pstate).titleflag then begin
  pergtitle = (*(*pstate).pcfname)+' RV '
  endif else pergtitle=''
  
  if ~(*pstate).pergfapbool then begin
		  fap = 0
		  multiple =0
  endif else begin
		  fap = (*(*pstate).ppergfap)
		  multiple = 10d / min(fap)
  endelse
  
  kfme_pergram_min, (*(*pstate).pcf_resid).cf_rv, $
  /verbose, lowper=(*pstate).perglow, pmax=(*pstate).perghi, $
  numper=(*pstate).pergres, title = pergtitle, $
  fap = fap, multiple=multiple, simsigni = simsigni, $
  pergstruct=pergstruct
  ;uncomment this stop if you want to save the periodogram information
  ;STOP
  
  if (*pstate).psplot then begin
	 ps_close
    spawn, 'open '+filen+'.eps'

	  widget_control, (*pstate).controlbar.psplotbutton, set_button = 0
	 (*pstate).psplot = 0
  endif
end;kfme_residpergram.pro

pro kfme_lowperg, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lower periodogram period limit:
  (*pstate).perglow = double(newpar)
  
  print, 'New Lower Period Periodogram Limit: ', newpar
end;kfme_lowperg.pro

pro kfme_hiperg, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lower periodogram period limit:
  (*pstate).perghi = double(newpar)
  
  print, 'New Upper Period Periodogram Limit: ', newpar
end;kfme_hiperg.pro

pro kfme_pergres, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the periodogram resolution (# of frequencies sampled):
  (*pstate).pergres = double(newpar)
  
  print, '# of Periodogram Frequencies Sampled: ', newpar
end;kfme_pergres.pro

pro kfme_pergfap, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  b = strsplit(newpar, ',', /extract)
  c = double(b)
  ;change the periodogram FAP desired:
  (*(*pstate).ppergfap) = c
  
  print, 'False Alarm Probability Desired: ', (*(*pstate).ppergfap)
end;kfme_pergfap.pro

pro kfme_pergfapbool, event

  ;This procedure will make a postscript plot of the next
  ;button pressed.
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the periodogram FAP overplots
  (*pstate).pergfapbool = event.select
  
  print, 'Periodogram FAP Overplots Engaged?', (*pstate).pergfapbool
end ;kfme_pergfapbool.pro

pro kfme_fap, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
  
  loadct, 39, /silent
  
  ;First determine what planet to run the FAP analysis on:
  planetnum = (*pstate).controlbar.aloneplanet
  
  if (*(*pstate).pfunctargs).n_planets eq 1 then planetnum = 1
  
  pararr = kfme_create_par(pstate)
  opararr = pararr
  
	m_star = (*(*pstate).pfunctargs).m_star
	inc = 89.9d
  
  orbel=dblarr(7)
;  orbel[7*indx] = orbpar[indx*5] ;p
;  orbel[7*indx+1] = orbpar[5*indx+4] ;tp
;  orbel[7*indx+2] = orbpar[5*indx+2] ;e
;  orbel[7*indx+3] = orbpar[5*indx+3] ;om
;  orbel[7*indx+4] = k ;k

  ;now create a pararr where the planet of interest is
  ;omitted (set everything to zero except the period):
  if planetnum eq 1 then begin
    indx=0d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k

    orbparf = [opararr[0:4].value, dblarr(5)]
	pararr[0:4].value = 0d
	pararr[0].value = 1d4

  endif
  
  if planetnum eq 2 then begin

	indx=1d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k
	
    orbparf = [opararr[5:9].value, dblarr(5)]
	pararr[5:9].value = 0d
	pararr[5].value = 1d4
  endif

  if planetnum eq 3 then begin

	indx=2d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k
	
    orbparf = [opararr[10:14].value, dblarr(5)]
	pararr[10:14].value = 0d
	pararr[10].value = 1d4
   endif

  if planetnum eq 4 then begin

	indx=3d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k
	
    orbparf = [opararr[15:19].value, dblarr(5)]
	pararr[15:19].value = 0d
	pararr[15].value = 1d4
  endif

  if planetnum eq 5 then begin 

	indx=4d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k
	
    orbparf = [opararr[20:24].value, dblarr(5)]
	pararr[20:24].value = 0d
	pararr[20].value = 1d4
  endif

  if planetnum eq 6 then begin

	indx=5d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k

    orbparf = [opararr[25:29], dblarr(3)]
	pararr[25:29].value = 0d
	pararr[25].value = 1d4
  endif

  if planetnum eq 7 then begin

	indx=6d
	orbel[0] = pararr[5*indx + 0L].value ;p
	orbel[1] = pararr[5*indx + 4L].value ;tp
	orbel[2] = pararr[5*indx + 2L].value ;e
	orbel[3] = pararr[5*indx + 3L].value ;om
	
	period=pararr[indx*5].value
	a_pl=((period/365.2564d)^2*m_star)^(1./3.)
	m_pl_earth = pararr[indx*5+1].value
	ecc = pararr[indx*5+2].value
	k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
	orbel[4] = k ;k
    orbparf = [opararr[30:34], dblarr(3)]
	pararr[30:34].value = 0d
	pararr[30].value = 1d4
	
  endif

    
	fitrv = rv_mpfit_rv((*(*pstate).pcf).cf_rv.jd, pararr.value, $
		_EXTRA = (*(*pstate).pfunctargs))
  ;CFALONE is the velocity structure after removing the linear trend, 
  ;offset, and other planets in the system:
  cffull =  (*(*pstate).pcf).cf_rv
  cfalone = cffull
  cfalone.mnvel = cffull.mnvel - fitrv
  
  cf3 = (*(*pstate).pcf_resid).cf_rv
  ntrial = (*pstate).fapiter
  
  plot, cffull.jd, cffull.mnvel, ps=8
  oplot, cffull.jd, fitrv, ps=8, color=240
  oplot, cffull.jd, cfalone.mnvel, ps=8, color=80
;  stop
  pergram,cfalone,nu_out,peri_out,pkperiods,pkheights
  pkh=max(pkheights)
  
  plot, cfalone.jd, cfalone.mnvel, ps=8
  print, 'velocities - other planet.'
;  stop
  ;solo functargs
  sfunctargs = (*(*pstate).pfunctargs)
  sfunctargs.n_planets = 1
  ;orbel = [orbel, 0d, 0d, 0d]
  
  
  
	fitrv2 = rv_mpfit_rv((*(*pstate).pcf).cf_rv.jd, $
		orbparf, _EXTRA = sfunctargs)
  oplot, cfalone.jd, fitrv2, ps=8, color=80
  print, 'theoretical velocities overplotted in blue'
  oplot, cfalone.jd, cfalone.mnvel - fitrv2, ps=8, color=240
  print, 'the planet alone - best fit ', $
  		 '(i.e. the residuals) are in red. '
;  stop
  
  orbel=rv_fit_mp(cfalone.jd,cfalone.mnvel, cfalone.errvel, $
				yfit=syn_fit,tps=max(cffull.jd), $
				chi=chi_sq,rms=rms, /plotfit, $
				orbel=orbel)

;  stop
  chi_init = chi_sq
  if ~keyword_set(ntrial) then ntrial=1000
  mc_arr=fltarr(ntrial)
  chiarr=dblarr(ntrial) + 99d
  num=n_elements(nu_out)
  ;nu=fltarr(ntrial,num)  & pwr=fltarr(ntrial,num)

  ;save the originals:
  ocfalone = cfalone
  scramjd = cfalone.jd
  scrammnvel = dblarr(n_elements(cfalone.jd))
  scramerrvel = dblarr(n_elements(cfalone.jd))
  
  ;a variable to store the # of false alarms:
  fas = 0d
  
  mnvel=mean(cf3.mnvel)
  ;scramble the velocities
  ndata=n_elements(cf3) 
  for j=0,ntrial-1 do begin
	  for jj=0,ndata-1 do begin    ;replacement with redraw
		 dum=fix(randomu(seed)*ndata)
		 scramerrvel[jj]=cfalone[dum].errvel
		 scrammnvel[jj] = cfalone[dum].mnvel
	  endfor
	  orbel=rv_fit_mp(scramjd,scrammnvel, scramerrvel, $
			  yfit=syn_fit,tps=max(cffull.jd), $
			  chi=chi,rms=rms, /quiet)
		;stop	  
      chiarr[j] = chi^2
	  
	  ;now determine the number of false alarms:
	  fas += (chi^2 le chi_init)
	  
	  if j mod 10 then begin
	  print, 'the chi^2 for this realization: ', $
	  chi^2, '. Original: ', chi_init
	  print, 'Percentage Complete: ', j/ntrial*1d2, ' %.', $
	  ' FA so far: ', fas
      ;stop
	  endif
  endfor

;stop

  if (*pstate).psplot then begin
  	 !x.margin=[12,5]
     !p.font=1
     if ~file_test('~/kfme_output', /directory) then spawn, 'mkdir ~/kfme_output'
     if ~file_test('~/kfme_output/fap_plots', /directory) then spawn, 'mkdir ~/kfme_output/fap_plots'
     
     etitl = (*(*pstate).pfunctargs).extitle
     if planetnum eq 1 then endttl = 'b'
     if planetnum eq 2 then endttl = 'c'
     if planetnum eq 3 then endttl = 'd'
     if planetnum eq 4 then endttl = 'e'
     if planetnum eq 5 then endttl = 'f'
     if planetnum eq 6 then endttl = 'g'
     if planetnum eq 7 then endttl = 'h'
	 filen=nextnameeps('~/kfme_output/fap_plots/'+etitl+endttl, '')
	 ps_open, filen, /encaps, /color
  endif;postscript
  
thick=2
plothist, chiarr, xhist, yhist, bin=0.25, $
xtitle='!6 '+greek('chi')+'!d'+greek('nu')+'!u2!n', $
ytitle='Number', $
xrange = [0, max(chiarr)], $
/fill, /fline, forient=135d, $
fspacing = 0.1

oplot, [chi_init, chi_init], [1.5*max(yhist),  0d], linesty=2
arrow, chi_init, 0.05*max(yhist), chi_init, 0d, /data, /solid

xyouts, chi_init + 0.05*(max(xhist) - min(xhist)), $
		0.5*1.25*max(yhist), align = 0.5, $
		'!6'+greek('chi')+'!d'+greek('nu')+'!u2!n '+$
		'(Unscrambled Velocities)', $
		orient=90d, $
		charthick = 1.5, charsize = 0.9
  
  if (*pstate).psplot then begin
	ps_close
	spawn, 'open '+filen+'.eps'
  endif  
  
  gg=sort(chiarr)
  new_chi=chiarr(gg)
  rnew_chi=reverse(new_chi)
  ind=where(rnew_chi le chi_init,nind)
  if nind eq 0 then nind=1.0
  fap=1.0*nind/n_elements(rnew_chi)
  print,'FAP: ',strt(100d * fap), ' %'
  fap_out=fap
end;kfme_fap.pro

pro kfme_fapiter, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the periodogram resolution (# of frequencies sampled):
  (*pstate).fapiter = double(newpar)
  
  print, '# of FAP Iterations: ', newpar
end;kfme_fapiter.pro

pro kfme_run, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  widget_control, event.id, get_value=oldway
  
pararr = kfme_create_par(pstate)
opararr = pararr

;TAKE CARE OF THE DEWARS:
;DEWAR 24:
;IF THERE WASN'T A DEWAR 24 (I.E., KECK)
;THEN DON'T TREAT IT AS A FREE PARAMETER:
npararr = n_elements(pararr)
if ((*(*(*pstate).pfunctargs).dew24) lt 0) then begin
pararr[npararr-2].fixed = 1
endif;dew24

;THE SAME GOES FOR DEW 39:
if ((*(*(*pstate).pfunctargs).dew39) lt 0) then begin
pararr[npararr-1].fixed = 1
endif;dew39


print, 'The parameters before running...'
print, '---------------------------------------'
for q=0, n_elements(pararr.value)-1 do begin
 print, 'Name of '+strt(q)+':       ', pararr[q].parname
 print, 'Value:      ', strt(pararr[q].value)
 print, 'Fixed:      ', strt(pararr[q].fixed)
 print, 'Lo Limited: ', strt(pararr[q].limited[0])
 print, 'Lo Limit:   ', strt(pararr[q].limits[0])
 print, 'Hi Limited: ', strt(pararr[q].limited[1])
 print, 'Hi Limit:   ', strt(pararr[q].limits[1])
 print, 'Step:       ', strt(pararr[q].step)
 print, '- - - - - - - - - - - - - - - - - - - - '
 if ( (pararr[q].value le pararr[q].limits[0]) OR $
     (pararr[q].value ge pararr[q].limits[1]) ) then begin
     print, 'CHECK YOUR PARAMETERS AND LIMITS!!'
     print, 'AUTOFIXING THEM BEFORE RUN.........'
	  if (pararr[q].value le pararr[q].limits[0]) then begin
	  pararr[q].value +=(pararr[q].limits[0]-pararr[q].value)+.1
	  endif else pararr[q].value -= $
			(pararr[q].value-pararr[q].limits[1])+.1
	  print, pararr[q].parname, ' is now: ', pararr[q].value
 endif

 endfor
print, '---------------------------------------'


  astx = (*(*pstate).pcf).cf_ast.astx
  errx = (*(*pstate).pcf).cf_ast.errx
  asty = (*(*pstate).pcf).cf_ast.asty
  erry = (*(*pstate).pcf).cf_ast.erry

	;JUST RV
	fitobs = (*(*pstate).pcf).cf_rv.jd
	fitdat = (*(*pstate).pcf).cf_rv.mnvel
	err = (*(*pstate).pcf).cf_rv.errvel
	
   pararr.value = MPFITFUN('rv_mpfit_rv',fitobs, fitdat, $
    err, parinfo=pararr, covar=covar, perror=perror, $
	 yfit=syn_fit, dof=dof, $
	 functargs=(*(*pstate).pfunctargs), maxiter = 1000., $
	 XTOL = 1d-12, errmsg = errmsg)
  (*pstate).ndof = dof + 1
 
print, 'error messages were: ', errmsg

 chi_sq=total(((fitdat-syn_fit)/err)^2)/(n_elements(fitobs) - $
   (*pstate).ndof)

 rms = stddev(fitdat-syn_fit)  

 
 n_planets = (*(*pstate).pfunctargs).n_planets
 
print, ' '
print, '-----------------------------------'
print, 'FINAL ORBITAL PARAMETERS'
print, '-----------------------------------'
for pl=0,n_planets-1 do begin
print, 'Period (days)              =', pararr[5*pl+0].value, $
		'+/- ', strt(perror[5*pl+0])
print, 'Period (years)             =', pararr[5*pl+0].value/365.2564d, $
		'+/- ', strt(perror[5*pl+0]/365.2564d)
print, 'Mass (Earth masses)        =', pararr[5*pl+1].value, $
		'+/- ', strt(perror[5*pl+1])
print, 'Eccentricity               =', pararr[5*pl+2].value, $
		'+/- ', strt(perror[5*pl+2])
print, 'omega (degrees)            =', pararr[5*pl+3].value, $
		'+/- ', strt(perror[5*pl+3])
print, 'Time of peri rv (days)     =', pararr[5*pl+4].value, $
		'+/- ', strt(perror[5*pl+4])
print, ' '
endfor
print, ' '
print, 'Offset on rv (m/s)             =',pararr[5*n_planets].value, $
		'+/- ', strt(perror[5*n_planets])
print, 'Slope on rv (m/s /year)        =',pararr[5*n_planets+1].value, $
		'+/- ', strt(perror[5*n_planets+1])
print, 'Curve on rv (m/s /year^2)      =',pararr[5*n_planets+2].value, $
		'+/- ', strt(perror[5*n_planets+2])
print, 'Dewar 24 Offset (m/s)             =', $
		 pararr[5*n_planets+3].value, $
		'+/- ', strt(perror[5*n_planets+3])
print, 'Dewar 39 Offset (m/s)             =', $
		 pararr[5*n_planets+4].value, $
		'+/- ', strt(perror[5*n_planets+4])
	  
print, ' '
print, 'chi square: ', chi_sq
print, 'rms is: ', rms


 ;Now to put the pararr back into the state variable and store 
 ;the uncertainties:
 kfme_retrieve_par, pstate, pararr, perror

 print, 'COMPLETED MPFITFUN AND SENT TO kfme_DOFIT'

  ;Update the text fields with the new parameters:
  kfme_update_fields, pstate
  
end ;kfme_run.pro

pro kfme_cyclerun, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  widget_control, event.id, get_value=oldway
  
pararr = kfme_create_par(pstate)

print, 'The parameters before running...'
print, '---------------------------------------'
for q=0, n_elements(pararr.value)-1 do begin
 print, 'Name of '+strt(q)+':       ', pararr[q].parname
 print, 'Value:      ', strt(pararr[q].value)
 print, 'Fixed:      ', strt(pararr[q].fixed)
 print, 'Lo Limited: ', strt(pararr[q].limited[0])
 print, 'Lo Limit:   ', strt(pararr[q].limits[0])
 print, 'Hi Limited: ', strt(pararr[q].limited[1])
 print, 'Hi Limit:   ', strt(pararr[q].limits[1])
 print, 'Step:       ', strt(pararr[q].step)
 print, '- - - - - - - - - - - - - - - - - - - - '
 if ( (pararr[q].value le pararr[q].limits[0]) OR $
     (pararr[q].value ge pararr[q].limits[1]) ) then begin
     print, 'CHECK YOUR PARAMETERS AND LIMITS!!'
     print, 'AUTOFIXING THEM BEFORE RUN.........'
     if (pararr[q].value le pararr[q].limits[0]) then begin
     pararr[q].value += 0.1
     endif else pararr[q].value -= 0.1
 endif

 endfor
print, '---------------------------------------'


;This part will take care of the special case where
;the user has all of the parameters fixed (at which
;point MPFIT will crash:
if total(pararr.fixed) eq n_elements(pararr.value) then begin
 print, 'YOU HAVE ALL OF THE PARAMETERS FIXED!'
 goto, before_update_in_run
endif

 astx = (*(*pstate).pcf).cf_ast.astx
 errx = (*(*pstate).pcf).cf_ast.errx
 asty = (*(*pstate).pcf).cf_ast.asty
 erry = (*(*pstate).pcf).cf_ast.erry
 
 opararr = pararr
 n_plnt = (*(*pstate).pfunctargs).n_planets
 
 ;Determine the Number of Not Fixed Planets (NNFP):
 nnfp = n_plnt
 for i=0, n_plnt-1 do if pararr[7*i + 3].fixed then nnfp--
 scour = dblarr(nnfp)
 m=0 
print, 'scour before is: ', scour

for yz=0, n_plnt-1 do	begin
   if ~(pararr[7*yz+3].fixed) then begin
	 scour[m] = yz
	 m++
   endif
 endfor

 inc = [45., 135.]
 bigom = [60., 180., 300.]
 nbigom = n_elements(bigom)
 ninc = n_elements(inc)
 chiarr = dblarr((ninc*nbigom)^nnfp)
 
if nnfp eq 1 then begin
 for i=0, ninc-1 do begin
	for j=0, nbigom-1 do begin
	  startinc = inc[i mod ninc]
	  pararr[7*scour[0]+3].value = startinc
	  startbigo = bigom[j mod nbigom]
	  pararr[7*scour[0]+4].value = startbigo

		for q=0, n_elements(pararr.value)-1 do begin
		 if ( (pararr[q].value le pararr[q].limits[0]) OR $
			  (pararr[q].value ge pararr[q].limits[1]) ) then begin
			  print, 'Name of '+strt(q)+':       ', pararr[q].parname
			  print, 'Value:      ', strt(pararr[q].value)
			  print, 'Lo Limit:   ', strt(pararr[q].limits[0])
			  print, 'Hi Limit:   ', strt(pararr[q].limits[1])
			  print, '- - - - - - - - - - - - - - - - - - - - '
			  print, 'CHECK YOUR PARAMETERS AND LIMITS!!'
			  print, 'AUTOFIXING THEM BEFORE RUN.........'
			  if (pararr[q].value le pararr[q].limits[0]) then begin
			  pararr[q].value +=(pararr[q].limits[0]-pararr[q].value)+.1
			  endif else pararr[q].value -= $
			  		(pararr[q].value-pararr[q].limits[1])+.1
			  print, pararr[q].parname, ' is now: ', pararr[q].value
		 endif
		
		 endfor;check limits

	;JUST RV
	fitobs = (*(*pstate).pcf).cf_rv.jd
	fitdat = (*(*pstate).pcf).cf_rv.mnvel
	err = (*(*pstate).pcf).cf_rv.errvel
	
   pararr.value = MPFITFUN('mpf_mpfit_rv',fitobs, fitdat, $
    err, parinfo=pararr, covar=covar, perror=perror, $
	 yfit=syn_fit, dof=dof, functargs=(*(*pstate).pfunctargs))
  (*pstate).ndof = n_elements(fitobs) - dof


 chi_sq=total(((fitdat-syn_fit)/err)^2)/(n_elements(fitobs) - $
   (*pstate).ndof)

 print, 'start inc: ', startinc
 print, 'start bigom: ', startbigo
 print, 'chi sq is: ', chi_sq

       chiarr[i*nbigom + j] = chi_sq
     pararr = opararr
     endfor ;bigom loop
   endfor ;inc loop
endif;NumberofNotFixedPlanets eq 1


;FUTURE MULTIPLE PLANET CYCLES GO HERE:
if nnfp ge 2 then begin
  PRINT, ' '
  print, 'YOU HAVE TO MANY UNFIXED PLANETS!'
  print, 'FIX THE INCLINATION OF ONE OR MORE OF THEM AND TRY AGAIN.'
  print, ' '
  goto, before_update_in_run
endif
 
 bestchipos = where(chiarr eq min(chiarr))
 bestchipos = bestchipos[0]
 pararr = opararr
 print, 'best chi is: ', min(chiarr)
 
 if nnfp eq 1 then begin
    startinc = inc[bestchipos/nbigom]
    pararr[7*scour[0]+3].value = startinc
    startbigo = bigom[bestchipos mod nbigom]
    pararr[7*scour[0]+4].value = startbigo
    print, 'starting inclination: ', startinc
    print, 'starting big om: ', startbigo
    print, 'inc index: ', 7*scour[0]+3
    print, 'bigom index: ', 7*scour[0]+4
 endif;nnfp=1
 
 
	;JUST RV
	fitobs = (*(*pstate).pcf).cf_rv.jd
	fitdat = (*(*pstate).pcf).cf_rv.mnvel
	err = (*(*pstate).pcf).cf_rv.errvel
	
   pararr.value = MPFITFUN('rv_mpfit_rv',fitobs, fitdat, $
    err, parinfo=pararr, covar=covar, perror=perror, $
	 yfit=syn_fit, dof=dof, functargs=(*(*pstate).pfunctargs) )
  (*pstate).ndof = n_elements(fitobs) - dof
	
 chi_sq=total(((fitdat-syn_fit)/err)^2)/(n_elements(fitobs) - $
   (*pstate).ndof)
   
 rms = stddev(fitdat-syn_fit)  

 print, 'start inc: ', startinc
 print, 'start bigom: ', startbigo
 print, 'chi final: ', chi_sq
 
 ast_pars = (*pstate).par1.value

 t_off = (*(*pstate).pcf).time_offset

 tp_ast = (*pstate).par1[4].value + (*pstate).par1[0].value * $
  (1. + fix((t_off - (*pstate).par1[4].value)/ $
  (*pstate).par1[0].value)) - t_off

 
 n_planets = (*(*pstate).pfunctargs).n_planets
 
print, ' '
print, '-----------------------------------'
print, 'FINAL ORBITAL PARAMETERS'
print, '-----------------------------------'
for pl=0,n_planets-1 do begin
print, 'Period (days)              =', pararr[7*pl+0].value, $
		 '+/- ', strt(perror[7*pl+0])
print, 'Period (years)             =', pararr[7*pl+0].value/365.2564d, $
		 '+/- ', strt(perror[7*pl+0]/365.2564d)
print, 'Mass (Earth masses)        =', pararr[7*pl+1].value, $
		 '+/- ', strt(perror[7*pl+1])
print, 'Eccentricity               =', pararr[7*pl+2].value, $
		 '+/- ', strt(perror[7*pl+2])
print, 'Inclination (degrees)      =', pararr[7*pl+3].value, $
		 '+/- ', strt(perror[7*pl+3])
print, 'Omega (degrees)            =', pararr[7*pl+4].value, $
		 '+/- ', strt(perror[7*pl+4])
print, 'omega (degrees)            =', pararr[7*pl+5].value, $
		 '+/- ', strt(perror[7*pl+5])
print, 'Time of peri rv (days)     =', pararr[7*pl+6].value, $
		 '+/- ', strt(perror[7*pl+6])
print, ' '
endfor
print, ' '
print, 'delta-parallax (microas)      =',pararr[7*n_planets+9].value, $
		 '+/- ', strt(perror[7*n_planets+9])
print, 'delta-prprx (mas/year)        =',pararr[7*n_planets+10].value, $
		 '+/- ', strt(perror[7*n_planets+10])
print, 'delta-prpry (mas/year)        =',pararr[7*n_planets+11].value, $
		 '+/- ', strt(perror[7*n_planets+11])
print, ' '
print, 'Offset on astx (micro as)      =',pararr[7*n_planets].value, $
		 '+/- ', strt(perror[7*n_planets+0])
print, 'Offset on asty (micro as)      =',pararr[7*n_planets+1].value, $
		 '+/- ', strt(perror[7*n_planets+1])
print, 'Offset on rv (m/s)             =',pararr[7*n_planets+2].value, $
		 '+/- ', strt(perror[7*n_planets+2])
print, 'Slope on astx (micro as/year)  =',pararr[7*n_planets+3].value, $
		 '+/- ', strt(perror[7*n_planets+3])
print, 'Slope on asty (micro as/year)  =',pararr[7*n_planets+4].value, $
		 '+/- ', strt(perror[7*n_planets+4])
print, 'Slope on rv (m/s /year)        =',pararr[7*n_planets+5].value, $
		 '+/- ', strt(perror[7*n_planets+5])
print, 'Curve on astx (micro as/year^2)=',pararr[7*n_planets+6].value, $
		 '+/- ', strt(perror[7*n_planets+6])
print, 'Curve on asty (micro as/year^2)=',pararr[7*n_planets+7].value, $
		 '+/- ', strt(perror[7*n_planets+7])
print, 'Curve on rv (m/s /year^2)      =',pararr[7*n_planets+8].value, $
		 '+/- ', strt(perror[7*n_planets+8])
		
print, ' '
print, 'chi square', chi_sq

print, 'FOR PRPR RA JOT: ', (*(*pstate).pcf).prpr[0] + $
	pararr[7*n_planets+10].value
print, 'FOR PRPR DEC JOT: ', (*(*pstate).pcf).prpr[1] + $
	pararr[7*n_planets+11].value
print, 'FOR PARALLAX JOT: ', (*(*pstate).pcf).PLX + $
	pararr[7*n_planets+9].value


 ;Now to put the pararr back into the state variable and store 
 ;the uncertainties:
 kfme_retrieve_par, pstate, pararr, perror

 print, 'COMPLETED MPFITFUN AND SENT TO kfme_DOFIT'

  before_update_in_run:
  ;Update the text fields with the new parameters:
  kfme_update_fields, pstate
  
end ;kfme_cyclerun.pro

pro kfme_rvlin, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  widget_control, event.id, get_value=oldway
  
pararr = kfme_create_par(pstate)
opararr = pararr

;TAKE CARE OF THE DEWARS:
;DEWAR 24:
;IF THERE WASN'T A DEWAR 24 (I.E., KECK)
;THEN DON'T TREAT IT AS A FREE PARAMETER:
npararr = n_elements(pararr)
;if ((*(*(*pstate).pfunctargs).dew24) le 0) then begin
;pararr[npararr-2].fixed = 1
;endif;dew24
;
;THE SAME GOES FOR DEW 39:
;if ((*(*(*pstate).pfunctargs).dew39) lt 0) then begin
;pararr[npararr-1].fixed = 1
;endif;dew39

print, 'The parameters before running...'
print, '---------------------------------------'
for q=0, n_elements(pararr.value)-1 do begin
 print, 'Name of '+strt(q)+':       ', pararr[q].parname
 print, 'Value:      ', strt(pararr[q].value)
 print, 'Fixed:      ', strt(pararr[q].fixed)
 print, 'Lo Limited: ', strt(pararr[q].limited[0])
 print, 'Lo Limit:   ', strt(pararr[q].limits[0])
 print, 'Hi Limited: ', strt(pararr[q].limited[1])
 print, 'Hi Limit:   ', strt(pararr[q].limits[1])
 print, 'Step:       ', strt(pararr[q].step)
 print, '- - - - - - - - - - - - - - - - - - - - '
 if ( (pararr[q].value le pararr[q].limits[0]) OR $
     (pararr[q].value ge pararr[q].limits[1]) ) then begin
     print, 'CHECK YOUR PARAMETERS AND LIMITS!!'
     print, 'AUTOFIXING THEM BEFORE RUN.........'
	  if (pararr[q].value le pararr[q].limits[0]) then begin
	  pararr[q].value +=(pararr[q].limits[0]-pararr[q].value)+.1
	  endif else pararr[q].value -= $
			(pararr[q].value-pararr[q].limits[1])+.1
	  print, pararr[q].parname, ' is now: ', pararr[q].value
 endif
 
 if ((pararr[q].parname eq 'Offset RV (m/s)') and $
    pararr[q].fixed and (pararr[q].value eq 0d)) then begin
    print, 'WARNING! THIS WILL FAIL!'
    print, 'You cannot have the offset set to zero and '
    print, 'fixed at the same time due to an issue with RVLIN. '
    print, 'Set the offset to 0.0001 if you would like to fix it '
    print, 'to zero'
    print, 'Type ".c" to continue.'
    stop
 endif   
 
 endfor
print, '---------------------------------------'


  astx = (*(*pstate).pcf).cf_ast.astx
  errx = (*(*pstate).pcf).cf_ast.errx
  asty = (*(*pstate).pcf).cf_ast.asty
  erry = (*(*pstate).pcf).cf_ast.erry

	;JUST RV
	fitobs = (*(*pstate).pcf).cf_rv.jd
	fitdat = (*(*pstate).pcf).cf_rv.mnvel
	err = (*(*pstate).pcf).cf_rv.errvel
	
;   pararr.value = MPFITFUN('rvlin',fitobs, fitdat, $
;    err, parinfo=pararr, covar=covar, perror=perror, $
;	 yfit=syn_fit, dof=dof, $
;	 functargs=(*(*pstate).pfunctargs), maxiter = 1000., $
;	 XTOL = 1d-12, errmsg = errmsg)
	 
	 orbpar=pararr.value

m_star = (*(*pstate).pfunctargs).m_star

n_planets = (*(*pstate).pfunctargs).n_planets

indx=findgen(n_planets)

period=orbpar[indx*5]
a_pl=((period/365.2564d)^2*m_star)^(1./3.)
m_pl_earth = orbpar[indx*5+1]
inc = 89.9d
ecc = orbpar[indx*5+2]

k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)

orbel=dblarr(n_planets*7)
orbel[7*indx] = orbpar[indx*5] ;p
orbel[7*indx+1] = orbpar[5*indx+4] ;tp
orbel[7*indx+2] = orbpar[5*indx+2] ;e
orbel[7*indx+3] = orbpar[5*indx+3] ;om
orbel[7*indx+4] = k ;k
orbel[5] = orbpar[5*n_planets];gam
orbel[6] = orbpar[5*n_planets+1];dvdt

fixed=dblarr(n_planets*7)
fixed[7*indx] = pararr[5*indx].fixed
fixed[7*indx+1] = pararr[5*indx+4].fixed
fixed[7*indx+2] = pararr[5*indx+2].fixed
fixed[7*indx+3] = pararr[5*indx+3].fixed
fixed[7*indx+4] =  pararr[5*indx+1].fixed
fixed[5] = pararr[5*n_planets].fixed;gam
fixed[6] = pararr[5*n_planets+1].fixed;dvdt



;number of degrees of freedom = 5 free orbital
;parameters + offset and slope - 
;number of fixed parameters 
;as in the equation:
;http://en.wikipedia.org/wiki/Goodness_of_fit

(*pstate).ndof = n_planets*5 + 2 - total(fixed)


    orbel=rv_fit_mp(fitobs,fitdat, err, fixed=fixed, $
					yfit=syn_fit,tps=max(fitobs),perror=perror, $
					chi=chi_sq,rms=rms, orbel=orbel);, /plotfit)

;stop
pararr[5*indx].value = orbel[7*indx] ;p
pararr[5*indx+4].value = orbel[7*indx+1] ;tp
pararr[5*indx+2].value = orbel[7*indx+2] ;ecc
pararr[5*indx+3].value = orbel[7*indx+3] ;om
k = orbel[7*indx+4]
pararr[5*n_planets].value = orbel[5] ;gam
pararr[5*n_planets+1].value = orbel[6] ;dvdt

period = pararr[5*indx].value
ecc = pararr[5*indx+2].value
a_pl=((period/365.2564d)^2*m_star)^(1./3.)

mnum = period * m_star * sqrt(1d - ecc^2) * K
mden = a_pl * 2d * !dpi * sin(inc*!dtor)
m_pl_earth = mnum/mden * 19200./100000.
pararr[5*indx+1].value = m_pl_earth

  ;perror only returns the error for p, 
  perrorl = dblarr(n_elements(pararr.value))

if ~pararr[0].fixed then begin
  perrorl[5*indx] = perror[3*indx]
  perrorl[5*indx+2] = perror[3*indx+2]
  perrorl[5*indx+4] = perror[3*indx+1]
  
endif
  perror=perrorl

;DEGREES OF FREEDOM, NU = N - n - 1 WHERE:
;N IS THE NUMBER OF OBSERVATIONS
;n IS THE NUMBER OF FITTED PARAMETERS


 chi_sq=total(((fitdat-syn_fit)/err)^2)/(n_elements(fitobs) - $
   (*pstate).ndof)

 ;rms = stddev(fitdat-syn_fit)  
 nfree = (*pstate).ndof
 rms = sqrt(total((fitdat - syn_fit)^2)/(n_elements(fitobs)-nfree))


 
 n_planets = (*(*pstate).pfunctargs).n_planets
 
print, ' '
print, '-----------------------------------'
print, 'FINAL ORBITAL PARAMETERS'
print, '-----------------------------------'
for pl=0,n_planets-1 do begin
print, 'Period (days)              =', pararr[5*pl+0].value, $
		'+/- ', strt(perror[5*pl+0])
print, 'Period (years)             =', pararr[5*pl+0].value/365.2564d, $
		'+/- ', strt(perror[5*pl+0]/365.2564d)
print, 'Mass (Earth masses)        =', pararr[5*pl+1].value, $
		'+/- ', strt(perror[5*pl+1])
print, 'Eccentricity               =', pararr[5*pl+2].value, $
		'+/- ', strt(perror[5*pl+2])
print, 'omega (degrees)            =', pararr[5*pl+3].value, $
		'+/- ', strt(perror[5*pl+3])
print, 'Time of peri rv (days)     =', pararr[5*pl+4].value, $
		'+/- ', strt(perror[5*pl+4])
print, ' '
endfor
print, ' '
print, 'Offset on rv (m/s)             =',pararr[5*n_planets].value, $
		'+/- ', strt(perror[5*n_planets])
print, 'Slope on rv (m/s /year)        =',pararr[5*n_planets+1].value, $
		'+/- ', strt(perror[5*n_planets+1])
print, 'Curve on rv (m/s /year^2)      =',pararr[5*n_planets+2].value, $
		'+/- ', strt(perror[5*n_planets+2])
print, 'Dewar 24 Offset (m/s)             =', $
		 pararr[5*n_planets+3].value, $
		'+/- ', strt(perror[5*n_planets+3])
print, 'Dewar 39 Offset (m/s)             =', $
		 pararr[5*n_planets+4].value, $
		'+/- ', strt(perror[5*n_planets+4])
	  
print, ' '
print, 'chi square: ', chi_sq
print, 'number of degrees of freedom: ', (*pstate).ndof
print, 'rms is: ', rms



 ;Now to put the pararr back into the state variable and store 
 ;the uncertainties:
 kfme_retrieve_par, pstate, pararr, perror

 print, 'COMPLETED MPFITFUN AND SENT TO kfme_DOFIT'
;stop
  ;Update the text fields with the new parameters:
  kfme_update_fields, pstate
  
end ;kfme_rvlin.pro

pro kfme_monte, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate

star = (*(*pstate).pfunctargs).extitle
print, 'the star is: ', star
ip = strmid(star, 2, strlen(star))
pararr = kfme_create_par(pstate)
incs=1000


nplanets=(*(*pstate).pfunctargs).n_planets
print, 'the # of planets is: ', nplanets
stop
   transit_monte, star = star, lick = nokeck, incs = incs, $
   database = datentry, debug = debug, frz_ecc = frz_ecc, $
   frz_om=frz_om, nplanets=nplanets, planetid=ip, $
   /normanimation, /widget, pararr=pararr, pstate=pstate


   (*pstate).ptransit = ptr_new(datentry, /allocate)
   loadct, 39, /silent

stop


end;kfme_monte.pro

pro kfme_monte2, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate

Ntrial=1d3
star = (*(*pstate).pfunctargs).extitle
print, 'the star is: ', star
ip = strmid(star, 2, strlen(star))

pararr = kfme_create_par(pstate)
orbpar=pararr.value
npararr = n_elements(pararr)


print, 'The parameters before running...'
print, '---------------------------------------'
for q=0, n_elements(pararr.value)-1 do begin
 print, 'Name of '+strt(q)+':       ', pararr[q].parname
 print, 'Value:      ', strt(pararr[q].value)
 print, 'Fixed:      ', strt(pararr[q].fixed)
 print, 'Lo Limited: ', strt(pararr[q].limited[0])
 print, 'Lo Limit:   ', strt(pararr[q].limits[0])
 print, 'Hi Limited: ', strt(pararr[q].limited[1])
 print, 'Hi Limit:   ', strt(pararr[q].limits[1])
 print, 'Step:       ', strt(pararr[q].step)
 print, '- - - - - - - - - - - - - - - - - - - - '
 if ( (pararr[q].value le pararr[q].limits[0]) OR $
     (pararr[q].value ge pararr[q].limits[1]) ) then begin
     print, 'CHECK YOUR PARAMETERS AND LIMITS!!'
     print, 'AUTOFIXING THEM BEFORE RUN.........'
	  if (pararr[q].value le pararr[q].limits[0]) then begin
	  pararr[q].value +=(pararr[q].limits[0]-pararr[q].value)+.1
	  endif else pararr[q].value -= $
			(pararr[q].value-pararr[q].limits[1])+.1
	  print, pararr[q].parname, ' is now: ', pararr[q].value
 endif
 
 if ((pararr[q].parname eq 'Offset RV (m/s)') and $
    pararr[q].fixed and (pararr[q].value eq 0d)) then begin
    print, 'WARNING! THIS WILL FAIL!'
    print, 'You cannot have the offset set to zero and '
    print, 'fixed at the same time due to an issue with RVLIN. '
    print, 'Set the offset to 0.0001 if you would like to fix it '
    print, 'to zero'
    print, 'Type ".c" to continue.'
    stop
 endif   
 
 endfor
print, '---------------------------------------'

	 orbpar=pararr.value

m_star = (*(*pstate).pfunctargs).m_star
nplanets=(*(*pstate).pfunctargs).n_planets
n_planets=nplanets
indx=findgen(n_planets)

period=orbpar[indx*5]
a_pl=((period/365.2564d)^2*m_star)^(1./3.)
m_pl_earth = orbpar[indx*5+1]
inc = 89.9d
ecc = orbpar[indx*5+2]

k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)

orbel=dblarr(n_planets*7)
orbel[7*indx] = orbpar[indx*5] ;p
orbel[7*indx+1] = orbpar[5*indx+4] ;tp
orbel[7*indx+2] = orbpar[5*indx+2] ;e
orbel[7*indx+3] = orbpar[5*indx+3] ;om
orbel[7*indx+4] = k ;k
orbel[5] = orbpar[5*n_planets];gam
orbel[6] = orbpar[5*n_planets+1];dvdt

;store the best-fit solution:
orbel_bf = orbel

fixed=dblarr(n_planets*7)
fixed[7*indx] = pararr[5*indx].fixed
fixed[7*indx+1] = pararr[5*indx+4].fixed
fixed[7*indx+2] = pararr[5*indx+2].fixed
fixed[7*indx+3] = pararr[5*indx+3].fixed
fixed[7*indx+4] =  pararr[5*indx+1].fixed
fixed[5] = pararr[5*n_planets].fixed;gam
fixed[6] = pararr[5*n_planets+1].fixed;dvdt

print, '*********************************************'
print, 'THE FIXED PARAMETERS ARE:'
print, fixed
print, '*********************************************'
;stop
thvel1 = rv_mpfit_rv((*(*pstate).pcf).cf_rv.jd, pararr.value, $
_EXTRA = (*(*pstate).pfunctargs))

fitobs = (*(*pstate).pcf).cf_rv.jd
fitdat = (*(*pstate).pcf).cf_rv.mnvel
err = (*(*pstate).pcf).cf_rv.errvel
errorig = err
resid = fitdat-thvel1

npts = n_elements((*(*pstate).pcf).cf_rv)


new_vel=fltarr(npts,Ntrial)
new_err=fltarr(npts,Ntrial)
plot, fitobs, resid, ps=8
if keyword_set(debug) then stop
for i=0,Ntrial-1 do begin
      ;repl = randoma(min=0, max=npts-1, num=npts)
     repl=long(randomu(seed,npts)*npts) ;index for replacement
        new_vel[*,i] = resid[repl] + thvel1
        new_err[*,i] = err[repl]
        ;print, 'scrambling residuals: ', i+1, ' of ', ntrial
     ;stop
     oplot, fitobs, resid[repl], ps=8, color=i*23.
     ;wait, 0.01
     ;stop
end ;for  

outarr=fltarr(7*n_planets,Ntrial)
chiarr = dblarr(Ntrial)

eccmax = dblarr(n_planets)
eccmax[0] = pararr[2].limits[1]
if n_planets gt 1 then eccmax[*] = pararr[7].limits[1]
;stop
for idxmte=0,Ntrial-1 do begin

	print, '*********************************************************'
	print, 'Now on run:', idxmte,' of ',strt(ntrial),' in MONTE_NEW.PRO'
	print, '*********************************************************'
	;set the starting guess back to the best-fit solution:
	orbel = orbel_bf
	
    fitdat=new_vel[*,idxmte]
    err=new_err[*,idxmte]
	 orbel=rv_fit_mp(fitobs,fitdat, err, fixed=fixed, $
		yfit=syn_fit,tps=max(fitobs),perror=perror, $
		chi=chi_sq,rms=rms, orbel=orbel, eccmax = eccmax);, /plotfit)

    print, 'CHI SQUARED IS: ', chi_sq
    chiarr[idxmte] = chi_sq

	 outarr[*, idxmte] = orbel	
	 ;print, orbel
	 ;stop
endfor

print, 'Tp Nan?', where(~finite(outarr[1, *]))
nans = where(~finite(outarr[1, *]), nnans)
notnans = where(finite(outarr[1, *]))

newoutarr=fltarr(7*n_planets, (n_elements(outarr[1, *]) - nnans))
for i=0,7*n_planets-1 do begin
    newoutarr[i,*]=outarr[i,notnans]
end

print, 'Nan?', where(~finite(newoutarr))

t_center = dblarr(ntrial, nplanets)
t_duration = dblarr(ntrial, nplanets)


;set parconstraint to 1 to exclude obvious outliers, 
;such as the case of HD 211810
parconstraint = 1
if parconstraint then begin
  ;use the size of newoutarr to create a new array 
  ;excluding the outlying elements:
  sznoa = size(newoutarr)
  ;IDL> print, sznoa
  ;         2          14        1000           4       14000
  ;create an incrementing array the size of the # of realizations 
  ;for indexing:
  gdels = lindgen(sznoa[2])
  ;only keep realizations where the ecc < 0.94:
  for ii=0, n_planets-1 do begin
	;cycle through each planet, removing the realizations
	;where any one of the planets has an e > that desired:
	gdels = where(newoutarr[ii*7 + 2,gdels] lt 0.94, nes)
  endfor
  ;create a new array that will have only the good values:
  neweroutarr = dblarr(sznoa[1], nes)
  ;now save the good elements into that new array:
  neweroutarr = newoutarr[*,gdels]
  ;finally rename it to what will be used from this point on:
  newoutarr = neweroutarr
  print, 'Number of realizations discarded: ', sznoa[2]-nes
  print, 'Fraction of realizations remaining: ', double(nes)/sznoa[2]
endif;pc & i=0

for i=0, n_planets-1 do begin

medianper=strcompress(string(median(newoutarr[i*7 + 0,*])),/remove_all)
pererr=strcompress(string(stdev(newoutarr[i*7 + 0,*])),/remove_all)

massarr = mpf_mass(newoutarr[i*7 + 0,*], m_star, newoutarr[i*7 + 4,*], $
            newoutarr[i*7 + 2,*], inc)

medianmass=strcompress(string(median(massarr)),/remove_all)
masserr=strcompress(string(stdev(massarr)),/remove_all)

mediantp=strcompress(string(median(newoutarr[i*7 + 1,*])),/remove_all)
tperr=strcompress(string(stdev(newoutarr[i*7 + 1,*])),/remove_all)

eccarr = newoutarr[i*7 + 2,*]
eccarr = transpose(eccarr)
medianecc=strcompress(string(median(eccarr)),/remove_all)
eccerr=strcompress(string(stdev(eccarr)),/remove_all)

medianom=strcompress(string(median(newoutarr[i*7 + 3,*])),/remove_all)
omerr=strcompress(string(stdev(newoutarr[i*7 + 3,*])),/remove_all)

karr = newoutarr[i*7 + 4,*]
karr = transpose(karr)
mediank=strcompress(string(median(karr)),/remove_all)
kerr=strcompress(string(stdev(karr)),/remove_all)

mediangam=strcompress(string(median(newoutarr[i*7 + 5,*])),/remove_all)
gamerr=strcompress(string(stdev(newoutarr[i*7 + 5,*])),/remove_all)

mediandvdt=strcompress(string(median(newoutarr[i*7 + 6,*])),/remove_all)
dvdterr=strcompress(string(stdev(newoutarr[i*7 + 6,*])),/remove_all)

arelarr = ((newoutarr[i*7 + 0,*]/365.2564d)^2*m_star)^(1./3.)
medianarel=strcompress(string(median(arelarr)),/remove_all)
arelerr=strcompress(string(stdev(arelarr)),/remove_all)

transitpars, $
per = newoutarr[i*7 + 0,*], $
om = newoutarr[i*7 + 3,*], $
ecc = newoutarr[i*7 + 2,*], $
Tp = newoutarr[i*7 + 1,*], $
msini = massarr, $
t_cen = t_center, $
t_dur = t_duration


mediantcen=strcompress(string(median(t_center)),/remove_all)

x = where(t_center lt (mediantcen - medianper/2d), offbyper)
if offbyper gt 0 then t_center[x] += period[i]

x = where(t_center gt  (mediantcen + medianper/2d), offbyper)
if offbyper gt 0 then t_center[x] -= period[i]

mediantcen=strcompress(string(median(t_center)),/remove_all)
tcenerr=strcompress(string(stdev(t_center)),/remove_all)

mediantdur=strcompress(string(median(t_duration)),/remove_all)
tdurerr=strcompress(string(stdev(t_duration)),/remove_all)

bfk = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)

;i is the planet number:
case i of
  0: vals = (*pstate).pars.par1.value
  1: vals = (*pstate).pars.par2.value
  2: vals = (*pstate).pars.par3.value
  3: vals = (*pstate).pars.par4.value
  4: vals = (*pstate).pars.par5.value
  5: vals = (*pstate).pars.par6.value
  6: vals = (*pstate).pars.par7.value
endcase

bfper = vals[0]
bfmsini = vals[1]
bfecc = vals[2] 
bfom = vals[3] 
bftp = vals[4] 
bfgam = vals[5] 
bfdvdt = vals[6] 

bfapl = a_pl[i]
bfk = bfk[i]

transitpars, $
per = bfper, $
om = bfom, $
ecc = bfecc, $
Tp = bftp, $
msini = bfmsini / 5.9742d24, $
t_cen = bftcen, $
t_dur = bftdur
bft_cenjd = jul2cal(bftcen + 2.44d6)

transit_prob2, $
a_au = bfapl, $
r_rsun = r_rsun, $
ecc = bfecc, $
om = bfom, $
tprob = bftprob, $
/noprint

print, '*******************************************'
print, 'Best-fit parameters with Bootstrap MC '
print, 'uncertainties for planet ',strt(i+1)
print, '*******************************************'
print,'Per: '+strt(bfper)+' +/- '+pererr, ' days'
print,'Tp: '+strt(bftp)+' +/- '+tperr, ' HJD'
print,'Ecc: '+strt(bfecc)+' +/- '+eccerr
print,'Om: '+strt(bfom)+' +/- '+omerr, ' degrees'
print,'K: '+strt(bfk)+' +/- '+kerr, ' m/s'
print,'gam: '+strt(bfgam)+' +/- '+gamerr, ' m/s'
print,'dvdt: '+strt(bfdvdt)+' +/- '+dvdterr, ' m/s/yr'
print,'msini: ', strt(bfmsini)+' +/- '+masserr, ' M_Earth'
print,'a_pl: ', strt(bfapl)+' +/- '+arelerr, ' AU'
print,'t_center: ', strt(bftcen)+' +/- '+tcenerr, ' HJD'
print,'t_dur: ', strt(bftdur)+' +/- '+strt(tdurerr, f='(F10.5)')+' hrs'
print, 'tprob: ', strt(bftprob), ' %'
print, ' t_c: ', bft_cenjd

print, '' & print, ''

;endif

if i eq 0 then begin
  (*pstate).pars.par1[0].error = pererr
  (*pstate).pars.par1[1].error = masserr
  (*pstate).pars.par1[2].error = eccerr
  (*pstate).pars.par1[3].error = omerr
  (*pstate).pars.par1[4].error = tperr
  (*pstate).pars.par1[5].error = gamerr
  (*pstate).pars.par1[6].error = dvdterr
  (*pstate).pars.par1[10].error = kerr
  (*pstate).pars.par1[11].error = arelerr
endif ;store uncertainties for planet 1

if i eq 1 then begin
  (*pstate).pars.par2[0].error = pererr
  (*pstate).pars.par2[1].error = masserr
  (*pstate).pars.par2[2].error = eccerr
  (*pstate).pars.par2[3].error = omerr
  (*pstate).pars.par2[4].error = tperr
  (*pstate).pars.par2[5].error = kerr
  (*pstate).pars.par2[6].error = arelerr
endif ;store uncertainties for planet 2

if i eq 2 then begin
  (*pstate).pars.par3[0].error = pererr
  (*pstate).pars.par3[1].error = masserr
  (*pstate).pars.par3[2].error = eccerr
  (*pstate).pars.par3[3].error = omerr
  (*pstate).pars.par3[4].error = tperr
  (*pstate).pars.par3[5].error = kerr
  (*pstate).pars.par3[6].error = arelerr
endif ;store uncertainties for planet 3

if i eq 3 then begin
  (*pstate).pars.par4[0].error = pererr
  (*pstate).pars.par4[1].error = masserr
  (*pstate).pars.par4[2].error = eccerr
  (*pstate).pars.par4[3].error = omerr
  (*pstate).pars.par4[4].error = tperr
  (*pstate).pars.par4[5].error = kerr
  (*pstate).pars.par4[6].error = arelerr
endif ;store uncertainties for planet 4

if i eq 4 then begin
  (*pstate).pars.par5[0].error = pererr
  (*pstate).pars.par5[1].error = masserr
  (*pstate).pars.par5[2].error = eccerr
  (*pstate).pars.par5[3].error = omerr
  (*pstate).pars.par5[4].error = tperr
  (*pstate).pars.par5[5].error = kerr
  (*pstate).pars.par5[6].error = arelerr
endif ;store uncertainties for planet 5

if i eq 5 then begin
  (*pstate).pars.par6[0].error = pererr
  (*pstate).pars.par6[1].error = masserr
  (*pstate).pars.par6[2].error = eccerr
  (*pstate).pars.par6[3].error = omerr
  (*pstate).pars.par6[4].error = tperr
  (*pstate).pars.par6[5].error = kerr
  (*pstate).pars.par6[6].error = arelerr
endif ;store uncertainties for planet 6

if i eq 6 then begin
  (*pstate).pars.par7[0].error = pererr
  (*pstate).pars.par7[1].error = masserr
  (*pstate).pars.par7[2].error = eccerr
  (*pstate).pars.par7[3].error = omerr
  (*pstate).pars.par7[4].error = tperr
  (*pstate).pars.par7[5].error = kerr
  (*pstate).pars.par7[6].error = arelerr
endif ;store uncertainties for planet 7



;print,'Mean RMS: ',median(newoutarr[7,*])
;print,'Mean chisq: ',median(newoutarr[8,*])

endfor

;**********************************************************************
;                     BEGIN CONTOUR SECTION
;**********************************************************************
loadct, 39, /silent
plot, eccarr, karr, ps=8, xtitle='Eccentricity', $
ytitle='K [m s!u-1!n]'

noasz = size(newoutarr)
for i=0, noasz[2] - 1 do oplot, [eccarr[i], eccarr[i]], $
[karr[i],karr[i]], color=round(chiarr[i]*100d), ps=8

wait, 3
xnel = 5d1
ynelmax = 5d1
buf = 2d
ynel = ynelmax < CEIL((max(karr)))
yfac = CEIL((max(karr) ))/ynelmax > 1d
pargrid = dblarr(xnel+ 2d * buf, ynel + 2d * buf)
numgrid =pargrid
chigrid = pargrid
meangrid = chigrid

for i=0,noasz[2]-1 do begin
  numgrid[round(eccarr[i]*xnel) +buf, floor((karr[i] )/yfac) +buf] += 1
  chigrid[round(eccarr[i]*xnel) + buf, floor((karr[i])/yfac) + buf] += $
	 chiarr[i]
endfor


values = where(numgrid ne 0)
meangrid[values] = chigrid[values] / numgrid[values]

;**********************************************************************
;    NOW TO PLOT THE CONTOURS FOR CHI SQ:
;**********************************************************************
numlvls = 20
levels = (dindgen(numlvls-1)+ 1d)*max(meangrid*100)/numlvls

contour, meangrid*1d2, dindgen(xnel+2d * buf)/(xnel - 1d) - buf/xnel, $
dindgen(ynel + 2d * buf)*yfac - buf*yfac, $
xtitle='Eccentricity', $
ytitle='K [m s!u-1!n]', $
/xsty, xran=[-buf/xnel, 1 + 2d / xnel], $
/yst, yran=[-buf*yfac, max(karr) + buf*yfac], $
c_colors = dindgen(numlvls)*254/numlvls, /fill, $
POSITION=[0.3, 0.1, 0.95, 0.95], $
levels=levels

 ncolors=254.
 loc = [0.1, 0.10, 0.15, 0.95]
 bar = REPLICATE(1B, 10) # BINDGEN(256) 
 xsize = (loc(2) - loc(0)) * !D.X_VSIZE
 ysize = (loc(3) - loc(1)) * !D.Y_VSIZE 
 xstart = loc(0) * !D.X_VSIZE
 ystart = loc(1) * !D.Y_VSIZE 
 bar = BYTSCL(bar, TOP=ncolors-1)
 IF !D.NAME EQ 'PS' THEN $
      TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize ELSE $
      TV, CONGRID(bar, xsize, ysize), xstart, ystart
 PLOTS, [loc(0), loc(0), loc(2), loc(2), loc(0)], $
       [loc(1), loc(3), loc(3), loc(1), loc(1)], /NORMAL
       
 minrange = 0
 maxrange = max(meangrid)
 title = ''
 ytitle=greek('chi')+'!d'+greek('nu')+'!n'+'!u2!n'
 position=loc
 divisions=6
 PLOT, [minrange, maxrange], [minrange, maxrange], /nodata, $
 XTICKS=1, YTICKS=divisions, XSTYLE=1, YSTYLE=1, TITLE=title, $
 POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
 YTICKFORMAT=format, XTICKFORMAT='(A1)', YTICKLEN=ticklen, $
 YRANGE=[minrange, maxrange], FONT=font, YMinor=minor, _STRICT_EXTRA=extra, $
 YTICKNAME=ticknames, YLOG=ylog, XTITLE="", YTITLE=ytitle

;**********************************************************************
;NOW TO PLOT THE CONTOURS FOR # REALIZATIONS IN THAT PARAMETER SPACE:
;**********************************************************************

if !d.name eq 'PS' then ylow = 0.2 else ylow = 0.1
if !d.name eq 'PS' then xlow = 0.35 else xlow = 0.3
levels2 = (dindgen(numlvls-1)+ 1d)*max(numgrid*10)/numlvls
contour, numgrid*10d, dindgen(xnel+2d * buf)/(xnel - 1d) - buf/xnel, $
dindgen(ynel + 2d * buf)*yfac - buf*yfac, $
xtitle='Eccentricity', $
ytitle='K [m s!u-1!n]', $
/xsty, xran=[-buf/xnel, 1 + 2d / xnel], $
/yst, yran=[-buf*yfac, max(karr) + buf*yfac], $
c_colors = dindgen(numlvls)*254/numlvls, /fill, $
POSITION=[xlow, ylow, 0.95, 0.95], $
levels=levels2

 ncolors=254.
 loc = [xlow-0.2, ylow, xlow-0.15, 0.95]
 bar = REPLICATE(1B, 10) # BINDGEN(256) 
 xsize = (loc(2) - loc(0)) * !D.X_VSIZE
 ysize = (loc(3) - loc(1)) * !D.Y_VSIZE 
 xstart = loc(0) * !D.X_VSIZE
 ystart = loc(1) * !D.Y_VSIZE 
 bar = BYTSCL(bar, TOP=ncolors-1)
 IF !D.NAME EQ 'PS' THEN $
      TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize ELSE $
      TV, CONGRID(bar, xsize, ysize), xstart, ystart
 PLOTS, [loc(0), loc(0), loc(2), loc(2), loc(0)], $
       [loc(1), loc(3), loc(3), loc(1), loc(1)], /NORMAL
       
 maxrange = max(numgrid)
 title = ''
 ytitle='# Realizations'
 PLOT, [minrange, maxrange], [minrange, maxrange], /nodata, $
 XTICKS=1, YTICKS=divisions, XSTYLE=1, YSTYLE=1, TITLE=title, $
 POSITION=loc, COLOR=color, CHARSIZE=charsize, /NOERASE, $
 YTICKFORMAT=format, XTICKFORMAT='(A1)', YTICKLEN=ticklen, $
 YRANGE=[minrange, maxrange], FONT=font, YMinor=minor, _STRICT_EXTRA=extra, $
 YTICKNAME=ticknames, YLOG=ylog, XTITLE="", YTITLE=ytitle

end;kfme_monte2.pro

pro kfme_saveresid, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
 astx = (*(*pstate).pcf).cf_ast.astx
 asty = (*(*pstate).pcf).cf_ast.asty
 rv = (*(*pstate).pcf).cf_rv.mnvel
 
 (*(*pstate).pcf).cf_ast.astx = (*(*pstate).pcf_resid).cf_ast.astx
 (*(*pstate).pcf).cf_ast.asty = (*(*pstate).pcf_resid).cf_ast.asty
 (*(*pstate).pcf).cf_rv.mnvel =  (*(*pstate).pcf_resid).cf_rv.mnvel
 
 state = (*pstate)
 rsdnm = nextnameeps('cfresid')

 (*(*pstate).pcf).cf_ast.astx = astx
 (*(*pstate).pcf).cf_ast.asty = asty
 (*(*pstate).pcf).cf_rv.mnvel = rv
save, state, filename = rsdnm

 print, 'YOUR STATE STRUCTURE W/ RESIDUALS HAVE BEEN SAVED TO: ', rsdnm

end;kfme_saveresid.pro

pro kfme_restoreresid, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
 ;User select the name of the residuals to restore:
 newname = dialog_pickfile(filter='*.dat', title= $
 'Please Select The Residuals to Restore')
 
 if newname ne '' then begin
 restore, newname

   state = {ast:state.ast, $
 			 botrow:(*pstate).botrow, $
 			 combperg:state.combperg, $
 			 controlbar:(*pstate).controlbar, $
 			 connect:state.connect, $
 			 controlbase:(*pstate).controlbase, $
 			 dof:state.ndof, $
 			 draw:(*pstate).draw, $
 			 fixbttns:(*pstate).fixbttns, $
 			 fitplarr:state.fitplarr, $
 			 fitplbttns:(*pstate).fitplbttns, $
 			 jitternum:state.jitternum, $
 			 linestyle:state.linestyle, $
 			 multith:state.multith, $
 			 p_orig:state.p_orig, $
 			 par1:state.par1, $
 			 pars:state.pars, $
 			 pcf:state.pcf, $
 			 pcf_resid:state.pcf_resid, $
 			 pcfname:state.pcfname, $
 			 pdatls:(*pstate).pdatls, $
 			 pdata:state.pdata, $
 			 pdatname:state.pdatname, $
 			 pfunctargs:state.pfunctargs, $
 			 plot_time_study:state.plot_time_study, $
 			 previousjitter:state.previousjitter, $
 			 psym:state.psym, $
 			 ptransit:state.ptransit, $
 			 rv:state.rv, $
 			 scroll:(*pstate).scroll, $
 			 sliderflds:(*pstate).sliderflds, $
 			 tfine:(*pstate).tfine, $
 			 togerr:state.togerr, $
 			 txtflds:(*pstate).txtflds, $
 			 toprow:(*pstate).toprow, $
 			 win_id:(*pstate).win_id, $
 			 xmin:state.xmin, $
 			 xmax:state.xmax, $
 			 zoomplot:state.zoomplot, $
 			 zoomrv:state.zoomrv}
 
 
 pstate = ptr_new(state, /no_copy, /allocate)
 
 print, 'THE FILE YOU RESTORED WAS: ', newname
 
 kfme_update_fields, pstate

  ;Save the pointer to the state structure:
  widget_control, event.top, set_uvalue=pstate
 endif;cancel not selected
 
end;kfme_restoreresid.pro

pro kfme_plotps, event

  ;This procedure will make a postscript plot of the next
  ;button pressed.
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).psplot = event.select
  
  ;print
  print, 'Postscript Plot Engaged?', (*pstate).psplot
  

end ;kfme_plotps.pro

pro kfme_sliderzoom, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).zoomplot = event.value

  ;Call the "scroll slider" routine to updat xmin and xmax:
  kfme_xminxmax, pstate

end ;kfme_sliderzoom.pro

pro kfme_sliderscroll, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  (*pstate).scroll = event.value/100.
  
  ;Call the "fit" routine:
  kfme_xminxmax, pstate
  
end ;kfme_sliderscroll.pro

pro kfme_xminxmax, pstate

  jd_rv = (*(*pstate).pcf).cf_rv.jd

  xrange =  max(jd_rv)
  
  
  xmid = max(jd_rv)/2. 
           

  ;Make sure there are still a few data points in the window:
  
  ;The difference in times between astrometric observations:
  numrv = n_elements(jd_rv)
  obdifrv = jd_rv[1:*] - jd_rv[0:numrv - 2]
  
  ;this factor makes sure that we don't zoom so far that
  ;there aren't any observations left to plot:
  mediandif = median(obdifrv)*3.d

  zoomfactor = 1000./(1001. - (*pstate).zoomplot) < $
  xrange/mediandif
  
  newrange = xrange/zoomfactor
  
  (*pstate).xmin = xmid + xrange*(1.d / zoomfactor) * $
  ( ( zoomfactor -1.d)/ 2.d) * (*pstate).scroll - newrange/2.

  (*pstate).xmax = xmid + xrange*(1.d / zoomfactor) * $
  ( ( zoomfactor -1.d)/ 2.d) * (*pstate).scroll + newrange/2.

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end ;kfme_xminmax.pro

pro kfme_zoomrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).zoomrv = event.value

  ;Call the "scroll slider" routine to updat xmin and xmax:
  kfme_dofit, pstate
end ;kfme_zoomrv.pro



pro kfme_multith, event
  ;This procedure will tie m & i for the case where the radial velocity
  ; data looks really good and you want to adjust the astrometry. 
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).multith = event.select
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_multith.pro

pro kfme_togerr, event
  ;This procedure will toggle the error bars on plots
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).togerr = event.select
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_togerr.pro

pro kfme_titleflag, event
  ;This procedure will toggle the plot titles
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).titleflag = event.select
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_titleflag.pro

pro kfme_connect, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  (*pstate).connect= -(*pstate).connect
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_connect.pro

pro kfme_printpars, event
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  ;(*pstate).printpars = event.select
  kfme_external_texpars, pstate
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_printpars.pro

pro kfme_printplpars, event
  ;This procedure will print just the planetary
  ;parameters in a format that can be thrown
  ;right into the paper
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the sign of the connect field:
  ;(*pstate).printpars = event.select
  kfme_external_texplpars, pstate
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_printplpars.pro

pro kfme_tfinebttn, event
  
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  ;Flip the value:
  if (*pstate).tfine then (*pstate).tfine=0 else (*pstate).tfine=1
  print, 'tfine toggled.'
  
  ;Call the "do" routine:
  kfme_dofit, pstate

end;kfme_tfinebttn.pro

pro kfme_sym, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
 ;Store the new linestyle in the state structure:
 (*pstate).psym=event.index
 
 ;Replot the data in the draw widget with the "do" routine:
 kfme_dofit, pstate
 
end;kfme_sym.pro

pro kfme_ls, event

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
 ;Store the new linestyle in the state structure:
 (*pstate).linestyle=event.index
 
 ;Replot the data in the draw widget with the "do" routine:
 kfme_dofit, pstate
 
end;kfme_ls.pro

pro kfme_plotorbit, event
 ;This procedure will create a plot of the orbit. 
 
 ;SET THIS KEYWORD IF YOU WANT TRANSIT STUFF INCLUDED!
 transitsyms = 0
 

 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
   	
 m2au = 1.49598d11 ; 1 AU in meters

 m_star = (*(*pstate).pfunctargs).m_star
 n_planets = (*(*pstate).pfunctargs).n_planets
 period = (*pstate).pars.par1[0].value
 a_pl=((period/365.2564d)^2*m_star)^(1./3.)*m2au
 m_pl_earth = (*pstate).pars.par1[1].value
 inc = 89.999d
 ecc = (*pstate).pars.par1[2].value
 om = (*pstate).pars.par1[3].value
 print, transpose((*pstate).pars.par1.parname)
 
 k = mpf_K(a_pl, m_pl_earth, period, m_star, inc, ecc)
 nels=1d3
 arel = a_pl
 
 
r_aparr = dblarr(n_planets)
a_plarr = dblarr(n_planets)
eccarr = dblarr(n_planets)
massarr = dblarr(n_planets)
perarr = dblarr(n_planets)
omarr = dblarr(n_planets)
tparr = dblarr(n_planets)

perarr[0] = (*pstate).pars.par1[0].value
massarr[0] = (*pstate).pars.par1[1].value
eccarr[0] = (*pstate).pars.par1[2].value
omarr[0] = (*pstate).pars.par1[3].value
tparr[0] = (*pstate).pars.par1[4].value

if n_planets gt 1 then begin
  perarr[1] = (*pstate).pars.par2[0].value
  massarr[1] = (*pstate).pars.par2[1].value
  eccarr[1] = (*pstate).pars.par2[2].value
  omarr[1] = (*pstate).pars.par2[3].value
  tparr[1] = (*pstate).pars.par2[4].value
endif

if n_planets gt 2 then begin
  perarr[2] = (*pstate).pars.par3[0].value
  massarr[2] = (*pstate).pars.par3[1].value
  eccarr[2] = (*pstate).pars.par3[2].value
  omarr[2] = (*pstate).pars.par3[3].value
  tparr[2] = (*pstate).pars.par3[4].value
endif

if n_planets gt 3 then begin
  perarr[3] = (*pstate).pars.par4[0].value
  massarr[3] = (*pstate).pars.par4[1].value
  eccarr[3] = (*pstate).pars.par4[2].value
  omarr[3] = (*pstate).pars.par4[3].value
  tparr[3] = (*pstate).pars.par4[4].value
endif

if n_planets gt 4 then begin
  perarr[4] = (*pstate).pars.par5[0].value
  massarr[4] = (*pstate).pars.par5[1].value
  eccarr[4] = (*pstate).pars.par5[2].value
  omarr[4] = (*pstate).pars.par5[3].value
  tparr[4] = (*pstate).pars.par5[4].value
endif

for i=0, n_planets-1 do begin
  a_plarr[i] = ((perarr[i]/365.25d)^2*m_star)^1./3d
  r_aparr[i] = (1d + eccarr[i])*a_plarr[i]
print, 'a_pl is: ', a_plarr[i]
print, 'eccarr is: ', eccarr[i]
print, 'r_ap is: ', r_aparr[i]
endfor
;stop
i1 = where( r_aparr eq max(r_aparr)) & i1 = double(i1[0])
print, 'the outer planet is planet ', strt(i1)
period = perarr[i1]
a_pl = a_plarr[i1] *m2au
ecc = eccarr[i1]
om = omarr[i1]
m_pl_earth = massarr[i1]



;psec: the period in days
 psec = period * 24.d * 3600.d

;a1sini: the upper boundary of the semimajor axis of the planet
a1sini = k*psec*sqrt(1.d - ecc^2.d)/(2.d * !pi)   ;in meters

;The longitude of the ascending node doesn't matter for radial
;	velocity or transit detection methods, only astrometric:
big_om = 0.d

;The angle traced out from periastron around orbit:
nu = 2.d*!pi*findgen(nels)/nels

;***********************************************************************
;      IN THE ORBITAL PLANE, THE PLANET IS GIVEN BY
;***********************************************************************
;The distance of the planet from the star in the orbital plane:
rarr = a_pl*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu))

;And in Cartesian Coordinates:
X = rarr*cos(nu)
Y = rarr*sin(nu)
om *= !pi/180.d
big_om *= !dpi / 180.d
inc *= !pi/180.d

print, 'omega is: ', strt(om), ' radians/ ',$
strt(om*!radeg), ' degrees.'
print, 'big_om is: ', strt(big_om), ' radians/ ',$
strt(big_om*!radeg), ' degrees.'
print, 'inc is: ', strt(inc), ' radians/ ',$
strt(inc*!radeg), ' degrees.'


;The Thiele-Innes Constants are:
ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(inc))
BTI =  (cos(om)*sin(big_om) + sin(om)*cos(big_om)*cos(inc))
CTI =   sin(om)*sin(inc)
FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(inc))
GTI = -(sin(om)*sin(big_om) - cos(om)*cos(big_om)*cos(inc))
HTI =   cos(om)*sin(inc)

;The Coordinates of the planet are now:
xnew = ATI*X + FTI*Y
ynew = BTI*X + GTI*Y
znew = CTI*X + HTI*Y

;Radius of the Sun:
rsun = 6.955d8		;in meters

StarRad = (*(*pstate).pfunctargs).rstar ;in Solar Radii
xstar = StarRad * rsun ;in meters

print, 'INGRESS, CENTER AND EGRESS OF TRANSITS: ', systime()
for i=1LL, n_elements(xnew)-1.d do begin
	if ( (xnew[i] le xstar) AND (xnew[i-1LL] gt xstar) ) then xing = i
	if ( (xnew[i] le -xstar) AND (xnew[i-1LL] gt -xstar) ) then xegr = i
	if ( (xnew[i] le 0) AND (xnew[i-1LL] gt 0) ) then xcen = i
	if ( (xnew[i-1LL] le xstar) AND (xnew[i] gt xstar) ) then sxegr = i
	if ( (xnew[i-1LL] lt -xstar) AND (xnew[i] ge -xstar) ) then sxing = i
	if ( (xnew[i] ge 0) AND (xnew[i-1LL] lt 0) ) then sxcen = i
endfor

if n_elements(xing) eq 0 then begin 
 if ( (xnew[0] le xstar) AND (xnew[n_elements(xnew)-1LL] gt xstar) ) then xing = 0
 print, 'there was no xing!'
 print, 'but #xing is now: ', n_elements(xing)
endif

;2009.02.18: added the following to take care of problems with 
; highly eccentric fits with short orbital period planets:
if n_elements(xing) le 0 then begin
  xing = LONG64(where(xnew eq max(xnew)))
  xing = xing[0]
endif
if n_elements(xegr) le 0 then begin
  xegr = LONG64(where(xnew eq min(xnew)))
  xegr = xegr[0]
endif

nnew = n_elements(xnew)
if n_elements(xcen) eq 0 then begin 
  if ( (xnew[0] le 0) AND (xnew[nnew-1LL] gt 0) ) then xcen = 0
  print, 'there was no xcen!'
  print, 'but #xcen is now: ', xcen
endif

if n_elements(xegr) eq 0 then begin 
  if (xnew[0] lt -xstar) AND (xnew[nnew-1LL] ge -xstar) then xegr = 0
  print, 'there was no xegr!'
  print, 'but #xegr is now: ', n_elements(xegr)
endif

if n_elements(sxcen) le 0 then begin
  if ( (xnew[0] ge 0) AND (xnew[nnew-1] lt 0) ) then sxcen = 0
endif;sxcen

if n_elements(sxing) le 0 then begin
  if ( (xnew[0] ge -xstar) AND (xnew[nnew-1] lt -xstar) ) then sxing =  0
endif;sxing

if n_elements(sxegr) le 0 then begin
  if ( (xnew[nnew-1] le xstar) AND (xnew[0] gt xstar) ) then sxegr=0
endif;sxegr

if n_elements(sxing) le 0 then begin
  usersymbol, 'star', sym_size=3
  plot, xnew, ynew
  oplot, [0,0], [0,0], ps=8
  sxing=where(xnew eq min(xnew))
endif;sxing

if n_elements(sxegr) le 0 then begin
  usersymbol, 'star', sym_size=3
  plot, xnew, ynew
  oplot, [0,0], [0,0], ps=8
  sxegr=where(xnew eq max(xnew))
endif;sxegr



if ~n_elements(xcen)*n_elements(xing)*n_elements(xegr) then stop
if ~n_elements(sxcen)*n_elements(sxing)*n_elements(sxegr) then stop

;For a planet with an ascending node on the RHS of the host star:

;The true anomaly of ingress:
nuing = nu[xing]

;The true anomaly of center:
nucen = nu[xcen]

;The true anomaly of egress:
nuegr = nu[xegr]

;The true anomaly of secondary transit ingress:
nusing = nu[sxing]

;The true anomaly of secondary transit center:
nuscen = nu[sxcen]

;The true anomaly of secondary transit egress:
nusegr = nu[sxegr]

;The planetary distance at ingress:
ring = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nuing) )

;The planetary distance at center:
rcen = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nucen) )

;The planetary distance at egress
regr = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nuegr) )

;The planetary distance at secondary transit ingress:
sring = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nusing) )

;The planetary distance at secondary transit center:
srcen = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nuscen) )

;The planetary distance at secondary transit egress
sregr = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nusegr) )

;The planetary distance throughout the orbit:
;rarr = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu) )

;determine if ing,cen or egr is the last element in the array 
;(happened with HIP85977):
if xing eq n_elements(nu)-1 then nuing2 = nu[0] else nuing2 = nu[xing+1]
if xcen eq n_elements(nu)-1 then nucen2 = nu[0] else nucen2 = nu[xcen+1]
if xegr eq n_elements(nu)-1 then nuegr2 = nu[0] else nuegr2 = nu[xegr+1]
if sxing eq n_elements(nu)-1 then nsing2=nu[0] else nsing2=nu[sxing+1]
if sxcen eq n_elements(nu)-1 then nscen2=nu[0] else nscen2=nu[sxcen+1]
if sxegr eq n_elements(nu)-1 then nsegr2=nu[0] else nsegr2=nu[sxegr+1]

;first we determine the element up from the ingress, center and egress:
ring2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nuing2) )
rcen2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nucen2) )
regr2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nuegr2) )
sring2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nsing2) )
srcen2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nscen2) )
sregr2 = arel*(1.d - ecc^(2.d))/(1.d + ecc*cos(nsegr2) )

;this flag will take care of the ambiguity in the mean anomaly later on:
if ring2 lt ring then ingflag = 1. else ingflag = 0.
if rcen2 lt rcen then cenflag = 1. else cenflag = 0.
if regr2 lt regr then egrflag = 1. else egrflag = 0.
if sring2 lt sring then singflag = 1. else singflag = 0.
if srcen2 lt srcen then scenflag = 1. else scenflag = 0.
if sregr2 lt sregr then segrflag = 1. else segrflag = 0.


if keyword_set(transit_plot) then plot, nu, rarr
;***********************************************************************
;		DETERMINE THE ECCENTRIC ANOMALY
;***********************************************************************
ecc = double(ecc)

;This IF compensates for circular orbits:
;For Ingress
numer = 1.d - abs(ring)/arel
if ( (numer ne 0.d) AND (ecc ne 0) ) then EAing =acos(numer/ecc) $
	else eaing = 0.d

;For Center:
numer = 1.d - abs(rcen)/arel
if ( (numer ne 0.d) and (ecc ne 0) ) then EAcen =acos(numer/ecc) $
	else eacen = 0.d

;For Egress:
numer = 1.d - abs(regr)/arel
if ( (numer ne 0.d) and (ecc ne 0) ) then EAegr =acos(numer/ecc) $
	else eaegr = 0.d

;For Secondary Transit Ingress
numer = 1.d - abs(sring)/arel
if ( (numer ne 0.d) AND (ecc ne 0) ) then sEAing =acos(numer/ecc) $
	else seaing = 0.d

;For Secondary Transit Center:
numer = 1.d - abs(srcen)/arel
if ( (numer ne 0.d) and (ecc ne 0) ) then sEAcen =acos(numer/ecc) $
	else seacen = 0.d

;For Secondary Transit Egress:
numer = 1.d - abs(sregr)/arel
if ( (numer ne 0.d) and (ecc ne 0) ) then sEAegr =acos(numer/ecc) $
	else seaegr = 0.d


;The Eccentric Anomaly throughout the orbit:
numer = 1.d - abs(rarr)/arel

;these next few lines take care of errors due to DOUBLEs not
;having high enough precision:
if max(numer) gt ecc then begin
print, 'max numer was: ', max(numer)
x = where(numer gt ecc)
numer[x] -= 1.1d-8
endif

if min(numer) lt -ecc then begin
print, 'min numer was: ', min(numer)
x = where(numer lt -ecc)
numer[x] = 1.1d-8
endif

if max(numer) gt ecc then begin
print, 'now you went too far the other way! ', max(numer)
x = where(numer gt ecc)
numer[x] = ecc
endif

if ((min(numer) lt 0) AND (min(numer) gt -1d-10)) then begin
endif



if (min(numer) lt -ecc) OR (max(numer) gt ecc) then begin
print, 'ecc is: ', ecc
print, 'minmax(numer): ', minmax(numer)
print, 'minmax(numer) cannot be lt/gt e, respectively.'
stop
endif

EAarr =acos(numer/ecc)


;***********************************************************************
;		DETERMINE THE MEAN ANOMALY
;***********************************************************************
;For Ingress:
if eaing ne 0.d then Ming = EAing - ecc*sin(EAing) else ming = nu[xing]

;For Center:
if eacen ne 0.d then Mcen = EAcen - ecc*sin(EAcen) else mcen = nu[xcen]

;For Egress:
if eaegr ne 0.d then Megr = EAegr - ecc*sin(EAegr) else megr = nu[xegr]

;For Ingress:
if seaing ne 0.d then sMing=sEAing - ecc*sin(sEAing) else sming=nu[sxing]

;For Center:
if seacen ne 0.d then sMcen=sEAcen - ecc*sin(sEAcen) else smcen=nu[sxcen]

;For Egress:
if seaegr ne 0.d then sMegr=sEAegr - ecc*sin(sEAegr) else smegr=nu[sxegr]

;this takes care of the pi ambiguity in the mean anomaly:
if ingflag then ming = (2*!pi-ming)
if cenflag then mcen = (2*!pi-mcen)
if egrflag then megr = (2*!pi-megr)
if singflag then sming = (2*!pi-sming)
if scenflag then smcen = (2*!pi-smcen)
if segrflag then smegr = (2*!pi-smegr)

;For the total orbit:
Marr = EAarr - ecc*sin(EAarr)

   lower = dblarr(2) - xstar
   upper = dblarr(2) + xstar
   yran = 1.25*[2*max(znew), min(znew)]/m2au
   xran = 1.25*minmax(xnew)/m2au

   rng= [yran[0] > xran[1], yran[1] < xran[0]]
   rng=[1.25*r_aparr[i1], -1.25*r_aparr[i1]]

   polyx = [-xstar, -xstar, xstar, xstar]/m2au
   polyy = [min(yran), max(yran), max(yran), min(yran)]
   polyy = 1.25*[-1d*r_aparr[i1], r_aparr[i1], $
                 r_aparr[i1], -1d*r_aparr[i1]]
   loadct, 39, /silent
   ;window, xsize=600, ysize=600
   starnm = (*(*pstate).pfunctargs).extitle
   plot, xnew/m2au, znew/m2au, title='Orbit of '+starnm+ $
   'b ', xtitle='Position(AU)', $
   ytitle='Position (AU)', $ 
	 yrange = rng, /ystyl, xrange=rng, $
	 /xsty, background=255, color=0, /isotropic
   loadct, 0, /silent
   oplot, lower, minmax(yran), color=225
   oplot, upper, minmax(yran), color=225
   polyfill, polyx, polyy, color=225
   loadct, 39, /silent

   ;overplot the star:
   usersymbol, 'CIRCLE', /fill
   oplot, [0, 0], [0, 0], ps=8, color=191, symsize = 5
   
   if transitsyms then begin
   PLANET_SIZE=1
   ;overplot primary ingress (aqua):
   color=63
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xing], xnew[xing]]/m2au, [znew[xing], znew[xing]]/m2au, $
   ps=8, symsize=planet_size, color=color
   legend, ['Primary Ingress   '], psym=8, pos=[.125,.88], /norm, box=0, $
   color=color
   
   ;overplot primary egress (blue):
   color=99
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xegr], xnew[xegr]]/m2au, [znew[xegr], znew[xegr]]/m2au, $
    ps=8, symsize=planet_size, color=color
   legend, ['Primary Egress    '], psym=8, pos=[.125,.83], /norm, box=0, $
   color=color
   
   ;overplot secondary ingress (green):
   color=151
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[sxing], xnew[sxing]]/m2au, [znew[sxing], znew[sxing]]/m2au,$
    ps=8, symsize=planet_size, color=color
   legend, ['Secondary Ingress'], psym=8, pos=[.125,.78], $
   /norm, box=0, color=color
   
   ;overplot secondary egress (orange):
   color=207
   usersymbol, 'CIRCLE',/fill
   oplot, [xnew[sxegr], xnew[sxegr]]/m2au, [znew[sxegr], znew[sxegr]]/m2au,$
   ps=8, symsize=planet_size, color=color
   legend, ['Secondary Egress '], psym=8, pos=[.125,.73], /norm, box=0, $
   color=color
   endif;transitsyms
   
   ;overplot the plane of the sky:
   oplot, rng, [0, 0], color=43, thick=2
   legend, 'plane of sky', linestyle=0, thick=2, color=45, $
   pos=[.16,.93], /norm, box=0, linelength=0.5
   
   color=140
   usersymbol, 'OBSERVER', orientation=15, size=3
   oplot, [0, 0], floor([max(rng),max(rng)]), ps=8, color=color, $
   symsize=2
   
   if transitsyms then begin
   legend, ' To Earth', psym=8, pos=[.125, .68], /norm, box=0, $
   color=color
   endif else begin
   legend, ' To Earth', psym=8, pos=[.125, .87], /norm, box=0, $
   color=color   
   endelse
   ;stop
   ;*******************************************************
   ;   OVERPLOT THE OTHER PLANETS
   ;*******************************************************
   if n_planets gt 1 then begin
     otrs = where( r_aparr ne max(r_aparr))
     for ii=0, n_elements(otrs)-1 do begin
        i1 = double(otrs[ii])
		  period = perarr[i1]
		  a_pl = a_plarr[i1]
		  ecc = eccarr[i1]
		  om = omarr[i1]
		  m_pl_earth = massarr[i1]
		  
		  psec = period * 24.d * 3600.d
		  nu = 2.d*!pi*findgen(nels)/nels
		  rarr = a_pl*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu))
		  X = rarr*cos(nu)
		  Y = rarr*sin(nu)
		  ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(inc))
		  CTI =   sin(om)*sin(inc)
		  FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(inc))
		  HTI =   cos(om)*sin(inc)
		  xnew = ATI*X + FTI*Y
		  znew = CTI*X + HTI*Y
		  
		  oplot, xnew, znew
     endfor
   
   endif;overplot the other planets section
   
   ;*******************************************************
   ;   OVERPLOT THE SOLAR SYSTEM PLANETS
   ;*******************************************************
     otrs = dindgen(6)
     solper = [87.97d, 224.7d, 365.25d, 686.97d, $
         4.33157d3, 1.0759d4]
     solapl = [0.387d, 0.723d, 1d, 1.524d, 5.204d, 9.582d]
     solecc = [0.21,6.8d-3, 1.7d-2, 9.3d-2, 4.9d-2, 5.5d-2]
     ;longitude of ascending node:
     solom = [48.3d, 76.67d, 348.74d, 49.56d, 100.5d, 113.6d]
     solma = [0.055d, 0.815d, 1d, 0.11d, 317.8d, 95d]
     solstr = ['M', 'V', 'E', 'M', 'J', 'S']

     loadct, 0, /silent
     for ii=0, n_elements(otrs)-1 do begin
		  period = solper[ii]
		  a_pl = solapl[ii]
		  ecc = solecc[ii]
		  om = solom[ii]
		  m_pl_earth = solma[ii]
		  
		  psec = period * 24.d * 3600.d
		  nu = 2.d*!pi*findgen(nels)/nels
		  rarr = a_pl*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu))
		  X = rarr*cos(nu)
		  Y = rarr*sin(nu)
		  ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(inc))
		  CTI =   sin(om)*sin(inc)
		  FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(inc))
		  HTI =   cos(om)*sin(inc)
		  xnew = ATI*X + FTI*Y
		  znew = CTI*X + HTI*Y
		  
		  oplot, xnew, znew, color=200
		  xyouts, xnew[0], znew[0], solstr[ii], color=200
     endfor
     loadct, 39, /silent
      
   
   
	pngdir = '/'
   time_stamp = jul2cal(systime(/julian), /dashn)
	  
   png_file = pngdir + starnm + '_orbit_' + time_stamp
   image=tvread(filename=png_file, /PNG, /nodialog)

   texdir='~/transit/PDFS/'+starnm+'/'
   if ~file_test(texdir) then spawn, 'mkdir '+texdir
if (*pstate).psplot then begin
	ps_open, texdir+starnm+'_pos'+time_stamp, /encaps, /color
	device, xsize=24, ysize=24
   plot, xnew/m2au, znew/m2au, title='Orbit of '+starnm+ $
   'b ', xtitle='Position(AU)', $
   ytitle='Position (AU)', $ 
	 yrange = rng, /ystyl, xrange=rng, $
	 /xsty, background=255, color=0, /isotropic
   loadct, 0, /silent
   oplot, lower, minmax(yran), color=225
   oplot, upper, minmax(yran), color=225
   polyfill, polyx, polyy, color=225
   loadct, 39, /silent

   ;overplot the star:
   usersymbol, 'CIRCLE', /fill
   oplot, [0, 0], [0, 0], ps=8, color=191, symsize = 5
   
   if transitsyms then begin
   PLANET_SIZE=1
   ;overplot primary ingress (aqua):
   color=63
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xing], xnew[xing]]/m2au, [znew[xing], znew[xing]]/m2au, $
   ps=8, symsize=planet_size, color=color
   legend, ['Primary Ingress   '], psym=8, pos=[.125,.88], /norm, box=0, $
   color=color
   
   ;overplot primary egress (blue):
   color=99
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xegr], xnew[xegr]]/m2au, [znew[xegr], znew[xegr]]/m2au, $
    ps=8, symsize=planet_size, color=color
   legend, ['Primary Egress    '], psym=8, pos=[.125,.83], /norm, box=0, $
   color=color
   
   ;overplot secondary ingress (green):
   color=151
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[sxing], xnew[sxing]]/m2au, [znew[sxing], znew[sxing]]/m2au,$
    ps=8, symsize=planet_size, color=color
   legend, ['Secondary Ingress'], psym=8, pos=[.125,.78], $
   /norm, box=0, color=color
   
   ;overplot secondary egress (orange):
   color=207
   usersymbol, 'CIRCLE',/fill
   oplot, [xnew[sxegr], xnew[sxegr]]/m2au, [znew[sxegr], znew[sxegr]]/m2au,$
   ps=8, symsize=planet_size, color=color
   legend, ['Secondary Egress '], psym=8, pos=[.125,.73], /norm, box=0, $
   color=color
   endif;transitsyms
   
   ;overplot the plane of the sky:
   oplot, rng, [0, 0], color=43, thick=2
   legend, 'plane of sky', linestyle=0, thick=2, color=45, $
   pos=[.16,.93], /norm, box=0, linelength=0.5
   
   color=140
   usersymbol, 'OBSERVER', orientation=15, size=3
   oplot, [0, 0], floor([max(rng),max(rng)]), ps=8, color=color, $
   symsize=2
   
   if transitsyms then begin
   legend, ' To Earth', psym=8, pos=[.125, .68], /norm, box=0, $
   color=color
   endif else begin
   legend, ' To Earth', psym=8, pos=[.125, .87], /norm, box=0, $
   color=color   
   endelse
   ;stop
   ;*******************************************************
   ;   OVERPLOT THE OTHER PLANETS
   ;*******************************************************
   if n_planets gt 1 then begin
     otrs = where( r_aparr ne max(r_aparr))
     for ii=0, n_elements(otrs)-1 do begin
        i1 = double(otrs[ii])
		  period = perarr[i1]
		  a_pl = a_plarr[i1]
		  ecc = eccarr[i1]
		  om = omarr[i1]
		  m_pl_earth = massarr[i1]
		  
		  psec = period * 24.d * 3600.d
		  nu = 2.d*!pi*findgen(nels)/nels
		  rarr = a_pl*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu))
		  X = rarr*cos(nu)
		  Y = rarr*sin(nu)
		  ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(inc))
		  CTI =   sin(om)*sin(inc)
		  FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(inc))
		  HTI =   cos(om)*sin(inc)
		  xnew = ATI*X + FTI*Y
		  znew = CTI*X + HTI*Y
		  
		  oplot, xnew, znew
     endfor
   
   endif;overplot the other planets section
   
   ;*******************************************************
   ;   OVERPLOT THE SOLAR SYSTEM PLANETS
   ;*******************************************************
     otrs = dindgen(6)
     solper = [87.97d, 224.7d, 365.25d, 686.97d, $
         4.33157d3, 1.0759d4]
     solapl = [0.387d, 0.723d, 1d, 1.524d, 5.204d, 9.582d]
     solecc = [0.21,6.8d-3, 1.7d-2, 9.3d-2, 4.9d-2, 5.5d-2]
     ;longitude of ascending node:
     solom = [48.3d, 76.67d, 348.74d, 49.56d, 100.5d, 113.6d]
     solma = [0.055d, 0.815d, 1d, 0.11d, 317.8d, 95d]
     solstr = ['M', 'V', 'E', 'M', 'J', 'S']

     loadct, 0, /silent
     for ii=0, n_elements(otrs)-1 do begin
		  period = solper[ii]
		  a_pl = solapl[ii]
		  ecc = solecc[ii]
		  om = solom[ii]
		  m_pl_earth = solma[ii]
		  
		  psec = period * 24.d * 3600.d
		  nu = 2.d*!pi*findgen(nels)/nels
		  rarr = a_pl*(1.d - ecc^(2.d))/(1.d + ecc*cos(nu))
		  X = rarr*cos(nu)
		  Y = rarr*sin(nu)
		  ATI =  (cos(om)*cos(big_om) - sin(om)*sin(big_om)*cos(inc))
		  CTI =   sin(om)*sin(inc)
		  FTI = -(sin(om)*cos(big_om) + cos(om)*sin(big_om)*cos(inc))
		  HTI =   cos(om)*sin(inc)
		  xnew = ATI*X + FTI*Y
		  znew = CTI*X + HTI*Y
		  
		  oplot, xnew, znew, color=200
		  xyouts, xnew[0], znew[0], solstr[ii], color=200
     endfor
     loadct, 39, /silent
      
   
	ps_close
endif;kw(pscolor)

if keyword_set(psbw) then begin
	ps_open, texdir+starnm+'_pos'+time_stamp+'bw', /encaps
	device, xsize=24, ysize=24

   plot, xnew/m2au, znew/m2au, title='Orbit of '+starnm+ $
   'b ', xtitle='Position(AU)', $
   ytitle='Position (AU)', $ 
	 yrange = rng, /ystyl, xrange=rng, /xsty, background=255, color=0
   loadct, 0, /silent
   oplot, lower, minmax(yran), color=225
   oplot, upper, minmax(yran), color=225
   polyfill, polyx, polyy, color=225
   loadct, 39, /silent

   ;overplot the star:
   usersymbol, 'CIRCLE', /fill
   oplot, [0, 0], [0, 0], ps=8, color=191, symsize = 5
   
   PLANET_SIZE=1
   ;overplot primary ingress (aqua):
   color=63
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xing], xnew[xing]]/m2au, [znew[xing], znew[xing]]/m2au, $
   ps=8, symsize=planet_size, color=color
   legend, ['Primary Ingress   '], psym=8, pos=[.125,.88], /norm, box=0, $
   color=color
   
   ;overplot primary egress (blue):
   color=99
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[xegr], xnew[xegr]]/m2au, [znew[xegr], znew[xegr]]/m2au, $
    ps=8, symsize=planet_size, color=color
   legend, ['Primary Egress    '], psym=8, pos=[.125,.83], /norm, box=0, $
   color=color
   
   ;overplot secondary ingress (green):
   color=151
   usersymbol, 'CIRCLE', /fill
   oplot, [xnew[sxing], xnew[sxing]]/m2au, [znew[sxing], znew[sxing]]/m2au,$
    ps=8, symsize=planet_size, color=color
   legend, ['Secondary Ingress'], psym=8, pos=[.125,.78], /norm, box=0, $
   color=color
   
   ;overplot secondary egress (orange):
   color=207
   usersymbol, 'CIRCLE',/fill
   oplot, [xnew[sxegr], xnew[sxegr]]/m2au, [znew[sxegr], znew[sxegr]]/m2au,$
   ps=8, symsize=planet_size, color=color
   legend, ['Secondary Egress '], psym=8, pos=[.125,.73], /norm, box=0, $
   color=color
   
   ;overplot the plane of the sky:
   oplot, rng, [0, 0], color=43, thick=2
   legend, 'plane of sky', linestyle=0, thick=2, color=45, $
   pos=[.125,.93], /norm, box=0
   
   color=140
   usersymbol, 'OBSERVER', orientation=15, size=1
   oplot, [0, 0], 2*[max(znew), max(znew)]/m2au, ps=8, color=color, $
   symsize=1
   legend, ' To Earth', psym=8, pos=[.125, .68], /norm, box=0, $
   color=color
   
	ps_close
endif;kw(psbw)

   loadct, 0, /silent

end;kfme_plotorbit.pro

pro kfme_addjitter, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;Ensure that the data are being plotted in the draw window:
  wset, (*pstate).win_id
  
  loadct, 39, /silent
  
  ;cf3 = (*(*pstate).pcf_resid).cf_rv
  print, 'the errvel before: ', avg((*(*pstate).pcf).cf_rv.errvel)
  ;stop
  (*(*pstate).pcf).cf_rv.errvel = $
  sqrt((*(*pstate).pcf).cf_rv.errvel^2 - $
  (*pstate).previousjitter^2 + $
  (*pstate).jitternum^2)

  print, 'the errvel after: ', avg((*(*pstate).pcf).cf_rv.errvel)
  print, 'the jitternum in pstate is: ', (*pstate).jitternum
  print, 'the stddev mnvel is: ', stddev((*(*pstate).pcf).cf_rv.mnvel)
  
 ;Replot the data in the draw widget with the "do" routine:
 kfme_dofit, pstate

end;kfme_addjitter.pro

pro kfme_jitternum, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;save the old jitter value:
  (*pstate).previousjitter = (*pstate).jitternum
  
  ;change the jitter value:
  (*pstate).jitternum = double(newpar)
  
  print, 'Jitter to add: ', newpar
  print, 'the current average err: ', avg((*(*pstate).pcf).cf_rv.errvel)
  ;stop
end;kfme_jitternum.pro


pro kfme_debug, event
 ; Get the pointer to the state structure from the user value
 ; of the top-level base.
 widget_control, event.top, get_uvalue=pstate
 
  stop
  
end;kfme_debug.pro


 ;**************************************************************
 ;**************************************************************
 ;           PLANET FIT CHECKBOXES PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_fitplnum, event
  ; Get the pointer to the state structure from the user value
  ; of the top-level base.
  widget_control, event.top, get_uvalue=pstate
  widget_control, event.id, get_value=val
  
  ;set the appropriate planet:
  case val of
	'FIT PLANET 1 ': (*pstate).fitplarr[0] = event.select
	'FIT PLANET 2 ': (*pstate).fitplarr[1] = event.select
	'FIT PLANET 3 ': (*pstate).fitplarr[2] = event.select
	'FIT PLANET 4 ': (*pstate).fitplarr[3] = event.select
	'FIT PLANET 5 ': (*pstate).fitplarr[4] = event.select
	'FIT PLANET 6 ': (*pstate).fitplarr[5] = event.select
	'FIT PLANET 7 ': (*pstate).fitplarr[6] = event.select
  endcase;val
  
  (*(*pstate).pfunctargs).n_planets = total((*pstate).fitplarr)
  ;print, (*pstate).fitplarr

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fit1.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX ALL PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fix1, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par1[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par1[0:18].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl1, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl1, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl1, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl1, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl1, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl1, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixaoxpl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixaoypl1, set_button = push
   widget_control, (*pstate).fixbttns.fixrvopl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixsrapl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixsdcpl1, set_button = push
   widget_control, (*pstate).fixbttns.fixsrvpl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixcrapl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixcdcpl1, set_button = push
   widget_control, (*pstate).fixbttns.fixcrvpl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixlaxpl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixprapl1, set_button = push
   ;widget_control, (*pstate).fixbttns.fixpdcpl1, set_button = push
 
   print, 'Planet 1 Fixed? ', push
 end ;kfme_fix1.pro

 pro kfme_fix2, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par2[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par2[0:18].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl2, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl2, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl2, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl2, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl2, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl2, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl2, set_button = push
 
   print, 'Planet 2 Fixed? ', push
 end ;kfme_fix2.pro

 pro kfme_fix3, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par3[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par3[0:6].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl3, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl3, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl3, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl3, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl3, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl3, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl3, set_button = push
 
   print, 'Planet 2 Fixed? ', push
 end ;kfme_fix3.pro

 pro kfme_fix4, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par4[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par4[0:6].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl4, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl4, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl4, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl4, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl4, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl4, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl4, set_button = push
 
   print, 'Planet 4 Fixed? ', push
 end ;kfme_fix4.pro

 pro kfme_fix5, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par5[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par5[0:6].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl5, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl5, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl5, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl5, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl5, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl5, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl5, set_button = push
 
   print, 'Planet 5 Fixed? ', push
 end ;kfme_fix5.pro

 pro kfme_fix6, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par6[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par6[0:6].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl6, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl6, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl6, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl6, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl6, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl6, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl6, set_button = push
 
   print, 'Planet 6 Fixed? ', push
 end ;kfme_fix6.pro

 pro kfme_fix7, event
   ;Retrieve the pointer to the state structure:
   widget_control, event.top, get_uvalue=pstate
  
   ;determine current state:
   push = ~((*pstate).pars.par7[0].fixed)
   
   ;change the period value:
   (*pstate).pars.par7[0:6].fixed = push
  
   widget_control, (*pstate).fixbttns.fixperpl7, set_button = push
   widget_control, (*pstate).fixbttns.fixmasspl7, set_button = push
   widget_control, (*pstate).fixbttns.fixeccpl7, set_button = push
   widget_control, (*pstate).fixbttns.fixincpl7, set_button = push
   widget_control, (*pstate).fixbttns.fixbompl7, set_button = push
   widget_control, (*pstate).fixbttns.fixlompl7, set_button = push
   widget_control, (*pstate).fixbttns.fixtoppl7, set_button = push
 
   print, 'Planet 7 Fixed? ', push
 end ;kfme_fix7.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 1 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************

 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par1[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl1, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl1, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par1[0].value
  
  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par1[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par1[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl1.pro

pro kfme_texperypl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par1[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl1, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl1, $
  set_value=strt(double(newper)*365.2564)
  
  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par1[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par1[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl1.pro

pro kfme_texmasspl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate

  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par1[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl1, $
  set_value=strt(newm)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl1.pro

pro kfme_texeccpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par1[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl1, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl1.pro

pro kfme_texincpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par1[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl1, $
  set_value=newi

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl1.pro

pro kfme_texbompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par1[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl1.pro

pro kfme_texlompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom value:
  (*pstate).pars.par1[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl1.pro

pro kfme_textoppl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the Tp value:
  (*pstate).pars.par1[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  if double(newpar) lt 20 then minpar = 0.01 else minpar = 0.1*$
  double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl1.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par1[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl1.pro
 
 pro kfme_fixmasspl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par1[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl1.pro
 
 pro kfme_fixeccpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par1[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl1.pro
 
 pro kfme_fixincpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par1[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl1.pro
 
 pro kfme_fixbompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par1[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl1.pro
 
 pro kfme_fixlompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par1[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl1.pro
 
 pro kfme_fixtoppl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par1[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl1.pro
 
 
 
 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par1[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl1, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl1, $
  set_value=strt(event.value/365.2564)

  widget_control, (*pstate).sliderflds.slidertoppl1, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par1[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par1[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl1.pro

pro kfme_slimasspl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par1[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl1, $
  set_value=strt((event.value/100.))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl1.pro

pro kfme_slieccpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the ecc value:
  (*pstate).pars.par1[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl1, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl1.pro

pro kfme_sliincpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par1[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl1, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl1.pro

pro kfme_slibompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par1[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl1, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl1.pro

pro kfme_slilompl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par1[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl1, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl1.pro

pro kfme_slitoppl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par1[4].value = top

  widget_control, (*pstate).txtflds.toptextpl1, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl1.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par1[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl1.pro

pro kfme_perhilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par1[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl1.pro

pro kfme_masslowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par1[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl1.pro

pro kfme_masshilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par1[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl1.pro

pro kfme_ecclowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par1[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl1.pro

pro kfme_ecchilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par1[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl1.pro

pro kfme_inclowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par1[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl1.pro

pro kfme_inchilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par1[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl1.pro

pro kfme_bomlowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par1[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl1.pro

pro kfme_bomhilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par1[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl1.pro

pro kfme_lomlowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par1[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl1.pro

pro kfme_lomhilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par1[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl1.pro

pro kfme_toplowlimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par1[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl1.pro

pro kfme_tophilimpl1, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par1[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl1.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 2 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par2[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl2, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl2, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par2[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par2[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl2.pro

pro kfme_texperypl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par2[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl2, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl2, $
  set_value=strt(double(newper)*365.2564)
  
  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par2[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par2[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl2.pro

pro kfme_texmasspl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate

  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par2[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl2, $
  set_value=strt(newm)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl2.pro


pro kfme_texeccpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par2[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl2, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl2.pro

pro kfme_texincpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)
  ;take into account Tie Mass + Inc
;  if (*pstate).tiemi then begin
;    (*pstate).pars.par2[1].value *= $
;    sin((*pstate).pars.par2[3].value*!dtor) / $
;    sin(newi*!dtor)
;
;	widget_control, (*pstate).txtflds.masstextpl2, $
;	 set_value=strt((*pstate).pars.par2[1].value)
; 
;	widget_control, (*pstate).sliderflds.slidermasspl2, $
;	 set_value=(*pstate).pars.par2[1].value*100d
;
;  endif;tiemi  

  ;change the inc value:
  (*pstate).pars.par2[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl2, $
  set_value=newi

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl2.pro

pro kfme_texbompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par2[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl2, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl2.pro

pro kfme_texlompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par2[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl2, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl2.pro

pro kfme_textoppl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par2[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl2.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par2[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl2.pro
 
 pro kfme_fixmasspl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par2[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl2.pro
 
 pro kfme_fixeccpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par2[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl2.pro
 
 pro kfme_fixincpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par2[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl2.pro
 
 pro kfme_fixbompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par2[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl2.pro
 
 pro kfme_fixlompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par2[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl2.pro
 
 pro kfme_fixtoppl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par2[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl2.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par2[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl2, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl2, $
  set_value=strt(event.value/365.2564)

  widget_control, (*pstate).sliderflds.slidertoppl2, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par2[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par2[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl2.pro

pro kfme_slimasspl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
;  ;take into account Tie Mass + Inc
;  if (*pstate).tiemi then begin
;	incold = (*pstate).pars.par2[3].value 
;	mold = (*pstate).pars.par2[1].value
;	msiniold = mold*sin(incold * !dtor)
;	if (newm gt msiniold) then begin
;	 (*pstate).pars.par2[3].value = $
;	 asin(mold * sin(incold*!dtor) /newm) * !radeg
;   endif else begin
;	 print, 'HALT! YOU CANNOT GO ANY FURTHER! ADJUST INC INSTEAD!'
;	 newm = (*pstate).pars.par2[1].value
;   endelse
;   
;	widget_control, (*pstate).txtflds.inctextpl2, $
;	 set_value=strt((*pstate).pars.par2[3].value)
; 
;	widget_control, (*pstate).sliderflds.sliderincpl2, $
;	 set_value=(*pstate).pars.par2[3].value
;  endif;tiemi
  
  ;change the mass value:
  (*pstate).pars.par2[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl2, $
  set_value=strt((event.value/100.))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl2.pro

pro kfme_slieccpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par2[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl2, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl2.pro

pro kfme_sliincpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

;  ;take into account Tie Mass + Inc
;  if (*pstate).tiemi then begin
;    (*pstate).pars.par2[1].value *= $
;    sin((*pstate).pars.par2[3].value*!dtor) / $
;    sin(event.value*!dtor)
;
;	widget_control, (*pstate).txtflds.masstextpl2, $
;	 set_value=strt((*pstate).pars.par2[1].value)
; 
;	widget_control, (*pstate).sliderflds.slidermasspl2, $
;	 set_value=(*pstate).pars.par2[1].value*100d
;
;  endif;tiemi  

  ;change the inc value:
  (*pstate).pars.par2[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl2, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl2.pro

pro kfme_slibompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par2[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl2, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl2.pro

pro kfme_slilompl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par2[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl2, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl2.pro

pro kfme_slitoppl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par2[4].value = top

  widget_control, (*pstate).txtflds.toptextpl2, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl2.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par2[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl2.pro

pro kfme_perhilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par2[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl2.pro

pro kfme_masslowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par2[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl2.pro

pro kfme_masshilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par2[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl2.pro

pro kfme_ecclowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par2[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl2.pro

pro kfme_ecchilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par2[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl2.pro

pro kfme_inclowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par2[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl2.pro

pro kfme_inchilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par2[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl2.pro

pro kfme_bomlowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par2[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl2.pro

pro kfme_bomhilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par2[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl2.pro

pro kfme_lomlowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par2[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl2.pro

pro kfme_lomhilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par2[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl2.pro

pro kfme_toplowlimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par2[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl2.pro

pro kfme_tophilimpl2, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par2[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl2.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 3 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par3[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl3, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl3, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl3, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par3[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par3[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl3.pro

pro kfme_texperypl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par3[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl3, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl3, $
  set_value=strt(double(newper)*365.2564)
  
  widget_control, (*pstate).sliderflds.slidertoppl3, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par3[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par3[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl3.pro

pro kfme_texmasspl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate

  newm = double(newpar)
  

  ;change the mass value:
  (*pstate).pars.par3[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl3, $
  set_value=strt(newm)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl3.pro

pro kfme_texeccpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par3[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl3, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl3.pro

pro kfme_texincpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par3[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl3, $
  set_value=newi

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl3.pro

pro kfme_texbompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par3[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl3, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl3.pro

pro kfme_texlompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par3[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl3, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl3.pro

pro kfme_textoppl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par3[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl3, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl3.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par3[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl3.pro
 
 pro kfme_fixmasspl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par3[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl3.pro
 
 pro kfme_fixeccpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par3[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl3.pro
 
 pro kfme_fixincpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par3[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl3.pro
 
 pro kfme_fixbompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par3[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl3.pro
 
 pro kfme_fixlompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par3[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl3.pro
 
 pro kfme_fixtoppl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par3[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl3.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par3[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl3, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl3, $
  set_value=strt(event.value/365.2564)

  widget_control, (*pstate).sliderflds.slidertoppl3, $
  set_slider_min=min((*(*pstate).pcf).cf_rv.jd) - $
  	(*pstate).pars.par3[0].value, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + $
  	(*pstate).pars.par3[0].value

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl3.pro

pro kfme_slimasspl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par3[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl3, $
  set_value=strt((event.value/100.))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl3.pro

pro kfme_slieccpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par3[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl3, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl3.pro

pro kfme_sliincpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par3[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl3, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl3.pro

pro kfme_slibompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par3[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl3, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl3.pro

pro kfme_slilompl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par3[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl3, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl3.pro

pro kfme_slitoppl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par3[4].value = top

  widget_control, (*pstate).txtflds.toptextpl3, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl3.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par3[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl3.pro

pro kfme_perhilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par3[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl3.pro

pro kfme_masslowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par3[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl3.pro

pro kfme_masshilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par3[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl3.pro

pro kfme_ecclowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par3[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl3.pro

pro kfme_ecchilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par3[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl3.pro

pro kfme_inclowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par3[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl3.pro

pro kfme_inchilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par3[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl3.pro

pro kfme_bomlowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par3[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl3.pro

pro kfme_bomhilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par3[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl3.pro

pro kfme_lomlowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par3[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl3.pro

pro kfme_lomhilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par3[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl3.pro

pro kfme_toplowlimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par3[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl3.pro

pro kfme_tophilimpl3, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par3[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl3.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 4 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par4[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl4, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl4, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl4, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl4.pro

pro kfme_texperypl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par4[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl4, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl4, $
  set_value=strt(double(newper)*365.2564)
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl4.pro

pro kfme_texmasspl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par4[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl4, $
  set_value=strt(newm)

  
  widget_control, (*pstate).sliderflds.slidermasspl4, $
  set_value=double(newm)*100.

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl4.pro

pro kfme_texeccpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par4[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl4, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl4.pro

pro kfme_texincpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par4[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl4, $
  set_value=newi

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl4.pro

pro kfme_texbompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par4[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl4, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl4.pro

pro kfme_texlompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par4[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl4, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl4.pro

pro kfme_textoppl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par4[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl4, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl4.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par4[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl4.pro
 
 pro kfme_fixmasspl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par4[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl4.pro
 
 pro kfme_fixeccpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par4[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl4.pro
 
 pro kfme_fixincpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par4[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl4.pro
 
 pro kfme_fixbompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par4[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl4.pro
 
 pro kfme_fixlompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par4[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl4.pro
 
 pro kfme_fixtoppl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par4[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl4.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par4[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl4, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl4, $
  set_value=strt(event.value/365.2564)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl4.pro

pro kfme_slimasspl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par4[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl4, $
  set_value=strt((event.value/100.))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl4.pro

pro kfme_slieccpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par4[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl4, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl4.pro

pro kfme_sliincpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par4[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl4, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl4.pro

pro kfme_slibompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par4[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl4, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl4.pro

pro kfme_slilompl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par4[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl4, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl4.pro

pro kfme_slitoppl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par4[4].value = top

  widget_control, (*pstate).txtflds.toptextpl4, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl4.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par4[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl4.pro

pro kfme_perhilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par4[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl4.pro

pro kfme_masslowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par4[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl4.pro

pro kfme_masshilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par4[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl4.pro

pro kfme_ecclowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par4[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl4.pro

pro kfme_ecchilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par4[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl4.pro

pro kfme_inclowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par4[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl4.pro

pro kfme_inchilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par4[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl4.pro

pro kfme_bomlowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par4[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl4.pro

pro kfme_bomhilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par4[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl4.pro

pro kfme_lomlowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par4[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl4.pro

pro kfme_lomhilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par4[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl4.pro

pro kfme_toplowlimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par4[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl4.pro

pro kfme_tophilimpl4, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par4[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl4.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 5 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par5[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl5, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl5, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl5, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl5.pro

pro kfme_texperypl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par5[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl5, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl5, $
  set_value=strt(double(newper)*365.2564)
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl5.pro

pro kfme_texmasspl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par5[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl5, $
  set_value=strt(newm)

  
  widget_control, (*pstate).sliderflds.slidermasspl5, $
  set_value=double(newm)*100.


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl5.pro

pro kfme_texeccpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par5[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl5, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl5.pro

pro kfme_texincpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par5[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl5, $
  set_value=newi


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl5.pro

pro kfme_texbompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par5[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl5, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl5.pro

pro kfme_texlompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par5[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl5, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl5.pro

pro kfme_textoppl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par5[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl5, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl5.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par5[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl5.pro
 
 pro kfme_fixmasspl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par5[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl5.pro
 
 pro kfme_fixeccpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par5[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl5.pro
 
 pro kfme_fixincpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par5[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl5.pro
 
 pro kfme_fixbompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par5[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl5.pro
 
 pro kfme_fixlompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par5[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl5.pro
 
 pro kfme_fixtoppl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par5[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl5.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par5[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl5, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl5, $
  set_value=strt(event.value/365.2564)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl5.pro

pro kfme_slimasspl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par5[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl5, $
  set_value=strt((event.value/100.))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl5.pro

pro kfme_slieccpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par5[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl5, $
  set_value=strt((event.value/1000.d))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl5.pro

pro kfme_sliincpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par5[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl5, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl5.pro

pro kfme_slibompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par5[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl5, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl5.pro

pro kfme_slilompl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par5[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl5, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl5.pro

pro kfme_slitoppl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par5[4].value = top

  widget_control, (*pstate).txtflds.toptextpl5, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl5.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par5[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl5.pro

pro kfme_perhilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par5[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl5.pro

pro kfme_masslowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par5[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl5.pro

pro kfme_masshilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par5[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl5.pro

pro kfme_ecclowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par5[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl5.pro

pro kfme_ecchilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par5[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl5.pro

pro kfme_inclowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par5[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl5.pro

pro kfme_inchilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par5[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl5.pro

pro kfme_bomlowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par5[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl5.pro

pro kfme_bomhilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par5[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl5.pro

pro kfme_lomlowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par5[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl5.pro

pro kfme_lomhilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par5[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl5.pro

pro kfme_toplowlimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par5[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl5.pro

pro kfme_tophilimpl5, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par5[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl5.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 6 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par6[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl6, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl6, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl6, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl6.pro

pro kfme_texperypl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par6[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl6, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl6, $
  set_value=strt(double(newper)*365.2564)
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl6.pro

pro kfme_texmasspl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par6[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl6, $
  set_value=strt(newm)

  
  widget_control, (*pstate).sliderflds.slidermasspl6, $
  set_value=double(newm)*100.


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl6.pro

pro kfme_texeccpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par6[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl6, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl6.pro

pro kfme_texincpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par6[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl6, $
  set_value=newi


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl6.pro

pro kfme_texbompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par6[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl6, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl6.pro

pro kfme_texlompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par6[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl6, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl6.pro

pro kfme_textoppl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par6[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl6, $
  set_value=double(newpar)
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl6.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par6[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl6.pro
 
 pro kfme_fixmasspl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par6[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl6.pro
 
 pro kfme_fixeccpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par6[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl6.pro
 
 pro kfme_fixincpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par6[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl6.pro
 
 pro kfme_fixbompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par6[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl6.pro
 
 pro kfme_fixlompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par6[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl6.pro
 
 pro kfme_fixtoppl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par6[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl6.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par6[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl6, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl6, $
  set_value=strt(event.value/365.2564)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl6.pro

pro kfme_slimasspl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par6[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl6, $
  set_value=strt((event.value/100.))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl6.pro

pro kfme_slieccpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par6[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl6, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl6.pro

pro kfme_sliincpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par6[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl6, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl6.pro

pro kfme_slibompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par6[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl6, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl6.pro

pro kfme_slilompl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par6[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl6, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl6.pro

pro kfme_slitoppl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par6[4].value = top

  widget_control, (*pstate).txtflds.toptextpl6, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl6.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par6[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl6.pro

pro kfme_perhilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par6[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl6.pro

pro kfme_masslowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par6[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl6.pro

pro kfme_masshilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par6[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl6.pro

pro kfme_ecclowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par6[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl6.pro

pro kfme_ecchilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par6[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl6.pro

pro kfme_inclowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par6[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl6.pro

pro kfme_inchilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par6[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl6.pro

pro kfme_bomlowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par6[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl6.pro

pro kfme_bomhilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par6[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl6.pro

pro kfme_lomlowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par6[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl6.pro

pro kfme_lomhilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par6[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl6.pro

pro kfme_toplowlimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par6[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl6.pro

pro kfme_tophilimpl6, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par6[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl6.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           PLANET 7 PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_texperpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par1[0].value = double(newper)
  
  widget_control, (*pstate).sliderflds.sliderperpl7, $
  set_value=double(newper)
  
  widget_control, (*pstate).txtflds.pertextypl7, $
  set_value=strt(double(newper)/365.2564d)
  
  widget_control, (*pstate).sliderflds.slidertoppl7, $
  set_slider_max=max((*(*pstate).pcf).cf_rv.jd) + newper
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperpl7.pro

pro kfme_texperypl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newper
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period value:
  (*pstate).pars.par1[0].value = double(newper)*365.2564
  
  widget_control, (*pstate).sliderflds.sliderperpl7, $
  set_value=double(newper)*365.2564

  widget_control, (*pstate).txtflds.pertextpl7, $
  set_value=strt(double(newper)*365.2564)
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texperypl7.pro

pro kfme_texmasspl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  newm = double(newpar)
  
  ;change the mass value:
  (*pstate).pars.par7[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl7, $
  set_value=strt(newm)

  
  widget_control, (*pstate).sliderflds.slidermasspl7, $
  set_value=double(newm)*100.


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texmasspl7.pro

pro kfme_texeccpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc value:
  (*pstate).pars.par7[2].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidereccpl7, $
  set_value=double(newpar*1000.d)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texeccpl7.pro

pro kfme_texincpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  
  newi = double(newpar)

  ;change the inc value:
  (*pstate).pars.par7[3].value = newi
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderincpl7, $
  set_value=newi


  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texincpl7.pro

pro kfme_texbompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom value:
  (*pstate).pars.par7[4].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderbompl7, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texbompl7.pro

pro kfme_texlompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par7[3].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderlompl7, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texlompl7.pro

pro kfme_textoppl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass value:
  (*pstate).pars.par7[4].value = double(newpar)
  
  widget_control, (*pstate).sliderflds.slidertoppl7, $
  set_value=double(newpar), $
  set_slider_min=minpar

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_textoppl7.pro



 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************
 pro kfme_fixperpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Period Fixed?', event.select

  ;change the period value:
  (*pstate).pars.par7[0].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixperpl7.pro
 
 pro kfme_fixmasspl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Mass Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par7[1].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixmasspl7.pro
 
 pro kfme_fixeccpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Eccentricity Fixed?', event.select

  ;change the ecc value:
  (*pstate).pars.par7[2].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixeccpl7.pro
 
 pro kfme_fixincpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Inclination Fixed?', event.select

  ;change the inc value:
  (*pstate).pars.par7[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixincpl7.pro
 
 pro kfme_fixbompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Big Omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par7[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixbompl7.pro
 
 pro kfme_fixlompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Lil omega Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par7[3].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixlompl7.pro
 
 pro kfme_fixtoppl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Time of Periastron Fixed?', event.select

  ;change the mass value:
  (*pstate).pars.par7[4].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixtoppl7.pro
 

 
 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_sliperpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the period value:
  (*pstate).pars.par7[0].value = event.value

  widget_control, (*pstate).txtflds.pertextpl7, $
  set_value=strt((event.value))

  widget_control, (*pstate).txtflds.pertextypl7, $
  set_value=strt(event.value/365.2564)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_sliperpl7.pro

pro kfme_slimasspl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  newm = double(event.value)/100.d
  
  ;change the mass value:
  (*pstate).pars.par7[1].value = newm
  
  widget_control, (*pstate).txtflds.masstextpl7, $
  set_value=strt((event.value/100.))


  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slimasspl7.pro

pro kfme_slieccpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the mass value:
  (*pstate).pars.par7[2].value = event.value/1000.d

  widget_control, (*pstate).txtflds.ecctextpl7, $
  set_value=strt((event.value/1000.d))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slieccpl7.pro

pro kfme_sliincpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the inc value:
  (*pstate).pars.par7[3].value = event.value

  widget_control, (*pstate).txtflds.inctextpl7, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_sliincpl7.pro

pro kfme_slibompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Big Omega value:
  (*pstate).pars.par7[4].value = event.value

  widget_control, (*pstate).txtflds.bomtextpl7, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slibompl7.pro

pro kfme_slilompl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the Little Omega value:
  (*pstate).pars.par7[3].value = event.value

  widget_control, (*pstate).txtflds.lomtextpl7, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slilompl7.pro

pro kfme_slitoppl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  top = event.value

  (*pstate).pars.par7[4].value = top

  widget_control, (*pstate).txtflds.toptextpl7, $
  set_value=strt(top)

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slitoppl7.pro




 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************
pro kfme_perlowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period min:
  (*pstate).pars.par7[0].limits[0] = double(newlim)
  print, 'New Lower Period Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perlowlimpl7.pro

pro kfme_perhilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the period max:
  (*pstate).pars.par7[0].limits[1] = double(newlim)
  print, 'New Upper Period Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_perhilimpl7.pro

pro kfme_masslowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass min:
  (*pstate).pars.par7[1].limits[0] = double(newlim)
  print, 'New Lower Mass Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masslowlimpl7.pro

pro kfme_masshilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the mass max:
  (*pstate).pars.par7[1].limits[1] = double(newlim)
  print, 'New Upper Mass Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_masshilimpl7.pro

pro kfme_ecclowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc min:
  (*pstate).pars.par7[2].limits[0] = double(newlim)
  print, 'New Lower Eccentricity Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecclowlimpl7.pro

pro kfme_ecchilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the ecc max:
  (*pstate).pars.par7[2].limits[1] = double(newlim)
  print, 'New Upper Eccentricity Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_ecchilimpl7.pro

pro kfme_inclowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc min:
  (*pstate).pars.par7[3].limits[0] = double(newlim)
  print, 'New Lower Inclination Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inclowlimpl7.pro

pro kfme_inchilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the inc max:
  (*pstate).pars.par7[3].limits[1] = double(newlim)
  print, 'New Upper Inclination Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_inchilimpl7.pro

pro kfme_bomlowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom min:
  (*pstate).pars.par7[4].limits[0] = double(newlim)
  print, 'New Lower Big Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomlowlimpl7.pro

pro kfme_bomhilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the bom max:
  (*pstate).pars.par7[4].limits[1] = double(newlim)
  print, 'New Upper Big Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_bomhilimpl7.pro

pro kfme_lomlowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom min:
  (*pstate).pars.par7[3].limits[0] = double(newlim)
  print, 'New Lower Little Omega Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomlowlimpl7.pro

pro kfme_lomhilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the lom max:
  (*pstate).pars.par7[3].limits[1] = double(newlim)
  print, 'New Upper Little Omega Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_lomhilimpl7.pro

pro kfme_toplowlimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top min:
  (*pstate).pars.par7[4].limits[0] = double(newlim)
  print, 'New Lower T_0 Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_toplowlimpl7.pro

pro kfme_tophilimpl7, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the top max:
  (*pstate).pars.par7[4].limits[1] = double(newlim)
  print, 'New Upper T_0 Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_tophilimpl7.pro




 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;           TRENDS PARAMETER BOXES
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************
 ;**************************************************************


 ;**************************************************************
 ;**************************************************************
 ;           TEXTBOX PROCEDURES
 ;**************************************************************
 ;**************************************************************


pro kfme_texrvo, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the rvo value:
  (*pstate).pars.par1[5].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderrvopl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texrvo.pro

pro kfme_texdew24, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the dewar value:
  (*pstate).dew24 = double(newpar)
  (*pstate).pars.par1[8].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderdew24, $
  set_value=double(newpar)

  ;restore the original data and add the dewar offset:
  restore, (*(*pstate).pcfname)

  x = where(cf3.errvel lt 1.25*median(cf3.errvel), errct)
  cf3 = cf3[x]
  
  newdew = where(cf3.dewar eq 24)
  
  cf3[newdew].mnvel = cf3[newdew].mnvel + (*pstate).dew24

  (*(*pstate).pcf).cf_rv= cf3
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texdew24.pro

pro kfme_texdew39, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the dewar value:
  (*pstate).dew39 = double(newpar)
  (*pstate).pars.par1[9].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderdew39, $
  set_value=double(newpar)

  ;restore the original data and add the dewar offset:
  restore, (*(*pstate).pcfname)

  x = where(cf3.errvel lt 1.25*median(cf3.errvel), errct)
  cf3 = cf3[x]
  
  newdew = where(cf3.dewar eq 39)
  
  cf3[newdew].mnvel = cf3[newdew].mnvel + (*pstate).dew39

  (*(*pstate).pcf).cf_rv= cf3
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texdew39.pro

pro kfme_texsrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the srv value:
  (*pstate).pars.par1[6].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidersrvpl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texsrv.pro

pro kfme_texcrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the crv value:
  (*pstate).pars.par1[7].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.slidercrvpl1, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texcrv.pro

pro kfme_texdew24, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the crv value:
  (*pstate).pars.par1[8].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderdew24, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texdew24.pro

pro kfme_texdew39, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newpar
  widget_control, event.top, get_uvalue=pstate
  
  ;change the crv value:
  (*pstate).pars.par1[8].value = double(newpar)
  
  ;Update the slider procedure to reflect this value:
  widget_control, (*pstate).sliderflds.sliderdew39, $
  set_value=double(newpar)

  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_texdew39.pro

 ;**************************************************************
 ;**************************************************************
 ;           FIX PROCEDURES
 ;**************************************************************
 ;**************************************************************

 pro kfme_fixrvo, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Radial Velocity Offset Fixed?', event.select

  ;change the rvo value:
  (*pstate).pars.par1[5].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixrvo.pro
 
pro kfme_fixsrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Slope RV Fixed?', event.select

  ;change the srv value:
  (*pstate).pars.par1[6].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixsrv.pro
 
 pro kfme_fixcrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Curve RV Fixed?', event.select

  ;change the crv value:
  (*pstate).pars.par1[7].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixcrv.pro
 
 pro kfme_fixdew24, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Dewar 24 Offset Fixed?', event.select

  ;change the crv value:
  (*pstate).pars.par1[8].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixdew24.pro
 
 pro kfme_fixdew39, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate
  
  print, 'Dewar 39 Offset Fixed?', event.select

  ;change the crv value:
  (*pstate).pars.par1[9].fixed = event.select
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_fixdew39.pro
 

 ;**************************************************************
 ;**************************************************************
 ;           SLIDER PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_slirvo, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the rvo value:
  (*pstate).pars.par1[5].value = event.value

  widget_control, (*pstate).txtflds.rvotextpl1, $
  set_value=strt((event.value))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slirvo.pro

pro kfme_slisrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the srv value:
  (*pstate).pars.par1[6].value = event.value/1d3

  widget_control, (*pstate).txtflds.srvtextpl1, $
  set_value=strt((event.value/1d3))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slisrv.pro

pro kfme_slicrv, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the crv value:
  (*pstate).pars.par1[7].value = event.value/1d6

  widget_control, (*pstate).txtflds.crvtextpl1, $
  set_value=strt((event.value/1d6))

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slicrv.pro

pro kfme_slidew24, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the crv value:
  (*pstate).dew24 = event.value/1d2

  widget_control, (*pstate).txtflds.dew24text, $
  set_value=strt((event.value/1d2))

  ;restore the original data and add the dewar offset:
  restore, (*(*pstate).pcfname)
  
  x = where(cf3.errvel lt 1.5*median(cf3.errvel), errct)
  cf3 = cf3[x]
  
  velplot, cf3, '', 4./24., dates, speed, errv, cai, nav, bincf, /noplot
  cf3=bincf
  loadct, 39, /silent
  !p = (*pstate).p_orig
  
  newdew = where(cf3.dewar eq 24, ndct)
  
  if ndct gt 0 then $
     cf3[newdew].mnvel = cf3[newdew].mnvel + (*pstate).dew24
  print, 'The dewar 24 offset being used is: ', (*pstate).dew24

  (*(*pstate).pcf).cf_rv= cf3

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slidew24.pro

pro kfme_slidew39, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.top, get_uvalue=pstate

  ;change the crv value:
  (*pstate).dew39 = event.value/1d2

  widget_control, (*pstate).txtflds.dew39text, $
  set_value=strt((event.value/1d2))

  ;restore the original data and add the dewar offset:
  restore, (*(*pstate).pcfname)

  x = where(cf3.errvel lt 1.5*median(cf3.errvel), errct)
  cf3 = cf3[x]
  
  velplot, cf3, '', 4./24., dates, speed, errv, cai, nav, bincf, /noplot
  cf3=bincf
  loadct, 39, /silent
  !p = (*pstate).p_orig
  
  
  newdew = where(cf3.dewar eq 39, ndct)
  
  if ndct gt 0 then $
     cf3[newdew].mnvel = cf3[newdew].mnvel + (*pstate).dew39

  print, 'The dewar 39 offset being used is: ', (*pstate).dew39

  (*(*pstate).pcf).cf_rv= cf3

  ;Call the "fit" routine:
  kfme_dofit, pstate

end;kfme_slidew39.pro


 ;**************************************************************
 ;**************************************************************
 ;           TEXT LIMIT PROCEDURES
 ;**************************************************************
 ;**************************************************************

pro kfme_rvolowlim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the rvo min:
  (*pstate).pars.par1[9].limits[0] = double(newlim)
  print, 'New Lower rvo Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_rvolowlim.pro

pro kfme_rvohilim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the rvo max:
  (*pstate).pars.par1[9].limits[1] = double(newlim)
  print, 'New Upper rvo Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_rvohilim.pro

pro kfme_srvlowlim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the srv min:
  (*pstate).pars.par1[12].limits[0] = double(newlim)
  print, 'New Lower srv Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_srvlowlim.pro

pro kfme_srvhilim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the srv max:
  (*pstate).pars.par1[12].limits[1] = double(newlim)
  print, 'New Upper srv Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_srvhilim.pro

pro kfme_crvlowlim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the crv min:
  (*pstate).pars.par1[15].limits[0] = double(newlim)
  print, 'New Lower crv Limit: ', newlim
  
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_crvlowlim.pro

pro kfme_crvhilim, event
  ;Retrieve the pointer to the state structure:
  widget_control, event.id, get_value=newlim
  widget_control, event.top, get_uvalue=pstate
  
  ;change the crv max:
  (*pstate).pars.par1[15].limits[1] = double(newlim)
  print, 'New Upper crv Limit: ', newlim
  ;Call the "fit" routine:
  kfme_dofit, pstate
  
end;kfme_crvhilim.pro


;**************************************************************
 ;**************************************************************
 ;                   THE BIG KAHUNA
 ;**************************************************************
 ;**************************************************************
pro kfme

 loadct, 39, /silent                
 !p.background = 255
 !p.color = 0
 usersymbol, 'CIRCLE', /fill
 p_orig = !p
 kfme_init, kfmedir = kfmedir, outputdir = outputdir, $
 datadir = datadir, starlist=starlist

 ;restore real data for the initial plot:
 cfname = 'cf1_001bl21'
 restore, kfmedir+'data/'+cfname+'.dat'
 
 cfname = kfmedir+'data/vst9826.dat'
 restore, cfname
 
 dew24 = where(cf3.dewar eq 24)
 pdew24 = ptr_new(dew24, /no_copy, /allocate)
 dew39 = where(cf3.dewar eq 39)
 pdew39 = ptr_new(dew39, /no_copy, /allocate)
 ast = 0
 rv = 1
 cf_resid = cf[0]
 chisq = 0d
 data = [cf.cf_ast.astx, cf.cf_ast.asty, cf.cf_rv.mnvel]
 combperg = [1, 1, 0]
 pcfname = ptr_new(cfname, /no_copy, /allocate)
 pdata = ptr_new(data, /no_copy, /allocate)
 pcf = ptr_new(cf, /no_copy, /allocate)
 max_time=max((*pcf).cf_ast.jd)
 n_obs=n_elements((*pcf).cf_ast.jd)
 time_study= fix(max_time * 1.1)
 pcf_resid = ptr_new(cf_resid, /no_copy, /allocate)
 transit = 0d
 ptransit = ptr_new(transit, /no_copy, /allocate)
 n_planets = 1
 rmsresid=0d
 zoomplot = 1.d
 scroll = 0.5d
 xmin = min((*pcf).cf_rv.jd)
 xmax = max((*pcf).cf_rv.jd)
 zoomrv = 1.

  cf = create_struct('cf_ast', (*pcf).cf_ast, $
                     'cf_rv', cf3, $
                     'm_star', 1d, $
                     'plx', (*pcf).plx, $
                     'prpr', (*pcf).prpr, $
                     'coords', (*pcf).coords, $
                     'midtime', 0d, $max(cf3.jd) - min(cf3.jd), $
                     'time_offset', 0d);(*(*pstate).pcf).time_offset)

  (*pcf) = cf
  (*pcf).cf_ast.jd *=0d
 xmin = min((*pcf).cf_rv.jd)
 xmax = max((*pcf).cf_rv.jd)

 ;Not Degrees of Freedom. This parameter is the number of
 ;data points - number of parameters:
 NDOF = 9.
 rstar=1d

 functargs={m_star:(*pcf).m_star, $
 parallax:(*pcf).plx, $
 rstar:rstar, $
 extitle:'HD9826', $
 time_study:time_study, $
 n_planets:n_planets, scaling:1, time_offset:(*pcf).time_offset, $
 dew24:pdew24, dew39:pdew39}
 
 pfunctargs = ptr_new(functargs, /allocate)
 
  BLIND21 = ['cf1_001bl21.dat', $
				 'cf1_020bl21.dat']

  pdatls = ptr_new(BLIND21, /no_copy, /allocate)
  
 ;The percentage of the screen you want the widget to take up:
 y_widget_size = 0.95
 x_widget_size = 0.95
 
   ; Get Monitor information
   oInfo = OBJ_NEW('IDLsysMonitorInfo')
   numMons = oinfo->GetNumberOfMonitors()
   names = oinfo->GetMonitorNames()
   rects = oInfo->GetRectangles()
   primaryIndex = oInfo->GetPrimaryMonitorIndex()

 ;Calculate size of widget:
 controlbase_xsize = 250
 botrow_ysize = 230
 mon_size = get_screen_size()
 y_offset = 0;(1. - y_widget_size)/2. * mon_size[1]
 
;if rects[2,0] gt rects[2,1] then midx=0 else midx=1
; mon_size = rects[2:3,midx]
 
 if !version.os eq 'darwin' then begin
 x_offset = 0;(1. - x_widget_size)/2. * mon_size[0]
 endif else x_offset = 0
 if mon_size[0] lt 1500 then x_offset = 0 

 draw_x_size = round(( mon_size[0] - controlbase_xsize - 25)* x_widget_size)
 
 if mon_size[1] ge 1200 then elgrande=1 else elgrande=0
 if ~elgrande then sm = 100 else sm = 90
 draw_y_size = round(( mon_size[1] - botrow_ysize - sm)* y_widget_size)

;if midx eq 1 then begin 
;x_offset += 1400
;endif
 
 ;make the top level base and add resize events:
 tlb = widget_base(title = 'Interactive KFME v. 2014/02/17 ', $
 /col, xoff = x_offset, yoff = y_offset, /tlb_size_events)
 
 ;Create the top row to house the plot & buttons:
 toprow = widget_base(tlb, title = 'Plot', /row, frame = 5)
 
 ;Create the bottom row to house the planetary parameters:
 botrow = widget_base(tlb, title= 'Planets', /col, $
 x_scroll_size = 800, scr_ysize=botrow_ysize+10)
 
 ;Make a base to hold the plot and the Fix Buttons:
 plotnfix = widget_base(toprow, /col)
 
 ;Make a draw widget:
 draw = widget_draw(plotnfix, xsize = draw_x_size, ysize=draw_y_size, $
 			frame = 2)
 
 ;Make a sizeable base to hold the controlbase:
 controlbasehm = widget_base(toprow, /col, $
 y_scroll_size = 200, scr_xsize = controlbase_xsize)
 

 ;Make a column base to hold a series of controls:
 controlbase = widget_base(controlbasehm, /col, $
 xsize=controlbase_xsize-20)
 ;controlbase = widget_base(toprow, /col)
 
 ;filetextbase = widget_base(controlbase, /col, frame = 1, scr_xsize=190,/ALIGN_CENTER)

 ;plottext = widget_label(filetextbase, value = 'FILE...', xsize = 100, /align_center)

 telrow = widget_base(controlbase, /row)

 fullcol=220
 halfcol=110
 thirdcol = 73
 fourthcol = 55
; openvst = widget_button(telrow, value = 'OPEN LICK', $
; event_pro = 'kfme_openlickvst',xsize=halfcol)
 
 openvst = widget_button(telrow, value = 'OPEN DATA', $
 event_pro = 'kfme_openkeckvst',xsize=halfcol)
 
; orbitrow = widget_base(controlbase, /row)
 savresid = widget_button(telrow, value = 'PLOT ORBIT', $
 event_pro = 'kfme_plotorbit', XSIZE=halfcol)
 
 importrow = widget_base(controlbase, /row)
 
 importtxt = widget_button(importrow, value = 'IMPORT TXT', $
 event_pro = 'kfme_importtxt',xsize=thirdcol)
 
 import_delimiter = ','
 importdelimbox = widget_text(importrow, value = import_delimiter, $
 	/editable, event_pro = 'kfme_importdelimiter', xsize = 8)

 import_skiplines = strt(2)
 importskipbox = widget_text(importrow, value = import_skiplines, $
 	/editable, event_pro = 'kfme_importskiplines', xsize = 8)

 importrow2 = widget_base(controlbase, /row)
 
 ;the offset for the observation times from BJD:
 import_jdoff = '2450000'
 importjdoffbox = widget_text(importrow2, value = import_jdoff, $
 	/editable, event_pro = 'kfme_importjdoff', xsize = 8)

 import_rvunit = 'kms'
 importrvunitbox = widget_text(importrow2, value = import_rvunit, $
 	/editable, event_pro = 'kfme_importrvunit', xsize = 8)

 import_errunit = 'kms'
 importerrunitbox = widget_text(importrow2, value = import_errunit, $
 	/editable, event_pro = 'kfme_importerrunit', xsize = 8)

 import_jdoff = '2450000'
 importdelimbox = widget_text(importrow2, value = import_jdoff, $
 	/editable, event_pro = 'kfme_importjdoff', xsize = 8)

 
 
 savrow = widget_base(controlbase, /row)

 savresid = widget_button(savrow, value = 'SAVE ALL', $
 event_pro = 'kfme_saveall', xsize=halfcol)
 
 residbuttn = widget_button(savrow, value = 'RESTORE ALL', $
 event_pro = 'kfme_restoreall', xsize=halfcol)
 
 datname=0
 pdatname = ptr_new(datname)
 
 
 datrow = widget_base(controlbase, /row)
 
 plotdatabuttn = widget_button(datrow, value = 'PLOT DATA', $
	 event_pro = 'kfme_plotdata', xsize=halfcol)
  
 plotresidbuttn = widget_button(datrow, value = 'PLOT RESIDUALS', $
 event_pro = 'kfme_plotresid', xsize=halfcol)

 ;Y-RANGE ROW:
 yranrow = widget_base(controlbase, /row)
 ;Add a button to toggle constraining y-range:
 yminmax = 0
 yranstbase = widget_base(yranrow, /nonexclusive)
 yranbutton = widget_button(yranstbase, $
   value = 'Y_MINMAX', $
   event_pro = 'kfme_set_y_range', XSIZE=thirdcol)
 
 yminval = widget_text(yranrow, value = strt(1.0), $
 	/editable, event_pro = 'kfme_ymin', xsize = 9)

 ymaxval = widget_text(yranrow, value = strt(1.0), $
 	/editable, event_pro = 'kfme_ymax', xsize = 9)

 ;STELLAR MASS ROW:
 stellarmrow = widget_base(controlbase, /row)
 
 smasstext = widget_text(stellarmrow, value = 'M_STAR:', xsize = 9)
 
 smassval = widget_text(stellarmrow, value = strt(1.0), $
 	/editable, event_pro = 'kfme_stellarmass', xsize = 9)

 smassuncval = widget_text(stellarmrow, value = strt(.01), $
 	/editable, event_pro = 'kfme_stellarmassunc', xsize = 9)

 stellarrrow = widget_base(controlbase, /row)
 
 sradiustext = widget_text(stellarrrow, value = 'R_STAR:', xsize = 9)
 
 sradiusval = widget_text(stellarrrow, value = strt(1.0), $
 	/editable, event_pro = 'kfme_stellarradius', xsize = 9)

 sradiusuncval = widget_text(stellarrrow, value = strt(.01), $
 	/editable, event_pro = 'kfme_stellarradiusunc', xsize = 9)

 alonerow = widget_base(controlbase, /row)
 
 aloneplanet = 2
 plalnumbox = widget_text(alonerow, value = strt(aloneplanet), $
 	/editable, event_pro = 'kfme_planetalone', xsize = '3')

 alonebuttn = widget_button(alonerow, value = 'REMOVE OTHERS', $
 event_pro = 'kfme_aloneplot', xsize=halfcol)
 
 phaserow = widget_base(controlbase, /row)

 phasebase = widget_base(alonerow, /nonexclusive)
 phasebool = 0
 phasebutton = widget_button(phasebase, $
   value = 'PHASE', $
   event_pro = 'kfme_orbitphase', xsize=halfcol)

 phasestart = 60d

 ;The orbital phase slider:
 phaseslide = widget_slider(controlbase, $
 title = 'Orbital Phase Start Point', $
 /suppress_val, $
    value = phasestart, minimum = -100., maximum = 100., $
    event_pro = 'kfme_sliderphase', /drag)
 
 pergrow = widget_base(controlbase, /row)

 pergrmbuttn = widget_button(pergrow, value = 'PERIODOGRAM', $
   event_pro = 'kfme_pergram', xsize=halfcol)
 

 pergrmbuttn = widget_button(pergrow, value = 'RESID PERG', $
   event_pro = 'kfme_residpergram', xsize=halfcol)
 
 lowperg = widget_base(controlbase, /row)

 lowpergtext = widget_text(lowperg, value = 'LOW_LIM:', xsize = 15)
 
 lowpergval = widget_text(lowperg, value = strt(0.75), $
 	/editable, event_pro = 'kfme_lowperg', xsize = '15')

 hiperg = widget_base(controlbase, /row)

 hipergtext = widget_text(hiperg, value = 'HI_LIM:', xsize = 15)
 
 hipergval = widget_text(hiperg, value = strt(10000), $
 	/editable, event_pro = 'kfme_hiperg', xsize = '15')

 ;;  (Horne and Baliunas, eq. 13)
 n0    = n_elements(cf3.jd)
 pergres = long(-6.362+1.193*n0+0.00098*n0^2.)
 pergres = 1d4

 pergresbox = widget_base(controlbase, /row)

 pergrestext = widget_text(pergresbox, value = '# FREQ:', xsize = 15)
 
 pergresval = widget_text(pergresbox, value = strt(pergres), $
 	/editable, event_pro = 'kfme_pergres', xsize = '15')

 pergfap = [1d-1, 1d-2]
 pergfapstr = '1d-1,1d-2'
 ppergfap = ptr_new(pergfap, /no_copy, /allocate)

 pergfapbox = widget_base(controlbase, /row)

 pergfaptext = widget_text(pergfapbox, value = 'FAP DESIRED:', xsize = 15)
 
 pergfapval = widget_text(pergfapbox, value = pergfapstr, $
 	/editable, event_pro = 'kfme_pergfap', xsize = '15')

 pergfapbuttnrow = widget_base(controlbase, /row)
 ;Add a button to toggle calculating the FAP in the periodogram:
 pergfapbool = 0
 pfapbase = widget_base(pergfapbuttnrow, /nonexclusive)
 psplotbutton = widget_button(pfapbase, $
   value = 'PERIODOGRAM FAP?', $
   event_pro = 'kfme_pergfapbool', XSIZE=fullcol)
 
 faprow = widget_base(controlbase, /row)

 fapbuttn = widget_button(faprow, value = 'KEP FAP', $
   event_pro = 'kfme_fap', xsize=halfcol)
   
 fapiter = 1000.
 fapval = widget_text(faprow, value = strt(fapiter), $
 	/editable, event_pro = 'kfme_fapiter', xsize = halfcol)

 
 runrow = widget_base(controlbase, /row)
 runbuttn = widget_button(runrow, value = 'RUN...', $
   event_pro = 'kfme_run', XSIZE=HALFCOL)
 
 runbuttn = widget_button(runrow, value = 'CYCLE RUN...', $
   event_pro = 'kfme_cyclerun', XSIZE=halfcol)
 
 run2row = widget_base(controlbase, /row)
 runbuttn = widget_button(run2row, value = 'RVLIN', $
   event_pro = 'kfme_rvlin', XSIZE=thirdcol)
; montebuttn = widget_button(run2row, value = 'MONTE', $
;   event_pro = 'kfme_monte', XSIZE=thirdcol)
 monte2buttn = widget_button(run2row, value = 'BOOTSTRAP MC', $
   event_pro = 'kfme_monte2', XSIZE=halfcol)
 
 residrow = widget_base(controlbase, /row)
 savresid = widget_button(residrow, value = 'SAVE RESID', $
 event_pro = 'kfme_saveresid', XSIZE=halfcol)
 
 residbuttn = widget_button(residrow, value = 'RESTORE RESID', $
 event_pro = 'kfme_restoreresid', XSIZE=halfcol)
 
 ;BUTTON ROW 1:
 
 buttnrow1 = widget_base(controlbase, /row)
 ;Add a button to toggle post script printing:
 psplot = 0
 psplotbase = widget_base(buttnrow1, /nonexclusive)
 psplotbutton = widget_button(psplotbase, $
   value = 'PS', $
   event_pro = 'kfme_plotps', XSIZE=thirdcol)
 
 ;Add a button to show the theoretical curve for each planet:
 multith = 0
 multithbase = widget_base(buttnrow1, /nonexclusive)
 multithbutton = widget_button(multithbase, $
   value = 'MULTI', $
   event_pro = 'kfme_multith', xsize=thirdcol)
   
 ;Add a button to toggle the error bars:
 togerr = 1
 togerrbase = widget_base(buttnrow1, /nonexclusive)
 togerrbutton = widget_button(togerrbase, $
   value = 'ERR', $
   event_pro = 'kfme_togerr', XSIZE=thirdcol)
 widget_control, togerrbutton, /set_button
   
;BUTTON ROW 2:
 buttnrow2 = widget_base(controlbase, /row)

 ;Add a button to toggle post script printing:
titleflag = 1
 tflagbase = widget_base(buttnrow2, /nonexclusive)
 tflagbutton = widget_button(tflagbase, $
   value = 'TITLE', $
   event_pro = 'kfme_titleflag', XSIZE=thirdcol)
 widget_control, tflagbutton, /set_button
 
tfinebase = widget_base(buttnrow2, /nonexclusive)
tfinebutton = widget_button(tfinebase, $
	value = 'TFINE', $
	event_pro = 'kfme_tfinebttn', XSIZE=fourthcol)
	tfine = 1d
 widget_control, tfinebutton, /set_button
   
;BUTTON ROW 3:   
 buttnrow3 = widget_base(controlbase, /row)
   
 ;Add a button to toggle the line connecting the symbols:
 connectbase = widget_base(buttnrow3, /nonexclusive)
 connectbutton = widget_button(connectbase, $
   value = 'CNCT SYM', $
   event_pro = 'kfme_connect', XSIZE=70)

 ;the default setting for the print pars button:
 printpars=0
 printparbase = widget_base(buttnrow3, /nonexclusive)
 printparbutton = widget_button(printparbase, $
   value = 'PARS', $
   event_pro = 'kfme_printpars', XSIZE=40)
 ;widget_control, printparbutton, /set_button

 printplpars=0
 printplparbase = widget_base(buttnrow3, /nonexclusive)
 printplparbutton = widget_button(printplparbase, $
   value = 'PL PARS', $
   event_pro = 'kfme_printplpars', XSIZE=fourthcol)

 ;The slider to Zoom:
 zoomslide = widget_slider(controlbase, title = 'Zoom', /suppress_val, $
    value = 1, minimum = 1., maximum = 1000., $
    event_pro = 'kfme_sliderzoom', /drag)
 
 ;The slider to Scroll:
 scrollslide = widget_slider(controlbase, title='Scroll', $
 	/suppress_val, $
    value = 0, minimum = -100., maximum = 100., $
    event_pro = 'kfme_sliderscroll', /drag)
 
 jitterrow = widget_base(controlbase, /row)

 jitterbuttn = widget_button(jitterrow, value = 'jitter', $
   event_pro = 'kfme_addjitter', xsize=halfcol)
   
 jitternum = 0d
 previousjitter = 0d
 jitterbox = widget_text(jitterrow, value = strt(jitternum), $
 	/editable, event_pro = 'kfme_jitternum', xsize = halfcol)

 ;Make a droplist to hold line style choices:
 symbols = ['SYMBOL', 'Plus', 'Asterisk', 'Period', 'Diamond', $
 'Triangle', 'Square', 'X', 'Solid Dot']
 
 linsymrow = widget_base(controlbase, /row)
 symboldrop = widget_droplist(linsymrow, value=symbols, $
 title = '', event_pro = 'kfme_sym', xsize=halfcol)
 
 ;Make a droplist to hold line style choices:
 linestyles= ['LINE', 'Dotted', 'Dashed', 'Dash Dot', 'Dash Dot Dot', $
 'Long Dash']
 
 linestyledrop = widget_droplist(linsymrow, value = linestyles, $
 title = '', event_pro = 'kfme_ls', xsize=halfcol)
 
;PLOTORBIT GOES HERE


 plotresidbuttn = widget_button(controlbase, value = 'DEBUG', $
 event_pro = 'kfme_debug')
 
 

 ;**************************************************************
 ;           CREATE PLANET FIT CHECKBOXES:
 ;**************************************************************
 ;The array that will be passed around with the checkbox values:
 fitplarr = [1, 0, 0, 0, 0, 0, 0]
 
 
 ;**************************************************************
 ;         CREATE LIMITS FOR EACH PLANET  
 ;**************************************************************
 ;Set the limits for the time of periastron passage to be the 
 ;same for all of the planets:
 ToPLimits=[1d-3,2.5d4]
 ;**************************************************************
 ;           CREATE TABS:
 ;**************************************************************
 planettab = widget_tab(botrow, location=0)
 titleFont = 'Times New Roman*bold'


 ;**************************************************************
 ;           CREATE FIRST PLANET:
 ;**************************************************************
 par1=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},12)

 par1[0].parname='Period [d]' 
 par1[1].parname='Mplanet [M_E]' 
 par1[2].parname='Ecc'  
 par1[3].parname='Lil omega'   
 par1[4].parname='2440000-Tp ' 
 par1[5].parname='Offset RV (m/s)'
 par1[6].parname='Slope RV'
 par1[7].parname='RV quad (m/s /year^2)'
 par1[8].parname='Dewar 24 Offset (m/s)'
 par1[9].parname='Dewar 39 Offset (m/s)'
 par1[10].parname='K (m/s)'
 par1[11].parname='a_pl (AU)'


 ;***************TABSTUFF************************************
 planetbase1 = widget_base(planettab,/column,title=' '+ string(127B)+ $
 ' Planet 1  ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase1, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar1bttn = widget_button(fitplanetbase, value = 'FIT PLANET 1 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar1bttn, /set_button

 resetbtn1 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************



 ;This button just shows which planet it is:
 red_image = BYTARR(6, 170, 3)
 red_image[*, *, 0] = 255
 plnt1clrbttn = widget_button(planet, value = red_image)
 plnt1bttnbgn = widget_button(planet, value = '1', $
 event_pro = 'kfme_fix1')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 270d
 par1[0].value = initper
 par1[0].limited=[1,1]
 par1[0].limits=[0.75,30000.]
 par1[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar1 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar1, /row)
 
 textparperpl1 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl1')
 
 textparperypl1 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar1, /row)
 
 pertextpl1 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl1', xsize = 10)

 pertextypl1 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl1', xsize = 10)

 sliderperpl1 = widget_slider(orbpar1, event_pro = $
 	'kfme_sliperpl1', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar1, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl1 = widget_text(lowlim, value = $
 	strt(par1[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl1', xsize = '12')
 
 uplim = widget_base(orbpar1, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl1 = widget_text(uplim, value = strt(par1[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl1', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = 50.0d 
 if elgrande then initmass = 50.0d 
 par1[1].value = initmass
 par1[1].limited=[1,1]
 par1[1].limits=[0.01,5000.]
 par1[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
amptitle = widget_base(orbpar2, /row)

 textparmspl1 = widget_text(amptitle, $
 value = 'Mpl[M_E]', $
 event_pro = 'kfme_par', xsize = 8)
 
 fixpar = widget_base(amptitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl1 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl1')
  
 textparkspl1 = widget_text(amptitle, $
 value = 'K[m/s]', $
 event_pro = 'kfme_par', xsize = 6)
 
  ampnums = widget_base(orbpar2, /row)
 masstextpl1 = widget_text(ampnums, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl1', xsize = 10) ;, /all_events

  period=initper
  mstar = functargs.m_star
  a_pl=((period/365.2564d)^2*mstar)^(1./3.)
  m_pl_earth = initmass
  inc = 89.9d
  initecc = 0.05
  ecc = initecc
  
  semiamp = mpf_K(a_pl, m_pl_earth, period, mstar, inc, ecc)

 amptextpl1 = widget_text(ampnums, value = strt(semiamp), /editable, $
 event_pro = 'kfme_texamppl1', xsize = 10) ;, /all_events

 slidermasspl1 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl1', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl1 = widget_text(lowlim, value = strt(par1[1].limits[0]), $
   /editable, event_pro = 'kfme_masslowlimpl1')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl1 = widget_text(uplim, value = strt(par1[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl1')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = initecc & par1[2].value = initecc
 par1[2].limited=[1,1]
 par1[2].limits=[1d-9 ,0.99d]
 par1[2].step=0.01

 
 ;Create a base to hold each orbital parameter:
 orbpar3 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar3, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar3, /row)
 
 ecctextpl1 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl1', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl1 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl1')
 
 slidereccpl1 = widget_slider(orbpar3, event_pro = $
 	'kfme_slieccpl1', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar3, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl1 = widget_text(lowlim, value = strt(par1[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl1', xsize = '12')
 
 uplim = widget_base(orbpar3, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl1 = widget_text(uplim, value = strt(par1[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl1', xsize = '12')
 

 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par1[3].value = initlom
 par1[3].limited=[1,1]
 par1[3].limits=[0.0d,360.]
 par1[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar4 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar4, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar4, /row)
 
 lomtextpl1 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl1', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl1')
 
 sliderlompl1 = widget_slider(orbpar4, event_pro = $
 	'kfme_slilompl1', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar4, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl1 = widget_text(lowlim, value = strt(par1[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl1', xsize = '12')
 
 uplim = widget_base(orbpar4, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl1 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl1', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = 12345d & par1[4].value = inittop
 par1[4].limited=[1,1]
 par1[4].limits=ToPLimits
 par1[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar5 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar5, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl1', xsize = 10)
 
 fixpar = widget_base(orbpar5, /row)
 
 toptextpl1 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl1', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl1')
 
 slidertoppl1 = widget_slider(orbpar5, $
 	event_pro = 'kfme_slitoppl1', /suppress_value, $
 	/drag, value = 1d * min(cf3.jd), minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar5, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl1 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl1', xsize = '12')
 
 uplim = widget_base(orbpar5, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl1 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl1', xsize = '12')
 

 
 ;This button just shows which planet it is:
 plnt1bttnend = widget_button(planet, value = '1', $
   event_pro = 'kfme_fix1')
 


 ;**************************************************************
 ;           CREATE SECOND PLANET:
 ;**************************************************************
 par2=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par2[0].parname='Period [d]' 
 par2[1].parname='Mplanet [M_E]' 
 par2[2].parname='Ecc'  
 par2[3].parname='Lil omega'   
 par2[4].parname='2440000-Tp ' 
 par2[5].parname='K (m/s)'
 par2[6].parname='a_pl (AU)'


;***************TABSTUFF************************************
 planetbase2 = widget_base(planettab,/column,title='   Planet 2   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase2, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar2bttn = widget_button(fitplanetbase, value = 'FIT PLANET 2 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar2bttn, /set_button

 resetbtn2 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 ;This button just shows which planet it is:
 orange_image = BYTARR(6, 170, 3)
 orange_image[*, *, 0] = 250
 orange_image[*, *, 1] = 166
 orange_image[*, *, 2] = 4
 plnt2clrbttn = widget_button(planet, value = orange_image)
 plnt2bttnbgn = widget_button(planet, value = '2', $
   event_pro = 'kfme_fix2')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par2[0].value = initper
 par2[0].limited=[1,1]
 par2[0].limits=[0.75,30000.]
 par2[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar2 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar2, /row)
 
 textparperpl2 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl2 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl2')
 
 textparperypl2 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar2, /row)
 
 pertextpl2 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl2', xsize = 10)

 pertextypl2 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl2', xsize = 10)

 sliderperpl2 = widget_slider(orbpar2, event_pro = $
 	'kfme_sliperpl2', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl2 = widget_text(lowlim, value = $
 	strt(par2[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl2', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl2 = widget_text(uplim, value = strt(par2[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl2', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = 0.1d & par2[1].value = initmass
 par2[1].limited=[1,1]
 par2[1].limits=[0.10,5000.]
 par2[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl2 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl2 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl2', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl2 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl2')
 
 slidermasspl2 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl2', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl2 = widget_text(lowlim, value = strt(par2[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl2', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl2 = widget_text(uplim, value = strt(par2[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl2', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par2[2].value = initecc
 par2[2].limited=[1,1]
 par2[2].limits=[1d-9 ,0.99d]
 par2[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar3 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar3, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar3, /row)
 
 ecctextpl2 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl2', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl2 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl2')
 
 slidereccpl2 = widget_slider(orbpar3, event_pro = $
 	'kfme_slieccpl2', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar3, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl2 = widget_text(lowlim, value = strt(par2[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl2', xsize = '12')
 
 uplim = widget_base(orbpar3, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl2 = widget_text(uplim, value = strt(par2[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl2', xsize = '12')
 



 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par2[3].value = initlom
 par2[3].limited=[1,1]
 par2[3].limits=[0.0d,360.]
 par2[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl2 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl2', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl2 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl2')
 
 sliderlompl2 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl2', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl2 = widget_text(lowlim, value = strt(par2[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl2', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl2 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl2', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = 14045d & par2[4].value = inittop
 par2[4].limited=[1,1]
 par2[4].limits=ToPLimits
 par2[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl2', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 toptextpl2 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl2', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl2 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl2')
 
 slidertoppl2 = widget_slider(orbpar6, $
 	event_pro = 'kfme_slitoppl2', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl2 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl2', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl2 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl2', xsize = '12')
 
 ;This button just shows which planet it is:
 plnt2bttnend = widget_button(planet, value = '2', $
   event_pro = 'kfme_fix2')
 



 ;**************************************************************
 ;           CREATE THIRD PLANET:
 ;**************************************************************
 par3=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par3[0].parname='Period [d]' 
 par3[1].parname='Mplanet [M_E]' 
 par3[2].parname='Ecc'  
 par3[3].parname='Lil omega'   
 par3[4].parname='2440000-Tp ' 
 par3[5].parname='K (m/s)'
 par3[6].parname='a_pl (AU)'


;***************TABSTUFF************************************
 planetbase3 = widget_base(planettab,/column,title='   Planet 3   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase3, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar3bttn = widget_button(fitplanetbase, value = 'FIT PLANET 3 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar3bttn, /set_button

 resetbtn3 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 ;This button just shows which planet it is:
 yellow_image = BYTARR(6, 170, 3)
 yellow_image[*, *, 0] = 250
 yellow_image[*, *, 1] = 236
 yellow_image[*, *, 2] = 4
 plnt3clrbttn = widget_button(planet, value = yellow_image)
 plnt3bttnbgn = widget_button(planet, value = '3', $
   event_pro = 'kfme_fix3')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par3[0].value = initper
 par3[0].limited=[1,1]
 par3[0].limits=[0.75,30000.]
 par3[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar3 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar3, /row)
 
 textparperpl3 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl3 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl3')
 
 textparperypl3 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar3, /row)
 
 pertextpl3 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl3', xsize = 10)

 pertextypl3 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl3', xsize = 10)

 sliderperpl3 = widget_slider(orbpar3, event_pro = $
 	'kfme_sliperpl3', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar3, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl3 = widget_text(lowlim, value = $
 	strt(par3[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl3', xsize = '12')
 
 uplim = widget_base(orbpar3, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl3 = widget_text(uplim, value = strt(par3[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl3', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = .1d & par3[1].value = initmass
 par3[1].limited=[1,1]
 par3[1].limits=[0.01,5000.]
 par3[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl3 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl3 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl3', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl3 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl3')
 
 slidermasspl3 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl3', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl3 = widget_text(lowlim, value = strt(par3[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl3', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl3 = widget_text(uplim, value = strt(par3[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl3', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par3[2].value = initecc
 par3[2].limited=[1,1]
 par3[2].limits=[1d-9 ,0.99d]
 par3[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar3 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar3, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar3, /row)
 
 ecctextpl3 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl3', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl3 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl3')
 
 slidereccpl3 = widget_slider(orbpar3, event_pro = $
 	'kfme_slieccpl3', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar3, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl3 = widget_text(lowlim, value = strt(par3[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl3', xsize = '12')
 
 uplim = widget_base(orbpar3, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl3 = widget_text(uplim, value = strt(par3[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl3', xsize = '12')
 

 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par3[3].value = initlom
 par3[3].limited=[1,1]
 par3[3].limits=[0.0d,360.]
 par3[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl3 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl3', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl3 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl3')
 
 sliderlompl3 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl3', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl3 = widget_text(lowlim, value = strt(par3[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl3', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl3 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl3', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = .5*initper & par3[4].value = inittop
 par3[4].limited=[1,1]
 par3[4].limits=ToPLimits
 par3[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl3', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 toptextpl3 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl3', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl3 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl3')
 
 slidertoppl3 = widget_slider(orbpar6, $
 	event_pro = 'kfme_slitoppl3', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl3 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl3', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl3 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl3', xsize = '12')
 

 ;This button just shows which planet it is:
 plnt3bttnend = widget_button(planet, value = '3', $
   event_pro = 'kfme_fix3')
 




 ;**************************************************************
 ;           CREATE FOURTH PLANET:
 ;**************************************************************
 par4=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par4[0].parname='Period [d]' 
 par4[1].parname='Mplanet [M_E]' 
 par4[2].parname='Ecc'  
 par4[3].parname='Lil omega'   
 par4[4].parname='2440000-Tp ' 
 par4[5].parname='K (m/s)'
 par4[6].parname='a_pl (AU)'


;***************TABSTUFF************************************
 planetbase4 = widget_base(planettab,/column,title='   Planet 4   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase4, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar4bttn = widget_button(fitplanetbase, value = 'FIT PLANET 4 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar4bttn, /set_button

 resetbtn4 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 
 ;This button just shows which planet it is:
 olive_image = BYTARR(6, 170, 3)
 olive_image[*, *, 0] = 179
 olive_image[*, *, 1] = 217
 olive_image[*, *, 2] = 4
 plnt4clrbttn = widget_button(planet, value = olive_image)
 plnt4bttnbgn = widget_button(planet, value = '4', $
   event_pro = 'kfme_fix4')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par4[0].value = initper
 par4[0].limited=[1,1]
 par4[0].limits=[0.75,30000.]
 par4[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar4 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar4, /row)
 
 textparperpl4 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl4 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl4')
 
 textparperypl4 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 pernums = widget_base(orbpar4, /row)
 
 pertextpl4 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl4', xsize = 10)

 pertextypl4 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl4', xsize = 10)

 sliderperpl4 = widget_slider(orbpar4, event_pro = $
 	'kfme_sliperpl4', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar4, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl4 = widget_text(lowlim, value = $
 	strt(par4[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl4', xsize = '12')
 
 uplim = widget_base(orbpar4, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl4 = widget_text(uplim, value = strt(par4[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl4', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = .1d & par4[1].value = initmass
 par4[1].limited=[1,1]
 par4[1].limits=[0.01,5000.]
 par4[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl4 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl4 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl4', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl4 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl4')
 
 slidermasspl4 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl4', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl4 = widget_text(lowlim, value = strt(par4[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl4', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl4 = widget_text(uplim, value = strt(par4[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl4', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par4[2].value = initecc
 par4[2].limited=[1,1]
 par4[2].limits=[1d-9 ,0.99d]
 par4[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar4 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar4, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar4, /row)
 
 ecctextpl4 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl4', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl4 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl4')
 
 slidereccpl4 = widget_slider(orbpar4, event_pro = $
 	'kfme_slieccpl4', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar4, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl4 = widget_text(lowlim, value = strt(par4[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl4', xsize = '12')
 
 uplim = widget_base(orbpar4, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl4 = widget_text(uplim, value = strt(par4[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl4', xsize = '12')
 



 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par4[3].value = initlom
 par4[3].limited=[1,1]
 par4[3].limits=[0.0d,360.]
 par4[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl4 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl4', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl4 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl4')
 
 sliderlompl4 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl4', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl4 = widget_text(lowlim, value = strt(par4[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl4', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl4 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl4', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = min(cf3.jd) & par4[4].value = inittop
 par4[4].limited=[1,1]
 par4[4].limits=ToPLimits
 par4[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl4', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 toptextpl4 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl4', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl4 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl4')
 
 slidertoppl4 = widget_slider(orbpar6, $
 	event_pro = 'kfme_slitoppl4', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl4 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl4', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl4 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl4', xsize = '12')
 
 ;This button just shows which planet it is:
 plnt1bttn = widget_button(planet, value = '4', $
   event_pro = 'kfme_fix4')
 



 ;**************************************************************
 ;           CREATE FIFTH PLANET:
 ;**************************************************************
 par5=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par5[0].parname='Period [d]' 
 par5[1].parname='Mplanet [M_E]' 
 par5[2].parname='Ecc'  
 par5[3].parname='Lil omega'   
 par5[4].parname='2440000-Tp ' 
 par5[5].parname='K (m/s)'
 par5[6].parname='a_pl (AU)'

;***************TABSTUFF************************************
 planetbase5 = widget_base(planettab,/column,title='   Planet 5   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase5, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar5bttn = widget_button(fitplanetbase, value = 'FIT PLANET 5 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar5bttn, /set_button

 resetbtn5 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 ;This button just shows which planet it is:
 green_image = BYTARR(6, 170, 3)
 green_image[*, *, 0] = 30
 green_image[*, *, 1] = 251
 green_image[*, *, 2] = 1
 plnt5clrbttn = widget_button(planet, value = green_image)
 plnt5bttnbgn = widget_button(planet, value = '5', $
   event_pro = 'kfme_fix5')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par5[0].value = initper
 par5[0].limited=[1,1]
 par5[0].limits=[0.75,30000.]
 par5[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar5 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar5, /row)
 
 textparperpl5 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl5 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl5')
 
 textparperypl5 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar5, /row)
 
 pertextpl5 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl5', xsize = 10)

 pertextypl5 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl5', xsize = 10)

 sliderperpl5 = widget_slider(orbpar5, event_pro = $
 	'kfme_sliperpl5', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar5, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl5 = widget_text(lowlim, value = $
 	strt(par5[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl5', xsize = '12')
 
 uplim = widget_base(orbpar5, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl5 = widget_text(uplim, value = strt(par5[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl5', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = .1d & par5[1].value = initmass
 par5[1].limited=[1,1]
 par5[1].limits=[0.01,5000.]
 par5[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl5 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl5 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl5', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl5 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl5')
 
 slidermasspl5 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl5', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl5 = widget_text(lowlim, value = strt(par5[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl5', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl5 = widget_text(uplim, value = strt(par5[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl5', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par5[2].value = initecc
 par5[2].limited=[1,1]
 par5[2].limits=[1d-9 ,0.99d]
 par5[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar5 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar5, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar5, /row)
 
 ecctextpl5 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl5', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl5 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl5')
 
 slidereccpl5 = widget_slider(orbpar5, event_pro = $
 	'kfme_slieccpl5', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar5, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl5 = widget_text(lowlim, value = strt(par5[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl5', xsize = '12')
 
 uplim = widget_base(orbpar5, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl5 = widget_text(uplim, value = strt(par5[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl5', xsize = '12')
 


 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par5[3].value = initlom
 par5[3].limited=[1,1]
 par5[3].limits=[0.0d,360.]
 par5[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl5 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl5', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl5 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl5')
 
 sliderlompl5 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl5', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl5 = widget_text(lowlim, value = strt(par5[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl5', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl5 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl5', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = min(cf3.jd) & par5[4].value = inittop
 par5[4].limited=[1,1]
 par5[4].limits=ToPLimits
 par5[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl5', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 toptextpl5 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl5', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl5 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl5')
 
 slidertoppl5 = widget_slider(orbpar6, $
 	event_pro = 'kfme_slitoppl5', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl5 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl5', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl5 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl5', xsize = '12')
 

 ;This button just shows which planet it is:
 plnt5bttnend = widget_button(planet, value = '5', $
   event_pro = 'kfme_fix5')



 ;**************************************************************
 ;           CREATE SIXTH PLANET:
 ;**************************************************************
 par6=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par6[0].parname='Period [d]' 
 par6[1].parname='Mplanet [M_E]' 
 par6[2].parname='Ecc'  
 par6[3].parname='Lil omega'   
 par6[4].parname='2440000-Tp ' 
 par6[5].parname='K (m/s)'
 par6[6].parname='a_pl (AU)'


;***************TABSTUFF************************************
 planetbase6 = widget_base(planettab,/column,title='   Planet 6   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase6, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar6bttn = widget_button(fitplanetbase, value = 'FIT PLANET 6 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar6bttn, /set_button

 resetbtn6 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 ;This button just shows which planet it is:
 blue_image = BYTARR(6, 170, 3)
 blue_image[*, *, 0] = 5
 blue_image[*, *, 1] = 13
 blue_image[*, *, 2] = 173
 plnt6clrbttn = widget_button(planet, value = blue_image)
 plnt6bttnbgn = widget_button(planet, value = '6', $
   event_pro = 'kfme_fix6')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par6[0].value = initper
 par6[0].limited=[1,1]
 par6[0].limits=[0.75,30000.]
 par6[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar6, /row)
 
 textparperpl6 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl6 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl6')
 
 textparperypl6 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar6, /row)
 
 pertextpl6 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl6', xsize = 10)

 pertextypl6 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl6', xsize = 10)

 sliderperpl6 = widget_slider(orbpar6, event_pro = $
 	'kfme_sliperpl6', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl6 = widget_text(lowlim, value = $
 	strt(par6[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl6', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl6 = widget_text(uplim, value = strt(par6[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl6', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = .1d & par6[1].value = initmass
 par6[1].limited=[1,1]
 par6[1].limits=[0.01,5000.]
 par6[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl6 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl6 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl6', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl6 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl6')
 
 slidermasspl6 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl6', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl6 = widget_text(lowlim, value = strt(par6[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl6', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl6 = widget_text(uplim, value = strt(par6[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl6', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par6[2].value = initecc
 par6[2].limited=[1,1]
 par6[2].limits=[1d-9 ,0.99d]
 par6[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar6, /row)
 
 ecctextpl6 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl6', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl6 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl6')
 
 slidereccpl6 = widget_slider(orbpar6, event_pro = $
 	'kfme_slieccpl6', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl6 = widget_text(lowlim, value = strt(par6[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl6', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl6 = widget_text(uplim, value = strt(par6[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl6', xsize = '12')
 

 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par6[3].value = initlom
 par6[3].limited=[1,1]
 par6[3].limits=[0.0d,360.]
 par6[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl6 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl6', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl6 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl6')
 
 sliderlompl6 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl6', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl6 = widget_text(lowlim, value = strt(par6[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl6', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl6 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl6', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = min(cf3.jd) & par6[4].value = inittop
 par6[4].limited=[1,1]
 par6[4].limits=ToPLimits
 par6[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl6', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 toptextpl6 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl6', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl6 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl6')
 
 slidertoppl6 = widget_slider(orbpar6, $
 	event_pro = 'kfme_slitoppl6', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl6 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl6', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl6 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl6', xsize = '12')
 

 
 ;This button just shows which planet it is:
 plnt6bttnend = widget_button(planet, value = '6', $
   event_pro = 'kfme_fix6')
 




 ;**************************************************************
 ;           CREATE SEVENTH PLANET:
 ;**************************************************************
 par7=replicate({parname:'', value:0.d, fixed:0, $
					limited:[0,0], limits:[0.d, 0.d], $
					step:0., error:0.d},7)

 par7[0].parname='Period [d]' 
 par7[1].parname='Mplanet [M_E]' 
 par7[2].parname='Ecc'  
 par7[3].parname='Lil omega'   
 par7[4].parname='2440000-Tp ' 
 par7[5].parname='K (m/s)'
 par7[6].parname='a_pl (AU)'


;***************TABSTUFF************************************
 planetbase7 = widget_base(planettab,/column,title='   Planet 7   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase7, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar7bttn = widget_button(fitplanetbase, value = 'FIT PLANET 7 ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar7bttn, /set_button

 resetbtn7 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 
 ;This button just shows which planet it is:
 purple_image = BYTARR(6, 170, 3)
 purple_image[*, *, 0] = 143
 purple_image[*, *, 1] = 9
 purple_image[*, *, 2] = 228
 plnt7clrbttn = widget_button(planet, value = purple_image)
 plnt7bttnbgn = widget_button(planet, value = '7', $
   event_pro = 'kfme_fix7')
 
 ;PERIOD FIELDS
 
 ;the initial period:
 initper = 1d & par7[0].value = initper
 par7[0].limited=[1,1]
 par7[0].limits=[0.75,30000.]
 par7[0].step=0.5

 ;Create a base to hold each orbital parameter:
 orbpar7 = widget_base(planet, /col, frame = 1)
 
 pertitle = widget_base(orbpar7, /row)
 
 textparperpl7 = widget_text(pertitle, value = 'P [d]', $
 event_pro = 'kfme_par', xsize = 5)
 
 fixpar = widget_base(pertitle, /row)
 radiobase = widget_base(fixpar, /nonexclusive)
 fixperpl7 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixperpl7')
 
 textparperypl7 = widget_text(pertitle, value = 'P [y]', $
 event_pro = 'kfme_par', xsize = 5)
 
 
 pernums = widget_base(orbpar7, /row)
 
 pertextpl7 = widget_text(pernums, value = strt(initper), $
 	/editable, event_pro = 'kfme_texperpl7', xsize = 10)

 pertextypl7 = widget_text(pernums, value = strt(initper/365.2564d), $
 	/editable, event_pro = 'kfme_texperypl7', xsize = 10)

 sliderperpl7 = widget_slider(orbpar7, event_pro = $
 	'kfme_sliperpl7', /suppress_value, /drag, $
 	minimum = 1, maximum = 30000., value = initper)
 
 lowlim = widget_base(orbpar7, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 periodlowlimpl7 = widget_text(lowlim, value = $
 	strt(par7[0].limits[0]), /editable, event_pro = $
 	'kfme_perlowlimpl7', xsize = '12')
 
 uplim = widget_base(orbpar7, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 periodhilimpl7 = widget_text(uplim, value = strt(par7[0].limits[1]), $
 	/editable, event_pro = 'kfme_perhilimpl7', xsize = '12')




 ;PLANET MASS FIELDS
 
 ;Create a base to hold each orbital parameter:
 initmass = .1d & par7[1].value = initmass
 par7[1].limited=[1,1]
 par7[1].limits=[0.01,5000.]
 par7[1].step=0.1
 
 orbpar2 = widget_base(planet, /col, frame =1)
 
 textparpl7 = widget_text(orbpar2, value = 'Mplanet [M_E]', $
 event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar2, /row)
 
 masstextpl7 = widget_text(fixpar, value = strt(initmass), /editable, $
 event_pro = 'kfme_texmasspl7', xsize = 10) ;, /all_events

 radiobase = widget_base(fixpar, /nonexclusive)
 fixmasspl7 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixmasspl7')
 
 slidermasspl7 = widget_slider(orbpar2, event_pro = $
 	'kfme_slimasspl7', /suppress_value, /drag, $
 	minimum = 1, maximum = 5d5, value = initmass*100.)

 lowlim = widget_base(orbpar2, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl7 = widget_text(lowlim, value = strt(par7[1].limits[0]), $
 /editable, $
 event_pro = 'kfme_masslowlimpl7', xsize = '12')
 
 uplim = widget_base(orbpar2, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl7 = widget_text(uplim, value = strt(par7[1].limits[1]), $
 	/editable, event_pro = 'kfme_masshilimpl7', xsize = '12')
 


 ;ECCENTRICITY FIELDS
 
 ;the initial eccentricity:
 initecc = 0.05d & par7[2].value = initecc
 par7[2].limited=[1,1]
 par7[2].limits=[1d-9 ,0.99d]
 par7[2].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar7 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar7, value = 'Eccentricity', $
 event_pro = 'kfme_par', xsize = 12)
 
 fixpar = widget_base(orbpar7, /row)
 
 ecctextpl7 = widget_text(fixpar, value = strt(initecc), $
 /editable, $
 event_pro = 'kfme_texeccpl7', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)
 fixeccpl7 = widget_button(radiobase, value = 'fix', $
 event_pro = 'kfme_fixeccpl7')
 
 slidereccpl7 = widget_slider(orbpar7, event_pro = $
 	'kfme_slieccpl7', /suppress_value, /drag, $
 	minimum = 1.d, maximum = 999.d, value = initecc*1000.)

 lowlim = widget_base(orbpar7, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl7 = widget_text(lowlim, value = strt(par7[2].limits[0]), $
 /editable, $
 event_pro = 'kfme_ecclowlimpl7', xsize = '12')
 
 uplim = widget_base(orbpar7, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl7 = widget_text(uplim, value = strt(par7[2].limits[1]), $
 /editable, event_pro = 'kfme_ecchilimpl7', xsize = '12')
 


 ;LITTLE OMEGA FIELDS
 
 ;set the initial value for lil om:
 initlom = 292.d & par7[3].value = initlom
 par7[3].limited=[1,1]
 par7[3].limits=[0.0d,360.]
 par7[3].step=0.1
 
 ;Create a base to hold each orbital parameter:
 orbpar6 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar6, value = 'Lil omega', $
 	event_pro = 'kfme_par', xsize = 10)
 
 fixpar = widget_base(orbpar6, /row)
 
 lomtextpl7 = widget_text(fixpar, value = strt(initlom), $
 	/editable, event_pro = 'kfme_texlompl7', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixlompl7 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixlompl7')
 
 sliderlompl7 = widget_slider(orbpar6, event_pro = $
 	'kfme_slilompl7', /suppress_value, /drag, $
 	minimum = 0.0, maximum = 360., value = initlom)

 lowlim = widget_base(orbpar6, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl7 = widget_text(lowlim, value = strt(par7[3].limits[0]), $
 	/editable, event_pro = 'kfme_lomlowlimpl7', xsize = '12')
 
 uplim = widget_base(orbpar6, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl7 = widget_text(uplim, value = '360', /editable, $
 	event_pro = 'kfme_lomhilimpl7', xsize = '12')
 


 ;TIME OF PERIASTRON FIELDS
 ;create the initial value:
 inittop = min(cf3.jd) & par7[4].value = inittop
 par7[4].limited=[1,1]
 par7[4].limits=ToPLimits
 par7[4].step=0.5
 
 ;Create a base to hold each orbital parameter:
 orbpar7 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar7, value = 'T_0 RV [d]', $
 	event_pro = 'kfme_textoppl7', xsize = 10)
 
 fixpar = widget_base(orbpar7, /row)
 
 toptextpl7 = widget_text(fixpar, value = strt(inittop), $
 	/editable, event_pro = 'kfme_textoppl7', xsize = 10)

 radiobase = widget_base(fixpar, /nonexclusive)

 fixtoppl7 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixtoppl7')
 
 slidertoppl7 = widget_slider(orbpar7, $
 	event_pro = 'kfme_slitoppl7', /suppress_value, $
 	/drag, value = inittop, minimum = 0.d, maximum = initper + max(cf3.jd))
 
 lowlim = widget_base(orbpar7, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 8)
 
 lowlimvalpl7 = widget_text(lowlim, value = '1.00', /editable, $
 	event_pro = 'kfme_toplowlimpl7', xsize = '12')
 
 uplim = widget_base(orbpar7, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 8)
 
 uplimvalpl7 = widget_text(uplim, value = '10000', /editable, $
 	event_pro = 'kfme_tophilimpl7', xsize = '12')
 
 ;This button just shows which planet it is:
 plnt7bttnend = widget_button(planet, value = '7', $
   event_pro = 'kfme_fix7')
 

 ;**************************************************************
 ;           TRENDS:
 ;**************************************************************
;***************TABSTUFF************************************
 planetbase8 = widget_base(planettab,/column,title='   Trends   ')

 ;Make a row base to hold a series of controls:
 planet = widget_base(planetbase8, /row)

 planetcontrolbase = widget_base(planet, /col)
 fitplanetbase = widget_base(planetcontrolbase, /row, /nonexclusive)
 fitpar8bttn = widget_button(fitplanetbase, value = 'FIT TRENDS ', $
        event_pro = 'kfme_fitplnum')
 
 widget_control, fitpar8bttn, /set_button

 resetbtn8 = widget_button(planetcontrolbase,value = 'RESET PARAMETERS',event_pro = 'kfme_resetparam', xsize = 120)
 ;***************TABSTUFF************************************
 

 ; OFFSET RV FIELDS
 ;create the initial value:
 initrvo = 0.0d & par1[5].value = initrvo
 par1[5].limited=[1,1]
 par1[5].limits=[-200.d,200.]
 par1[5].step=0.01
 
 ;Create a base to hold each orbital parameter:
 orbpar5 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar5, value = 'OFFSET RV [M/S]', $
 event_pro = 'kfme_texrvo', xsize = 10)
 
 fixparrvo = widget_base(orbpar5, /row)
 
 rvotextpl1 = widget_text(fixparrvo, value = strt(initrvo), $
 	/editable, event_pro = 'kfme_texrvo', xsize = 10)

 radiobase = widget_base(fixparrvo, /nonexclusive)

 fixrvopl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixrvo')
 
 sliderrvopl1 = widget_slider(orbpar5, $
 	event_pro = 'kfme_slirvo', /suppress_value, $
 	/drag, value = initrvo/10000., minimum = -200.d, maximum = 200.d)
 
 lowlim = widget_base(orbpar5, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 9)
 
 lowlimvalpl1 = widget_text(lowlim, value = '-30', /editable, $
 	event_pro = 'kfme_rvolowlim', xsize = '12')
 
 uplim = widget_base(orbpar5, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 9)
 
 uplimvalpl1 = widget_text(uplim, value = '30', /editable, $
 	event_pro = 'kfme_rvohilim', xsize = '12')

 
  ;SLOPE RV FIELDS
 ;create the initial value:
 initsrv = 0.0d & par1[6].value = initsrv
 par1[6].limited=[1,1]
 par1[6].limits=[-100.d,100.d]
 par1[6].step=0.01
 par1[6].fixed = 1

 ;Create a base to hold each orbital parameter:
 orbpar12 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar12, value = 'SLOPE RV [M/S/YR]', $
 	event_pro = 'kfme_texsrv', xsize = 10)
 
 fixparsrv = widget_base(orbpar12, /row)
 
 srvtextpl1 = widget_text(fixparsrv, value = strt(initsrv), $
 	/editable, event_pro = 'kfme_texsrv', xsize = 10)

 radiobase = widget_base(fixparsrv, /nonexclusive)

 fixsrvpl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixsrv')
 
 slidersrvpl1 = widget_slider(orbpar12, $
 	event_pro = 'kfme_slisrv', /suppress_value, $
 	/drag, value = initsrv, minimum = -200.d, maximum = 200.d)
 
 lowlim = widget_base(orbpar12, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 12)
 
 lowlimvalpl1 = widget_text(lowlim, value = '-100', /editable, $
 	event_pro = 'kfme_srvlowlim', xsize = '12')
 
 uplim = widget_base(orbpar12, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 12)
 
 uplimvalpl1 = widget_text(uplim, value = '100', /editable, $
 	event_pro = 'kfme_srvhilim', xsize = '12')

 widget_control, fixsrvpl1, /set_button
 
 
 ;CURVE RV FIELDS
 ;create the initial value:
 initcrv = 0.0d & par1[7].value = initcrv
 par1[7].limited=[1,1]
 par1[7].limits=[-15d,15d]
 par1[7].step=0.01d
 par1[7].fixed = 1
 
 ;Create a base to hold each orbital parameter:
 orbpar15 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar15, value = 'CURVE RV [M/S/YR^2]', $
 	event_pro = 'kfme_texcrv', xsize = 10)
 
 fixparcrv = widget_base(orbpar15, /row)
 
 crvtextpl1 = widget_text(fixparcrv, value = strt(initcrv), $
 	/editable, event_pro = 'kfme_texcrv', xsize = 10)

 radiobase = widget_base(fixparcrv, /nonexclusive)

 fixcrvpl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixcrv')
 
 slidercrvpl1 = widget_slider(orbpar15, $
 	event_pro = 'kfme_slicrv', /suppress_value, $
 	/drag, value = initcrv, minimum = -200.d, maximum = 200.d)
 
 lowlim = widget_base(orbpar15, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 12)
 
 lowlimvalpl1 = widget_text(lowlim, value = strt(par1[7].limits[0]), $
 /editable, event_pro = 'kfme_crvlowlim', xsize = '12')
 
 uplim = widget_base(orbpar15, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 12)
 
 uplimvalpl1 = widget_text(uplim, value = strt(par1[7].limits[1]), $
 /editable, event_pro = 'kfme_crvhilim', xsize = '12')

 widget_control, fixcrvpl1, /set_button
 
 
 
 ; DEWAR24 OFFSET FIELDS
 ;create the initial value:
 dew24 = 0.0d & par1[8].value = dew24
 par1[8].limited=[1,1]
 par1[8].limits=[-5d2,5d2]
 par1[8].step=0.01d
 par1[8].fixed = 1
 
 ;Create a base to hold each orbital parameter:
 orbpar9 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar9, value = 'DEWAR24 OFFSET [M/S]', $
 event_pro = 'kfme_texdew24', xsize = 10)
 
 fixpardew = widget_base(orbpar9, /row)
 
 dew24text = widget_text(fixpardew, value = strt(dew24), $
 	/editable, event_pro = 'kfme_texdew24', xsize = 10)

 radiobase = widget_base(fixpardew, /nonexclusive)

 fixdewpl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixdew24')
 
 sliderdew24 = widget_slider(orbpar9, $
 	event_pro = 'kfme_slidew24', /suppress_value, $
 	/drag, value = dew24*100., minimum = -1500.d, maximum = 1500.d)
 
 lowlim = widget_base(orbpar9, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 9)
 
 lowlimvalpl1 = widget_text(lowlim, $
	 value = strt(par1[8].limits[0]), /editable, $
 	 event_pro = 'kfme_dew24lowlim', xsize = '12')
 
 uplim = widget_base(orbpar9, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 9)
 
 uplimvalpl1 = widget_text(uplim, $
	 value = strt(par1[8].limits[1]), /editable, $
 	 event_pro = 'kfme_dew24hilim', xsize = '12')

 ; DEWAR39 OFFSET FIELDS
 ;create the initial value:
 dew39 = 0.0d & par1[9].value = dew39
 par1[9].limited=[1,1]
 par1[9].limits=[-5d2,5d2]
 par1[9].step=0.01d
 par1[9].fixed = 1
 
 ;Create a base to hold each orbital parameter:
 orbpar9 = widget_base(planet, /col, frame =1)
 
 textpar = widget_text(orbpar9, value = 'DEWAR39 OFFSET [M/S]', $
 event_pro = 'kfme_texdew39', xsize = 10)
 
 fixpardew = widget_base(orbpar9, /row)
 
 dew39text = widget_text(fixpardew, value = strt(dew39), $
 	/editable, event_pro = 'kfme_texdew39', xsize = 10)

 radiobase = widget_base(fixpardew, /nonexclusive)

 fixdewpl1 = widget_button(radiobase, value = 'fix', $
 	event_pro = 'kfme_fixdew39')
 
 sliderdew39 = widget_slider(orbpar9, $
 	event_pro = 'kfme_slidew39', /suppress_value, $
 	/drag, value = dew39*100., minimum = -1500.d, maximum = 1500.d)
 
 lowlim = widget_base(orbpar9, /row)
 
 lowlimtext = widget_text(lowlim, value = 'LOW_LIM:', xsize = 9)
 
 lowlimvalpl1 = widget_text(lowlim, $
	 value = strt(par1[9].limits[0]), /editable, $
 	 event_pro = 'kfme_dew39lowlim', xsize = '12')
 
 uplim = widget_base(orbpar9, /row)

 uplimtext = widget_text(uplim, value = 'UP_LIM:', xsize = 9)
 
 uplimvalpl1 = widget_text(uplim, $
	 value = strt(par1[9].limits[1]), /editable, $
 	 event_pro = 'kfme_dew39hilim', xsize = '12')




;**********************************************************************
;**********************************************************************
;**********************************************************************
;                  END OF PLANET WIDGET SECTION
;**********************************************************************
;**********************************************************************
;**********************************************************************

 ;Draw the widget hierarchy to the screen:
 widget_control, tlb, /realize, tlb_set_yoffset=0;, xoffset=1920

 ;Get the window index of the draw widget:
 widget_control, draw, get_value=win_id
 
 ;Create the structure to hold the structures for each planet:
 pars = {par1:par1, $
 			par2:par2, $
 			par3:par3, $
 			par4:par4, $
 			par5:par5, $
 			par6:par6, $
 			par7:par7}
 			
 

 ;The are the fix parameter widget buttons:
 fixbttns = {fixperpl1:fixperpl1, $
				 fixmasspl1:fixmasspl1, $
				 fixeccpl1:fixeccpl1, $
				 ;fixincpl1:fixincpl1, $
				 ;fixbompl1:fixbompl1, $
				 fixlompl1:fixlompl1, $
				 fixtoppl1:fixtoppl1, $
				 ;fixaoxpl1:fixaoxpl1, $
				 ;fixaoypl1:fixaoypl1, $
				 fixrvopl1:fixrvopl1, $
				 ;fixsrapl1:fixsrapl1, $
				 ;fixsdcpl1:fixsdcpl1, $
				 fixsrvpl1:fixsrvpl1, $
				 ;fixcrapl1:fixcrapl1, $
				 ;fixcdcpl1:fixcdcpl1, $
				 fixcrvpl1:fixcrvpl1, $
				 ;fixlaxpl1:fixlaxpl1, $
				 ;fixprapl1:fixprapl1, $
				 ;fixpdcpl1:fixpdcpl1, $
				 fixperpl2:fixperpl2, $
				 fixmasspl2:fixmasspl2, $
				 fixeccpl2:fixeccpl2, $
				 ;fixincpl2:fixincpl2, $
				 ;fixbompl2:fixbompl2, $
				 fixlompl2:fixlompl2, $
				 fixtoppl2:fixtoppl2, $
				 fixperpl3:fixperpl3, $
				 fixmasspl3:fixmasspl3, $
				 fixeccpl3:fixeccpl3, $
				 ;fixincpl3:fixincpl3, $
				 ;fixbompl3:fixbompl3, $
				 fixlompl3:fixlompl3, $
				 fixtoppl3:fixtoppl3, $
				 fixperpl4:fixperpl4, $
				 fixmasspl4:fixmasspl4, $
				 fixeccpl4:fixeccpl4, $
				 ;fixincpl4:fixincpl4, $
				 ;fixbompl4:fixbompl4, $
				 fixlompl4:fixlompl4, $
				 fixtoppl4:fixtoppl4, $
				 fixperpl5:fixperpl5, $
				 fixmasspl5:fixmasspl5, $
				 fixeccpl5:fixeccpl5, $
				 ;fixincpl5:fixincpl5, $
				 ;fixbompl5:fixbompl5, $
				 fixlompl5:fixlompl5, $
				 fixtoppl5:fixtoppl5, $
				 fixperpl6:fixperpl6, $
				 fixmasspl6:fixmasspl6, $
				 fixeccpl6:fixeccpl6, $
				 ;fixincpl6:fixincpl6, $
				 ;fixbompl6:fixbompl6, $
				 fixlompl6:fixlompl6, $
				 fixtoppl6:fixtoppl6, $
				 fixperpl7:fixperpl7, $
				 fixmasspl7:fixmasspl7, $
				 fixeccpl7:fixeccpl7, $
				 ;fixincpl7:fixincpl7, $
				 ;fixbompl7:fixbompl7, $
				 fixlompl7:fixlompl7, $
				 fixtoppl7:fixtoppl7}

 ;These are the planet tabs
 planettabs = [planetbase1,planetbase2,planetbase3, $
 					planetbase4,planetbase5,planetbase6, $
 					planetbase7]
 					
 ;These are the planet tabs
 resetbtns = [resetbtn1,resetbtn2,resetbtn3, $
 					resetbtn4,resetbtn5,resetbtn6, $
 					resetbtn7]
 					
 ;These are the fit planet button widgets:
 fitplbttns = {fitpar1bttn:fitpar1bttn, $
 					fitpar2bttn:fitpar2bttn, $
 					fitpar3bttn:fitpar3bttn, $
 					fitpar4bttn:fitpar4bttn, $
 					fitpar5bttn:fitpar5bttn, $
 					fitpar6bttn:fitpar6bttn, $
 					fitpar7bttn:fitpar7bttn}
 					
 ;Create a structure to house all of the text fields which will then 
 ; go into the state structure:
 txtflds = {pertextpl1:pertextpl1, $
            pertextypl1:pertextypl1, $
            masstextpl1:masstextpl1, $
            ecctextpl1:ecctextpl1, $
            ;inctextpl1:inctextpl1, $
            ;bomtextpl1:bomtextpl1, $
            lomtextpl1:lomtextpl1, $
            toptextpl1:toptextpl1, $
            ;aoxtextpl1:aoxtextpl1, $
            ;aoytextpl1:aoytextpl1, $
            rvotextpl1:rvotextpl1, $
            ;sratextpl1:sratextpl1, $
            ;sdctextpl1:sdctextpl1, $
            srvtextpl1:srvtextpl1, $
            ;cratextpl1:cratextpl1, $
            ;cdctextpl1:cdctextpl1, $
            crvtextpl1:crvtextpl1, $
            ;laxtextpl1:laxtextpl1, $
            ;pratextpl1:pratextpl1, $
            ;pdctextpl1:pdctextpl1, $
            dew24text:dew24text, $
            dew39text:dew39text, $
				pertextpl2:pertextpl2, $
            pertextypl2:pertextypl2, $
            masstextpl2:masstextpl2, $
            ecctextpl2:ecctextpl2, $
            ;inctextpl2:inctextpl2, $
            ;bomtextpl2:bomtextpl2, $
            lomtextpl2:lomtextpl2, $
            toptextpl2:toptextpl2, $
            pertextypl3:pertextypl3, $
				pertextpl3:pertextpl3, $
            masstextpl3:masstextpl3, $
            ecctextpl3:ecctextpl3, $
            ;inctextpl3:inctextpl3, $
            ;bomtextpl3:bomtextpl3, $
            lomtextpl3:lomtextpl3, $
            toptextpl3:toptextpl3, $
				pertextpl4:pertextpl4, $
            pertextypl4:pertextypl4, $
            masstextpl4:masstextpl4, $
            ecctextpl4:ecctextpl4, $
            ;inctextpl4:inctextpl4, $
            ;bomtextpl4:bomtextpl4, $
            lomtextpl4:lomtextpl4, $
            toptextpl4:toptextpl4, $
				pertextpl5:pertextpl5, $
            pertextypl5:pertextypl5, $
            masstextpl5:masstextpl5, $
            ecctextpl5:ecctextpl5, $
            ;inctextpl5:inctextpl5, $
            ;bomtextpl5:bomtextpl5, $
            lomtextpl5:lomtextpl5, $
            toptextpl5:toptextpl5, $
				pertextpl6:pertextpl6, $
            pertextypl6:pertextypl6, $
            masstextpl6:masstextpl6, $
            ecctextpl6:ecctextpl6, $
            ;inctextpl6:inctextpl6, $
            ;bomtextpl6:bomtextpl6, $
            lomtextpl6:lomtextpl6, $
            toptextpl6:toptextpl6, $
				pertextpl7:pertextpl7, $
            pertextypl7:pertextypl7, $
            masstextpl7:masstextpl7, $
            ecctextpl7:ecctextpl7, $
            ;inctextpl7:inctextpl7, $
            ;bomtextpl7:bomtextpl7, $
            lomtextpl7:lomtextpl7, $
            toptextpl7:toptextpl7}


 ;Create a structure to house all of the slider fields:
 sliderflds = {sliderperpl1:sliderperpl1, $
               slidermasspl1:slidermasspl1, $
               slidereccpl1:slidereccpl1, $
               ;sliderincpl1:sliderincpl1, $
               ;sliderbompl1:sliderbompl1, $
               sliderlompl1:sliderlompl1, $
               slidertoppl1:slidertoppl1, $
               ;slideraoxpl1:slideraoxpl1, $
               ;slideraoypl1:slideraoypl1, $
               sliderrvopl1:sliderrvopl1, $
               ;slidersrapl1:slidersrapl1, $
               ;slidersdcpl1:slidersdcpl1, $
               slidersrvpl1:slidersrvpl1, $
               ;slidercrapl1:slidercrapl1, $
               ;slidercdcpl1:slidercdcpl1, $
               slidercrvpl1:slidercrvpl1, $
               ;sliderlaxpl1:sliderlaxpl1, $
               ;sliderprapl1:sliderprapl1, $
               ;sliderpdcpl1:sliderpdcpl1, $
               sliderdew24:sliderdew24, $
               sliderdew39:sliderdew39, $
					sliderperpl2:sliderperpl2, $
               slidermasspl2:slidermasspl2, $
               slidereccpl2:slidereccpl2, $
               ;sliderincpl2:sliderincpl2, $
               ;sliderbompl2:sliderbompl2, $
               sliderlompl2:sliderlompl2, $
               slidertoppl2:slidertoppl2, $
					sliderperpl3:sliderperpl3, $
               slidermasspl3:slidermasspl3, $
               slidereccpl3:slidereccpl3, $
               ;sliderincpl3:sliderincpl3, $
               ;sliderbompl3:sliderbompl3, $
               sliderlompl3:sliderlompl3, $
               slidertoppl3:slidertoppl3, $
					sliderperpl4:sliderperpl4, $
               slidermasspl4:slidermasspl4, $
               slidereccpl4:slidereccpl4, $
               ;sliderincpl4:sliderincpl4, $
               ;sliderbompl4:sliderbompl4, $
               sliderlompl4:sliderlompl4, $
               slidertoppl4:slidertoppl4, $
					sliderperpl5:sliderperpl5, $
               slidermasspl5:slidermasspl5, $
               slidereccpl5:slidereccpl5, $
               ;sliderincpl5:sliderincpl5, $
               ;sliderbompl5:sliderbompl5, $
               sliderlompl5:sliderlompl5, $
               slidertoppl5:slidertoppl5, $
					sliderperpl6:sliderperpl6, $
               slidermasspl6:slidermasspl6, $
               slidereccpl6:slidereccpl6, $
               ;sliderincpl6:sliderincpl6, $
               ;sliderbompl6:sliderbompl6, $
               sliderlompl6:sliderlompl6, $
               slidertoppl6:slidertoppl6, $
					sliderperpl7:sliderperpl7, $
               slidermasspl7:slidermasspl7, $
               slidereccpl7:slidereccpl7, $
               ;sliderincpl7:sliderincpl7, $
               ;sliderbompl7:sliderbompl7, $
               sliderlompl7:sliderlompl7, $
               slidertoppl7:slidertoppl7}
               
 
 ;Widgets in the Control Bar:
 
 controlbar = { aloneplanet:aloneplanet, $
				plalnumbox:plalnumbox, $
				phasebool:phasebool, $
				phasebutton:phasebutton, $
				phasestart:phasestart, $
				datname:datname, $
				yranbutton:yranbutton, $
				ymaxval:ymaxval, $
				yminval:yminval, $
				smassval: smassval, $
				smassuncval:smassuncval, $
				sradiusval:sradiusval, $
				sradiusuncval:sradiusuncval, $
 			   jitterbox:jitterbox, $
            psplotbutton:psplotbutton, $
            zoomslide:zoomslide, $
            scrollslide:scrollslide, $
            ;zoomrvslide:zoomrvslide, $
            multithbutton:multithbutton, $
            togerrbutton:togerrbutton, $
            connectbutton:connectbutton}
 

 ;Create a structure of data for the application:
 state = {ast:ast, $
 			 botrow:botrow, $
 			 chisq:chisq, $
 			 combperg:combperg, $
 			 controlbar:controlbar, $
 			 connect:1, $
 			 controlbase:controlbase, $
 			 ndof:ndof, $
 			 datadir:datadir, $
 			 dew24:dew24, $
 			 dew39:dew39, $
 			 draw:draw, $
 			 fapiter:fapiter, $
 			 fixbttns:fixbttns, $
 			 fitplarr:fitplarr, $
 			 fitplbttns:fitplbttns, $
 			 import_delimiter:import_delimiter, $
 			 import_errunit:import_errunit, $
 			 import_jdoff:import_jdoff, $
 			 import_rvunit:import_rvunit, $
 			 import_skiplines:import_skiplines, $
 			 jitternum:jitternum, $
 			 kfmedir:kfmedir, $
 			 linestyle:0, $
 			 multith:multith, $
 			 outputdir:outputdir, $
 			 p_orig:p_orig, $
 			 par1:par1, $
 			 pars:pars, $
 			 pcf:pcf, $
 			 pcf_resid:pcf_resid, $
 			 pcfname:pcfname, $
 			 pdatls:pdatls, $
 			 pdata:pdata, $
 			 pdatname:pdatname, $
 			 perghi:10000, $
 			 perglow:0.75, $
 			 pergres:pergres, $
 			 ppergfap:ppergfap, $
 			 pergfapbool:pergfapbool, $ 			 
 			 pfunctargs:pfunctargs, $
 			 planettabs:planettabs, $
 			 plot_time_study:functargs.time_study, $
 			 previousjitter:previousjitter, $
 			 printpars:printpars, $
 			 psplot:psplot, $
 			 psym:8, $
 			 ptransit:ptransit, $
 			 resetbtns:resetbtns, $
 			 rmsresid:rmsresid, $
 			 rv:rv, $
 			 scroll:scroll, $
 			 sliderflds:sliderflds, $
 			 starlist:starlist, $
 			 tfine:tfine, $
 			 titleflag:titleflag, $
 			 togerr:togerr, $
 			 txtflds:txtflds, $
 			 toprow:toprow, $
 			 win_id:win_id, $
 			 xmin:xmin, $
 			 xmax:xmax, $
 			 yminmax:yminmax, $
 			 yminval:yminval, $
 			 ymaxval:ymaxval, $
 			 zoomplot:zoomplot}
 			 ;zoomrv:zoomrv
 
 ; Create a pointer to the state structure and put that pointer
 ; into the user value of the top-level base.
 pstate = ptr_new(state, /no_copy, /allocate)
 
 widget_control, tlb, set_uvalue = pstate
 

 
 ;Plot the data using the "do" routine:
 kfme_dofit, pstate
 
 
 ;Call XMANAGER to start event handling:
 xmanager, 'mpf_widget', tlb, $
 cleanup='kfme_cleanup', $
 event_handler='kfme_resize', $
 /no_block
 
end;mpf_widget.pro