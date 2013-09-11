function t_eff,input,bmv=bmv,spln=spln
;
;  This function is designed to return the effective temperature for a
;  main sequence star given either its spectral type or B-V.  The defualt
;  assumes the spectral type is the input.  Calibration comes from 
;  Johnson, H.L. 1966, ARAA, 4, 193, and is intended for stars between B0V
;  and M8V.  This reference is often used for TTS, and seems more correct 
;  than for example Gray 1996 at the later spectral types.  The routine 
;  normally performs linear interpolation, but a cubic spline can be invoked
;  by keyword.
;  INPUTS:
;    input - input quantity, default is a string variable assumed to be
;            the spectral type.  If keyword bmv set, this is assumed to
;            be B-V.
;    bmv   - keyword set if bmv is the value of B-V.
;    spln  - perform a cubic spline.
;
;  OUTPUTS:
;    Function returns the effective temperature for the star.
;
;  HISTORY:
;    01-Sep-99 CMJ Written.
;
if n_params(0) lt 1 then begin
   print,"Syntax is: result = T_eff('spty' [,/bmv,/spln])"
   retall
endif

spty = ['B0','B0.5','B1','B2','B3','B5','B6','B7','B8','B9','A0','A2','A5', $
        'A7','F0','F2','F5','F8','G0','G2','G5','G8','K0','K2','K5','K7', $
        'M0','M1','M2','M3','M4','M5','M6','M7','M8']

sptyn = [0.,0.5,1.,2.,3.,5.,6.,7.,8.,9.,10.,12.,15.,17.,20.,22.,25.,28.,30., $
         32.,35.,38.,40.,42.,45.,47.,50.,51.,52.,53.,54.,55.,56.,57.,58.]

bv = [-0.30,-0.28,-0.26,-0.24,-0.20,-0.16,-0.14,-0.12,-0.09,-0.06,0.00, $
      0.06,0.14,0.19,0.31,0.36,0.43,0.54,0.59,0.63,0.66,0.75,0.82,0.92,1.15, $
      1.30,1.41,1.48,1.52,1.55,1.56,1.61,1.72,1.84,2.00]

teff = [26500.,24500.,21500.,18000.,15500.,13800.,12900.,12200.,11300., $
        10600.,9850.,9120.,8260.,7880.,7030.,6700.,6400.,6000.,5900.,5770., $
        5660.,5440.,5240.,4960.,4400.,4000.,3750.,3600.,3400.,3300.,3200., $
        3100.,2950.,2850.,2750.]

if keyword_set(bmv) then begin
   aout=where(input lt -0.30 or input gt 2.00,nout)
   if nout gt 0 then begin
      message,/info,'Some requested value outside data range: Extrapolating!'
   endif
   if keyword_set(spln) then begin
      temp = spline(bv,teff,input,0.01)
   endif else begin
      temp = interpol(teff,bv,input)
   endelse
endif else begin
   letter = strmid(input,0,1)
   nstar = n_elements(letter)
   spn = fltarr(nstar)-10.                        ; set spectral type flag
   for i=0,nstar-1 do begin
      if letter(i) eq 'b' or letter(i) eq 'B' then spn(i)=0.
      if letter(i) eq 'a' or letter(i) eq 'A' then spn(i)=10.
      if letter(i) eq 'f' or letter(i) eq 'F' then spn(i)=20.
      if letter(i) eq 'g' or letter(i) eq 'G' then spn(i)=30.
      if letter(i) eq 'k' or letter(i) eq 'K' then spn(i)=40.
      if letter(i) eq 'm' or letter(i) eq 'M' then spn(i)=50.
      if spn(i) lt -1. then begin
         message,'Requested spectral type out of range.  Must be B0 - M8'
      endif
      spn(i)=spn(i)+float(strmid(input(i),1,5))
   endfor
   if keyword_set(spln) then begin
      temp = spline(sptyn,teff,spn,0.01)
   endif else begin
      temp = interpol(teff,sptyn,spn)
   endelse
endelse

return,temp
end

