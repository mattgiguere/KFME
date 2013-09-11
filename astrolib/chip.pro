function chip,tpname,gain,telescope,wav_scale,slit,mask
;  this routine figures out which dewar (chip) was used on a 
;  give observation
;  tpname is an input string (i.e.  'rh84.23' ...  'ra25.41')
;  dwr is an output integer (i.e. 1 or 2  or 6 or 8 or 13)
;  wav_scale (output) wavelength scale appropriate for chip


letters=strmid(tpname,0,2)  ; i.e. 'em'
if (letters ne 'em') then begin
    numbers=fix(strmid(tpname,2,2))
endif else begin
    numbers = -1
endelse

dwr=-1
gain=2.5
if letters eq 'rh' and numbers ge 16 and numbers lt 21 then dwr=1
if letters eq 'rh' and numbers gt 20 and numbers lt 25 then dwr=2
if letters eq 'rh' and numbers ge 25 then dwr=6
if letters eq 'rh' and numbers eq 86 then dwr=8
if letters eq 'rh' and numbers eq 87 then dwr=8
;
if letters eq 'ra' and numbers lt 11 then dwr=6
if letters eq 'ra' and numbers ge 11 and numbers lt 18 then dwr=13
if letters eq 'ra' and numbers ge 18 then dwr=8
;
if letters eq 'rb' and numbers lt 2 then dwr=8
if letters eq 'rb' and numbers ge 2 then begin
   dwr=39
   if numbers lt 14 then gain=1.33 else gain=2.66
endif
if letters eq 'rb' and numbers ge 69 then begin
   dwr=18
   gain=2.80
endif
;rb86 is the emergency Dewar #13 run (Dewar #6 being repaired)
if letters eq 'rb' and numbers eq 86 then begin
   dwr=39
   gain=2.80
endif
if letters eq 'rs' and numbers ge 1 then begin
   dwr=18
   gain=2.80
endif
;rs4 is an emergency dewar13 run
if letters eq 'rs' and numbers eq 4 then begin
   dwr=39
   gain=2.66
endif
if letters eq 'rd' and numbers le 20 then begin
   dwr=18
   gain=2.80
endif
if letters eq 'rd' and numbers eq 21 then begin
   dwr=24   ;dewar 8 high resistivity ccd
   gain=1.20
endif
if letters eq 'rd' and numbers ge 22 then begin
   dwr=18
   gain=2.80
endif
if letters eq 'rd' and numbers eq 67 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif
if letters eq 'rd' and numbers eq 70 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif
if letters eq 'rd' and numbers eq 73 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 74 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 77 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 81 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 86 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 93 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 95 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 96 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 98 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rd' and numbers eq 99 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;remeasured the gain with richard stover on oct 26,2001
endif;
if letters eq 'rf' and numbers eq 1 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;New tag series for Lick in 2002
endif;
if letters eq 'rg' then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=8.1    ;New tag series for Lick in 2002
endif;
if letters eq 'rf' and numbers eq 2 then begin
   dwr=18
   gain=8.1
endif
if letters eq 'rf' and numbers ge 3 and numbers lt 13 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=1.53    ;New tag series for Lick in 2002
endif;
if letters eq 'rf' and numbers eq 13 then begin
   dwr=18
   gain=2.80
endif
if letters eq 'rf' and numbers ge 14 then begin
   dwr=24       ;dewar 8 high resistivity ccd
   gain=8.1    ;New tag series for Lick in 2002
endif;
if letters eq 'rj' then begin
   dwr=103       ;Post-fix HIRES chip
   gain=2.19     
endif
if letters eq 'rc' and numbers ge 89 then begin
   dwr=39
   gain=2.66
endif
if letters eq 'rc' and numbers ge 40 and numbers lt 89 then dwr=8
if letters eq 'rc' and numbers le 39 and numbers ge 22 then dwr=98
if letters eq 'rc' and numbers le 21 then dwr=99
;
if letters eq 'rx' then begin
   dwr=39                         ;Dewar #13, New Hamilton!
   gain=1.33
endif
;
if letters eq 'rz' then begin     ;UCLA, Chris McCarthy, Ben Zuckerman
   dwr=39
   gain=2.66
endif
;
if letters eq 're' then begin     ;April 98 eta Boo campaign
   dwr=18
   gain=2.80
endif
;
if letters eq 'em' then begin
    dwr = 111
    gain = 1.0
endif
if letters eq 'rk' then begin
   dwr=101                        ;Geoff Marcy Keck Hi-Res seismology data
   gain=2.38                      ;Paul guess, get real value!
   if numbers ge 5 then begin
      dwr=102
      gain=4.76                   ;Geoff guess, get real value!
   endif
   if numbers eq 25 then begin
      obs=fix(strmid(tpname,5,3))
      if obs ge 489 and obs le 496 then gain = 2.38 ;HIRES crash night!
   endif
   if numbers eq 39 then begin
      obs=fix(strmid(tpname,5,3))
      if obs ge 347 and obs le 349 then gain = 2.38 ;High Gain Test!
   endif
   if numbers eq 43 then begin
      obs=fix(strmid(tpname,5,3))
      if obs ge 111 and obs le 114 then gain = 2.38 ;High Gain Test!
   endif
endif
;

if letters eq 'rm' then begin
    dwr=104
    gain=0.92
endif
if letters eq 'ru' and numbers eq 2 then begin
   dwr=150
   gain=2.3
endif
if letters eq 'ru' and numbers eq 3 then begin
   dwr=150
   gain=1.1
endif
if letters eq 'ru' and numbers ge 4 and numbers le 16 then begin
   dwr=150
   gain=2.3
endif
if letters eq 'ru' and numbers ge 17 and numbers le 37 then begin
   dwr=150
   gain=1.8
endif
if letters eq 'ru' and numbers ge 38 and numbers le 41 then begin
   dwr=150
   gain=4.8     ;alpha Cen A seismology run, non-astro readout mode 
endif
if letters eq 'ru' and numbers ge 42 and numbers le 43 then begin
   dwr=150
   gain=1.8
endif
if letters eq 'ru' and numbers eq 44 then begin   ;EEV CCD test
   dwr=151
   gain=2.7
endif
if letters eq 'ru' and numbers ge 45 and numbers le 46 then begin ;MITLL2 CCD
   dwr=150
   gain=1.8
endif
if letters eq 'ru' and numbers eq 47 then begin   ;EEV CCD test
   dwr=151
   gain=2.7
   obs=fix(strmid(tpname,5,3))
   if obs le 14 then gain = 1.3 ;readout initially "normal" not "fast"
endif
if letters eq 'ru' and numbers eq 48 then begin   ;EEV CCD
   dwr=151
   gain=2.7
endif
if letters eq 'ru' and numbers eq 49 then begin
   dwr=150
   gain=4.8     ;alpha Cen A seismology run, non-astro readout mode
endif
if letters eq 'ru' and numbers ge 50 then begin   ;EEV CCD
   dwr=151
   gain=2.7
endif
;
telescope = 'SHANE'                          ;Assume Lick 3-m
if letters eq 'rk' or letters eq 'em' then telescope = 'KECK'
if letters eq 'rc' then telescope = 'CAT'
if letters eq 'rc' and numbers eq 91 then telescope = 'SHANE'
if letters eq 'rz' then telescope = 'CAT'    ;Chris McCarthy, UCLA
if letters eq 'ru' then telescope = 'AAT'
if letters eq 're' then telescope = 'CAT'
if letters eq 'rv' then telescope = 'VLT2'
;
if letters eq 'rf' then begin                ;new Lick tag
  if numbers ge 3 and numbers le 4 then telescope = 'CAT'
  if numbers ge 6 and numbers le 7 then telescope = 'CAT'
  if numbers ge 10 and numbers le 14 then telescope = 'CAT'
  if numbers ge 16 then telescope = 'CAT'
endif  
if letters eq 'rd' then begin
  if numbers le 5 then telescope = 'CAT'
  if numbers eq 7 then telescope = 'CAT'
  if numbers eq 8 then telescope = 'CAT'
  if numbers eq 10 then telescope = 'CAT'
  if numbers ge 14 and numbers le 22 then telescope = 'CAT'
  if numbers ge 24 and numbers lt 26 then telescope = 'CAT'
  if numbers ge 27 and numbers lt 31 then telescope = 'CAT'
  if numbers ge 32 and numbers lt 35 then telescope = 'CAT'
  if numbers eq 36 then telescope = 'CAT'
  if numbers eq 38 then telescope = 'CAT'
  if numbers ge 40 and numbers le 41 then telescope = 'CAT'
  if numbers ge 43 and numbers le 44 then telescope = 'CAT'
  if numbers ge 47 and numbers le 48 then telescope = 'CAT'
  if numbers ge 50 and numbers le 53 then telescope = 'CAT'
  if numbers ge 55 and numbers le 58 then telescope = 'CAT'
  if numbers ge 60 and numbers le 73 then telescope = 'CAT'
  if numbers ge 75 and numbers le 77 then telescope = 'CAT'
  if numbers ge 78 and numbers le 80 then telescope = 'CAT'
  if numbers ge 82 and numbers le 84 then telescope = 'CAT'
  if numbers ge 88 and numbers le 90 then telescope = 'CAT'
  if numbers ge 92 then telescope = 'CAT'
endif
if letters eq 'rb' then begin
   if numbers ge  5 and numbers le  9 then telescope = 'CAT'
   if numbers ge 11 and numbers le 13 then telescope = 'CAT'
   if numbers eq 15 then telescope = 'CAT'
   if numbers ge 17 and numbers le 18 then telescope = 'CAT'
   if numbers eq 20 then telescope = 'CAT'
   if numbers ge 22 and numbers le 25 then telescope = 'CAT'
   if numbers eq 27 then telescope = 'CAT'
   if numbers eq 28 then begin
      obs=fix(strmid(tpname,5,3))
      if obs lt 51 then telescope = 'CAT'
      if obs gt 88 then telescope = 'CAT'
   endif
   if numbers eq 29 then telescope = 'CAT'
   if numbers ge 31 and numbers le 32 then telescope = 'CAT'
   if numbers eq 34 then telescope = 'CAT'
   if numbers eq 38 then telescope = 'CAT'
   if numbers ge 40 and numbers le 41 then telescope = 'CAT'
   if numbers ge 43 and numbers le 46 then telescope = 'CAT'
   if numbers ge 55 and numbers le 58 then telescope = 'CAT'
   if numbers eq 69 then telescope = 'CAT'
   if numbers eq 71 then telescope = 'CAT'
   if numbers ge 73 and numbers le 78 then telescope = 'CAT'
   if numbers eq 81 then telescope = 'CAT'
   if numbers eq 83 then telescope = 'CAT'
   if numbers eq 84 then telescope = 'CAT'
   if numbers eq 88 then telescope = 'CAT'
   if numbers eq 89 then telescope = 'CAT'
   if numbers eq 91 then telescope = 'CAT'
   if numbers eq 92 then telescope = 'CAT'
   if numbers eq 93 then telescope = 'CAT'
   if numbers eq 95 then telescope = 'CAT'
   if numbers eq 96 then telescope = 'CAT'
   if numbers gt 97 then telescope = 'CAT'   
endif
;
if letters eq 'rs' then telescope = 'CAT'     ;Assume CAT observation
if letters eq 'rs' then begin                 ;Then check if SHANE
   if numbers eq 11 then telescope = 'SHANE'
   if numbers eq 13 then telescope = 'SHANE'
   if numbers eq 16 then telescope = 'SHANE'
   if numbers eq 21 then telescope = 'SHANE'
   if numbers eq 23 then telescope = 'SHANE'
   if numbers eq 30 then telescope = 'SHANE'
   if numbers eq 42 then telescope = 'SHANE'
endif
;
if letters eq 'rh' then begin
   if numbers ge 25 and numbers le 29 then telescope = 'CAT'
   if numbers ge 39 and numbers le 49 then telescope = 'CAT'
   if numbers ge 54 and numbers le 59 then telescope = 'CAT'
   if numbers ge 69 and numbers le 72 then telescope = 'CAT'
   if numbers ge 74 and numbers le 76 then telescope = 'CAT'
   if numbers ge 81 and numbers le 83 then telescope = 'CAT'
   if numbers ge 86 and numbers le 87 then telescope = 'CAT'
   if numbers eq 98 then telescope = 'CAT'
endif
;
if letters eq 'ra' then begin
   if numbers ge  4 and numbers le  6 then telescope = 'CAT'
   if numbers ge 23 and numbers le 31 then telescope = 'CAT'
   if numbers ge 33 and numbers le 38 then telescope = 'CAT'
   if numbers ge 43 and numbers le 48 then telescope = 'CAT'
   if numbers ge 52 and numbers le 54 then telescope = 'CAT'
   if numbers ge 57 and numbers le 62 then telescope = 'CAT'
   if numbers ge 64 and numbers le 66 then telescope = 'CAT'
   if numbers ge 72 and numbers le 80 then telescope = 'CAT'
   if numbers eq 87 then telescope = 'CAT'
   if numbers ge 90 and numbers le 97 then telescope = 'CAT'
endif

return,dwr
end



