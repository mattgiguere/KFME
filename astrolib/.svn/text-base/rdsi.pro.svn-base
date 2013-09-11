pro rdsi,ob,obnam,filter,fdsk,inpdsk=inpdsk,noob=noob,tellist=tellist

;This code drives the PSF and velocity analysis
;
;ob      (output array)     Observation 
;obname  (input string)     Observation name  i.e. 'rc10.7' or 'ra49.31' or 'rb34.28'
;filter  (output array)     Observation filter
;fdsk    (output string)    files disk name   i.e. '/mir1/files/' or '/d4/cepheid/files/'
;inpdsk  (input keyword string)  input files disk name i.e. '/mir2/paul/raw/'
;
;Created June 7, 1994  R.P.B.
;Updated Feb 25, 1996  R.P.B.
;Updated Nov  9, 1998  R.P.B., AAT/UCLES, inpdsk
;


if n_params () lt 2 then begin
  print,' IDL> rdsi,ob,obnam,filter,fdsk
  return
endif

if n_elements(noob) ne 1 then noob = 0   ;default, noob = 0, return observation
tellist=0    ;initial default

; Initially assume this is being run on the Berkeley system
       fd0='/mir1/files/'                         ;files disk
       fd1='/mir1/paul/exfiles/'                  ;files disk
       fd2='/mir2/files/'                         ;AAT/UCLES files disk
       fd4='/mir4/files/'                         ;VLT/UVES files disk
       fd5='/mir3/files/'                         ;Keck files disk
       fd6='/mir5/files/'                         ;Magellen/MIKE files disk
       fd7='/mir6/files/'                         ;Subaru/HDS
       obd0='/mir1/iodspec/'                      ;observation disk
       obd1='/mir1/paul/cepspec/'                 ;observation disk
       obd2='/mir2/iodspec/'                      ;DTM/UCLES observation disk
       obd4='/mir4/iodspec/'                      ;VLT/UVES observation disk
       obd5='/mir3/iodspec/'                      ;Keck observation disk
       obd6='/mir5/iodspec/'                      ;Magellan/MIKE obs disk
       obd7='/mir6/redspec/'                      ;Subaru/HDS
       fldsk0='/mir1/files/'                      ;filter disk
       fldsk1='/mir1/paul/exfiles/'               ;filter disk


; Directories for DTM
; Check to see if this is being run on the DTM system
       dtmfts='/mir1/paul/atlas/ftsiod.bin'       ;DTM FTS Atlas
       dummy=first_el(findfile(dtmfts))
       if dummy eq dtmfts then begin    ;This is being run on the DTM system
          fd0='/mir1/paul/files/'                 ;files disk
          fd1='/mir1/paul/exfiles/'               ;files disk
          fd2='/mir2/paul/files/'                 ;DTM/UCLES files disk
          fd4='/mir4/paul/files/'                 ;VLT/UVES files disk
          fd5='/mir3/paul/files/'                 ;Keck files disk
          fd6='/mir5/paul/files/'                 ;Magellen/MIKE files disk
          fd7='/mir6/files/'                      ;Subaru/HDS files disk

          obd0='/mir1/paul/iodspec/'              ;observation disk
          obd1='/mir1/paul/cepspec/'              ;observation disk
          obd2='/mir2/paul/iodspec/'              ;DTM/UCLES observation disk
          obd4='/mir4/paul/iodspec/'              ;Keck observation disk
          obd5='/mir3/paul/iodspec/'              ;Keck observation disk
          obd6='/mir5/paul/iodspec/'              ;Magellan/MIKE obs disk
          obd7='/mir6/redspec/'                   ;Subaru/HDS obs disk

          fldsk0='/mir1/paul/files/'              ;filter disk
          fldsk1='/mir1/paul/exfiles/'            ;filter disk
       endif

; Directories for Keck/Waimea
; Check to see if this is being run on the Keck/Waimea system
       keckfts='/s/sdata9/hires/software/gmarcy/login'  ;Keck
       dummy=first_el(findfile(keckfts))
       if dummy eq keckfts then begin    ;This is being run on the Keck system
          fd0='/mir1/paul/files/'                 ;files disk
          fd1='/mir1/paul/exfiles/'               ;files disk
          fd5='/s/sdata9/hires/software/gmarcy/focus/'
                                                  ;keck files disk
          obd0='/mir1/paul/iodspec/'              ;observation disk
          obd1='/mir1/paul/cepspec/'              ;observation disk
          obd5='/s/sdata9/hires/software/gmarcy/iodspec/'
                                                  ;keck observation disk
          fldsk0='/mir1/paul/files/'              ;filter disk
          fldsk1='/mir1/paul/exfiles/'            ;filter disk
       endif

; Directories for AAT/Siding Spring Mountain
; Check to see if this is being run on the Anglo-Australian Telescope system
       aatfts='/home/aatsse/rpb/paul/notes'       ;Anlgo-Australian Telescope
       dummy=first_el(findfile(aatfts))
       if dummy eq aatfts then begin    ;This is being run on the AAT system
;          fd0='/mir1/paul/files/'                 ;files disk
;          fd1='/mir1/paul/exfiles/'               ;files disk
          fd2='/mir2/paul/files/'                  ;AAT/UCLES files disk
          fd4='/mir4/paul/files/'                  ;VLT/UVES files disk
;          fd5='/mir3/paul/files/'                 ;keck files disk
;          obd0='/mir1/paul/iodspec/'              ;observation disk
;          obd1='/mir1/paul/cepspec/'              ;observation disk
          obd2='/data_ssf/obsred/rpb/'             ;AAT/UCLES observation disk
;          obd4='/data_ssf/obsred/rpb/'            ;VLT/UVES observation disk
;          obd5='/mir3/paul/iodspec/'              ;keck observation disk
          fldsk0='/mir1/paul/files/'               ;filter disk
          fldsk1='/mir1/paul/exfiles/'             ;filter disk
       endif

; Directories for SSL
; Check to see if this is being run on the SSL/DENALI system
;       sslfts='/disks/denali/scratch/paulb/atlas/ftseso50.bin'   ;SSL FTS Atlas
;       dummy=first_el(findfile(sslfts))
;       if dummy eq sslfts then begin    ;This is being run on the SSL system
;          fd0='/disks/denali/scratch/paulb/files/'     ;files disk
;          fd1='/disks/denali/scratch/paulb/cfiles/'    ;files disk
;          obd0='/disks/denali/scratch/paulb/iodspec/'  ;observation disk
;          obd1='/disks/denali/scratch/paulb/cepspec/'  ;observation disk
;          fldsk0='/disks/denali/scratch/paulb/files/'  ;filter disk
;          fldsk1='/disks/denali/scratch/paulb/cfiles/' ;filter disk
;       endif

;which disk is which?
          tp = strmid(obnam,0,2)  ;tape series (i.e. ra,rb,rc,rd,rh,rk,rz,ru,re,rv,rm,rp,rj,rg)
          nb = strmid(obnam,2,2)  ;first two digits of tape number
          oo = strpos(obnam,'.')
          oo=fix(strmid(obnam,oo+1,4))  ;observation number
	  dwr = chip(obnam,gain)
          obdsk=obd0                    ;observation directory
          if tp eq 'rh' then fdsk=fd0   ;files directory
          if tp eq 'ra' then fdsk=fd0
          if tp eq 'rb' then fdsk=fd0
          if tp eq 'rd' then fdsk=fd0
          if tp eq 'rf' then fdsk=fd0
          if tp eq 'rg' then fdsk=fd0
          if tp eq 'rp' then fdsk='/mir6/files'
          if tp eq 'rj' then fdsk='/mir3/files'
 	  if tp eq 'rs' then fdsk=fd0   ;Fischer, SIM project 
          if tp eq 'rz' then fdsk=fd0   ;Chris McCarthy, UCLA
          if tp eq 'rx' then fdsk=fd0   ;New Hamilton, Big Chip
          if tp eq 're' then fdsk=fd0   ;eta Boo campaign, April/May 1998
          if tp eq 'rc' then begin
	     fdsk=fd1                   ;files directory
             obdsk=obd1                 ;observation directory
         endif
 ;Lick telluric filter ??
           if tp eq 'rh' or tp eq 'ra' or tp eq 'rb' or tp eq 'rd' or $
              tp eq 'rf' or tp eq 'rs' or tp eq 'rz' or tp eq 'rx' or $
              tp eq 're' or tp eq 'rc' then $
               rascii,tellist,2,fd0+'telluric_wav.ascii'

          if tp eq 'rk' then begin      ;Keck HIRES, 2048 chip
             fdsk=fd5                   ;files directory
             obdsk=obd5                 ;observation directory
              rascii,tellist,2,fdsk+'telluric_wav.ascii' ;Keck telluric lines
          endif
          if tp eq 'rj' then begin      ;Keck HIRES, 2048 chip
             fdsk=fd5                   ;files directory
             obdsk=obd5                 ;observation directory
              rascii,tellist,2,fdsk+'telluric_wav.ascii' ;Keck telluric lines
          endif
          if tp eq 'ru' then begin      ;AAT UCLES, 2496 chip
             fdsk=fd2                   ;files directory
             obdsk=obd2                 ;observation directory
            rascii,tellist,2,fdsk+'telluric_wav_ru16.ascii' ;AAT telluric                  
          endif
          if tp eq 'rm' then begin      ;MIKE, 4096 chip                                     
             fdsk=fd6                   ;files directory                                     
             obdsk=obd6 ;observation directory                               
;            tellist=0                  ;need to construct Las Campans
             rascii,tellist,2,fdsk+'telluric_wav.ascii' ;MIKE telluric lines                 
         endif                                                                              
          if tp eq 'rp' then begin      ;HDS, 4096 chip                                      
             fdsk=fd7                   ;files directory                                     
             obdsk=obd7                 ;observation directory                               
             rascii,tellist,2,fdsk+'telluric_wav.ascii' ;HDS telluric lines                  
         endif                                                                              
          obname=obdsk+obnam            ;observation name including directory        
          if n_elements(inpdsk) eq 1 then obname=inpdsk+obnam $
	    else obname=obdsk+obnam    ;observation name including directory
;special Keck instructions for Steve Vogt's "rk1" 4-interleaved run 
          if tp eq 'rk' and nb eq '1.' then obname=strmid(obname,0,strlen(obname)-2)
          if noob eq 1 then GOTO,NOOBEND                 
 	  rdsk,ob,obname,1               ;get the observation from disk

	  fixpix,ob,dewar=dwr            ;  smooth over bad pixels

    if n_elements(gain) eq 1 then ob=ob*gain(0) $    ;Convert from DN to photons     
              else begin                   ;deal with UVES "rv" data from 2 CCDs             
                print,'Check GAIN  for VLT2/UVES in rdsi.pro' ;9 July 2002 RPB               
                ob(*,0:17)=ob(*,0:17)*gain(0)                                                
                ob(*,18:*)=ob(*,18:*)*gain(1)                                                
            endelse                                                                        
            nanfilt=ob*0+1                                                                   
            nanpix=wherenan(ob,nnan)       ;check for NaN values  15Jan2002  RPB             
            if nnan gt 0 then begin        ;any NaN values?                                  
              for qq=0,nnan-1 do begin                                                       
                ord=nanpix(qq)/n_elements(ob(*,0))                                           
                pix=nanpix(qq)-(ord * n_elements(ob(*,0)))                                   
                print,'NaN value at  Order: '+strtrim(ord,2)+'  Pixel: '+strtrim(pix,2)      
                nanfilt(pix,ord)=0                                                           
                if pix gt 2 and pix lt (n_elements(ob(*,0))-3) then badpix,ob,ord,pix,1      
            endfor                                                                         
        endif                                                                            
;tweak UCLES obsevations so they all have the same 0-th order
          if tp eq 'ru' then if fix(nb) eq 2 then ob=ob(*,1:*) 
          if tp eq 'ru' then if fix(nb) eq 3 then ob=ob(*,2:*) 
          if tp eq 'ru' then if fix(nb) eq 5 then $
                 if oo gt 80 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 7 then $
                 if oo gt 70 then ob=ob(*,2:*)
          if tp eq 'ru' then if fix(nb) ge 9 then $
                 if fix(nb) le 13 then ob=ob(*,2:*) 
          if tp eq 'ru' then if fix(nb) eq 14 then ob=ob(*,1:*) 
          if tp eq 'ru' then if fix(nb) eq 15 then ob=ob(*,3:*) 
          if (tp eq 'ru') and (fix(nb) eq 16) then ob=ob(*,2:*)  ;PB 17 Aug 01
	  if (tp eq 'ru') and (fix(nb) eq 16) and $  
	     (oo ge 23) and (oo le 24) then begin  ;why only observation 23/24?
	     badpix,ob,14,2115,6
             filter(2114:2121,14)=0.
	  endif
          if tp eq 'ru' then if fix(nb) eq 18 then ob=ob(*,3:*)
	  if tp eq 'ru' then if fix(nb) eq 19 then ob=ob(*,2:*)
	  if tp eq 'ru' then if fix(nb) eq 20 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 21 then ob=ob(*,2:*)
	  if tp eq 'ru' then if fix(nb) ge 22 then $
	         if fix(nb) le 27 then ob=ob(*,2:*)
          if tp eq 'ru' then if fix(nb) eq 28 then ob=ob(*,3:*)
	  if tp eq 'ru' then if fix(nb) eq 29 then $
	         if fix(oo) lt 115 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 29 then $
	         if fix(oo) gt 115 then if fix(oo) lt 200 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 29 then $
	         if fix(oo) gt 200 then if fix(oo) lt 250 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 29 then $
	         if fix(oo) gt 250 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) ge 30 then $
          if fix(nb) le 37 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) ge 38 and fix(nb) le 41 then begin ; alpha Cen A
	         ob=reverse(ob)        ;rotated format for alpha Cen A
	         bork=fltarr(2047,31)
	         bork(*,3:30)=ob       ;offset orders to match all other observations
	         ob=bork
	  endif
          if tp eq 'ru' then if fix(nb) eq 42 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 43 then $
                 if fix(oo) lt 100 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 43 then $
                 if fix(oo) ge 100 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 44 then ob=rotate(ob,2)  ;EEV
          if tp eq 'ru' then if fix(nb) eq 44 then ob=ob(*,1:*)     ;EEV
          if tp eq 'ru' then if fix(nb) eq 45 then $
                 if fix(oo) lt 180 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 45 then $
                 if fix(oo) gt 180 then if fix(oo) lt 220 then ob=ob(*,3:*)
          if tp eq 'ru' then if fix(nb) eq 45 then $
                 if fix(oo) gt 220 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 46 then $
                 if fix(oo) lt 70 then ob=ob(*,1:*)
          if tp eq 'ru' then if fix(nb) eq 46 then $
                 if fix(oo) ge 70 then ob=ob(*,3:*)
          if tp eq 'ru' then if (fix(nb) ge 47) and (fix(nb) le 48) $
		 then ob=rotate(ob,2)   ;EEV
          if tp eq 'ru' then if (fix(nb) ge 47) and (fix(nb) le 48) $
		 then ob=ob(*,1:*)      ;EEV
          if tp eq 'ru' then if fix(nb) eq 49 then begin ;alpha Cen A seismology
                 ob=reverse(ob)   ;this is continuous with ru38-ru41
                 bork=fltarr(2047,31)
                 bork(*,3:30)=ob  ;offset orders to match all other observations
                 ob=bork
          endif
          if tp eq 'ru' then if fix(nb) ge 50 then ob=rotate(ob,2)  ;EEV
          if tp eq 'ru' then if fix(nb) ge 50 then ob=ob(*,1:*)     ;EEV

;Kludge for 3rd night of rf01 run (Lick)    DAF, RPB   25Jun02
          if tp eq 'rf' then if fix(nb) eq 1 then $
             if (oo ge 115) then if (oo le 137) then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(0:1849,*)=ob
	     dork(1850,*)=dork(1848,*)
	     ob=dork
          endif

;Kludge for rf14 run (Lick)    DAF, RPB   16JSep02
          if tp eq 'rf' then if fix(nb) eq 14 then begin 
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
          endif

;Kludge for fourth night of rf21 run (Lick)    DAF, RPB   16JSep02
          if tp eq 'rf' then if fix(nb) eq 21 then if oo ge 179 then begin 
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
          endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 23 then if oo le 74 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 123 then if oo le 251 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 295 then if oo le 458 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 495 then if oo le 666 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 694 then if oo le 928 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif

;Kludge for Jason's moon run
	if tp eq 'rf' then if fix(nb) eq 19 then $
	  if oo ge 968 then if oo le 1278 then begin
	   dork=fltarr(1851,n_elements(ob(0,*)))
	   dork(*,3:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-4)
	   dork(*,0)=ob(*,0)
	   dork(*,1)=ob(*,0)
	   dork(*,2)=ob(*,0)
	   ob=dork
	endif


;Kludge for rf60 run   (Halpha in order 28)
	if tp eq 'rf' then if fix(nb) eq 60 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif

;Kludge for rf63 run 
	if tp eq 'rf' then if fix(nb) eq 63 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
	endif

;Kludge for rf64 run   (Halpha in order 26)
	if tp eq 'rf' then if fix(nb) eq 64 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
	endif

;Kludge for rf65 run   (Halpha in order 26)
	if tp eq 'rf' then if fix(nb) eq 65 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
	endif


;Kludge for rf66,logsheet4   (Halpha in order 26)
;	if tp eq 'rf' then if fix(nb) eq 66 then $
;	  if oo ge 80 then if oo le 190 then begin
;	     dork=fltarr(1851,n_elements(ob(0,*)))
;	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
;	     dork(*,0)=ob(*,0)
;	     ob=dork
;	endif

;Kludge for rf66,logsheet4   (Halpha in order 26)
;	if tp eq 'rf' then if fix(nb) eq 66 then $
;	  if oo ge 258 then if oo le 288 then begin
;	     dork=fltarr(1851,n_elements(ob(0,*)))
;	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
;	     dork(*,0)=ob(*,0)
;	     ob=dork
;	endif


;Kludge for rf68,logsheet1   (Halpha in order 26)
	if tp eq 'rf' then if fix(nb) eq 68 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
	endif

;Kludge for rf90 run   (Halpha in order 26)
;	if tp eq 'rf' then if fix(nb) eq 90 then begin
;  	if oo lt 90 then begin
;	     dork=fltarr(1851,n_elements(ob(0,*)))
;	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
;	     dork(*,0)=ob(*,0)
;	     ob=dork
;        endif
;	endif

;Kludge for rf92 run   (Halpha in order 26)
	if tp eq 'rf' then if fix(nb) eq 92 then begin
  	if oo lt 170 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
         endif
 	 endif

;Kludge for rf93 run   (Halpha in order 26)
	if tp eq 'rf' then if fix(nb) eq 93 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
	     dork(*,1:n_elements(ob(0,*))-1)=ob(*,0:n_elements(ob(0,*))-2)
	     dork(*,0)=ob(*,0)
	     ob=dork
 	 endif

;Kludge for rf98 run   (Halpha in order 28)
	if tp eq 'rf' then if fix(nb) eq 98 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif

;Kludge for rf99 run   (Halpha in order 28)
	if tp eq 'rf' then if fix(nb) eq 99 then begin
  	if oo lt 50 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rf99 run   (Halpha in order 28)
	if tp eq 'rf' then if fix(nb) eq 99 then begin
  	if oo gt 90 and oo lt 140 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rg01 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 1 then begin
	if oo gt 50 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rg02 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 2 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif

;Kludge for rg03 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 3 then begin
	if oo lt 50 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rg03 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 3 then begin
	if oo gt 120 and oo lt 155 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rg03 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 3 then begin
	if oo gt 230 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif

;Kludge for rg06 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 6 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif

;Kludge for rg41 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 41 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for rg50 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 50 then begin
	if oo lt 600 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif
	endif

;Kludge for rg52 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 52 then begin
	if oo lt 170 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif
	endif


;Kludge for rg53 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 53 then begin
	if oo gt 430 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif
     endif

;Kludge for rg53 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 54 then begin
	if oo gt 490 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif
	endif

;Kludge for rg55 run   (Halpha in order 28  nights: 1,2,5)
	if tp eq 'rg' then if fix(nb) eq 55 then begin
	if oo lt 399  or oo gt 664 then begin  ; First 3 nights shifted
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
    endif

;Kludge for rg57 run   (Halpha in order 28  nights: 1)
	if tp eq 'rg' then if fix(nb) eq 57 then begin
	if oo lt 500 then begin  ; First 3 nights shifted
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
        endif
	endif

;Kludge for re16 run   (Halpha in order 28)
	if tp eq 're' then if fix(nb) eq 16 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for re17 run   (Halpha in order 28)
	if tp eq 're' then if fix(nb) eq 17 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for re18 run   (Halpha in order 28)
	if tp eq 're' then if fix(nb) eq 18 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for re19 run   (Halpha in order 28)
	if tp eq 're' then if fix(nb) eq 19 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for re23 run   (Halpha in order 28)
	if tp eq 're' then if fix(nb) eq 23 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
         endif

;Kludge for rg46 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 46 then begin
	if oo gt 325 and oo lt 365 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif
	endif
;Kludge for rg60 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 60 then begin
;	if oo gt 325 and oo lt 365 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
;	endif
         endif

;Kludge for rg65 run   (Halpha in order 28)
	if tp eq 'rg' then if fix(nb) eq 65 then begin
	     dork=fltarr(1851,n_elements(ob(0,*)))
             lstord=n_elements(ob(0,*))
	     dork(*,0:n_elements(ob(0,*))-2)=ob(*,1:n_elements(ob(0,*))-1)
	     dork(*,lstord-1)=ob(*,lstord-1)
	     ob=dork
	endif

         
;Kludge for rm (MIKE)    RPB   27Apr04                                                       
          if tp eq 'rm' then if fix(nb) eq 1 then begin                                      
             bork=fltarr(n_elements(ob(*,0)),n_elements(ob(0,*))+1)                          
             bork(*,1:*)=ob                                                                  
             ob=bork                                                                         
         endif                                                                              
          if tp eq 'rm' then if fix(nb) eq 2 then begin                                      
             bork=fltarr(n_elements(ob(*,0)),n_elements(ob(0,*))+1)                          
             bork(*,1:*)=ob                                                                  
             ob=bork                                                                         
         endif                                                                              
          if tp eq 'rm' then if fix(nb) eq 3 then begin                                      
             bork=fltarr(n_elements(ob(*,0)),n_elements(ob(0,*))+1)                          
             bork(*,1:*)=ob                                                                  
             ob=bork                                                                         
         endif                                                                              
          if tp eq 'rm' then if fix(nb) eq 4 then begin                                      
             bork=fltarr(n_elements(ob(*,0)),n_elements(ob(0,*))+1)                          
             bork(*,1:*)=ob                                                                  
             ob=bork                                                                         
         endif                                               
          if tp eq 'rm' then if fix(nb) eq 5 then ob=ob(*,1:*)                               
          if tp eq 'rm' then if fix(nb) eq 6 then ob=ob(*,1:*)                               
          if tp eq 'rm' then if fix(nb) eq 7 then ob=ob(*,1:*)                               
          if tp eq 'rm' then if fix(nb) eq 8 then ob=ob(*,1:*)                               
          if tp eq 'rm' then if fix(nb) eq 9 then ob=ob(*,1:*)                               
          if tp eq 'rm' then if fix(nb) eq 10 then ob=ob(*,1:*)                              
          if tp eq 'rm' then if fix(nb) eq 12 then ob=ob(*,1:*)                              
          if tp eq 'rm' then if fix(nb) ge 14 then ob=ob(*,1:*)    

;get proper filter
	  if dwr eq 1 then rdsk,filter,fldsk0+'filt1.dsk'
	  if dwr eq 2 then rdsk,filter,fldsk0+'filt2.dsk'
	  if dwr eq 6 then rdsk,filter,fldsk0+'filt6.dsk'
	  if dwr eq 8 then begin
	     rdsk,filter,fldsk0+'filt8.dsk'
	     filter(553:554,*)=0  &  filter(559,*)=0   ;PB Kludge  5/25/93
          endif
	  if dwr eq 13 then rdsk,filter,fldsk0+'filt13.dsk' ;Old Dewar #13
	  if dwr eq 39 then begin           ;The New Dewar #13, thick chip
	     filter=ob*0.+1.                ;Used from rb02 through rb68
	     filter(1341,24:51)=-1          ;only CCD flaw?
          endif
	  if dwr eq 24 then begin           ;The New Dewar #8, thick chip
	     filter=ob*0.+1.                ;High Resistivity CCD
	   ;  filter(1341,24:51)=-1          ;only CCD flaw?
          endif          

   if dwr eq 18 then rdsk,filter,fldsk0+'filt18.dsk' ;New Dewar #6, rb69-?, re
   if dwr eq 103 then filter=ob*0.+1.        ;New HIRES CCD, August 2004      
   if dwr eq 150 then filter=ob*0.+1.       ;UCLES MIT/LL, Perfect?
   if dwr eq 151 then filter=ob*0.+1.       ;UCLES MIT/LL, Perfect?
   if dwr eq 161 then filter=ob*0.+1.       ;UCLES MIT/LL, Perfect?
   if dwr eq 161 then filter(0:300,*)=0.     ;VLT UVES, Toss far ends, no DN                 
   if dwr eq 161 then filter(4050:*,*)=0.    ;VLT UVES, Toss far ends, no DN                 
   if dwr eq 191 then filter=ob*0.+1.       ;Magellan, Perfect?
   if dwr eq 171 then rdsk,filter,fd7+'filt.dsk' ;HDS      
	  if dwr eq 98 then begin
	     rdsk,filter,fldsk1+'filt_cep33.dsk'
; No longer trim observation to "look" like pre-fix,  Feb 25, 1996
;	     ob=ob(*,6:30)                         ;orders same as rh, ra
;	     filter=filter(*,6:30)
          endif
	  if dwr eq 99 then begin
	     rdsk,filter,fldsk1+'filt_cep52.dsk'
; No longer trim observation to "look" like pre-fix,  Feb 25, 1996
;	     ob=ob(*,25:49)                        ;orders same as rh, ra
;	     filter=filter(*,25:49)
         endif
   if dwr eq 102 then rdsk,filter,fdsk+'filt_rk5.dsk' ;Keck  rk5 ->       
	  if tp eq 'rk' then begin              ;Keck set up 
;	     filter=ob*0+1                      ;perfect chip?

	     rdsk,filter,fdsk+'filt_rk5.dsk'    ;GWM Oct 96 filter (rk5 and on)
	     filter(85:86,*)=0                  ;PB Kludge, 6Jun02 from JTW
	     filter(1127:1128,*)=0              ;PB Kludge, 6Jun02 from JTW
;special Keck instructions for Steve Vogt's "rk1" 4-interleaved run 
             if nb eq '1.' then begin
;get discontinuity fix file   ** not needed with new G. Marcy reduced data
;	       rascii,hrfix,2,'/mir1/paul/seismo/files/hires_fix.ascii',skip=1
;	       dum=fltarr(7,n_elements(hrfix(0,*)))
;	       dum(0,*)=hrfix(0,*)  &  dum(6,*)=hrfix(1,*)
;	       mixpix,ob,filter,dum,/fixmarr
;there are four interleaved exposures, need to pull them apart
	       interleaf=fix(strmid(obnam,strlen(obnam)-1,1))
	       ob=ob(*,indgen(11)*4+3-interleaf)
	       filter=filter(*,indgen(11)*4+3-interleaf)
;Stupid Kludge   PB 4/7/95
	       dumob=fltarr(2048,12)*0.
	       dumob(*,1:11)=ob
	       ob=dumob
	       dumob(*,1:11)=filter
	       filter=dumob
	     endif
          endif
	  prop_filt,filter,/zero        ;"proper filter" of 0's and 1's
;finished get proper filter

NOOBEND:    ;  returning information without actual spectra or filter               
return
end
