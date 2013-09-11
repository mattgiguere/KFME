pro modecha, file_or_struct, sig=sig, cont=cont, wave=wave, orders=orders $
          , zapind=zapind, zapval=zapval, resolut=resolut, gain=gain $
          , barycorr=barycorr, radvel=radvel, _EXTRA=extra $
          , process=process, syntax=syntax
;Modifies echelle spectrum data in a ".ech" file or in an IDL structure.
;  The two cases are distinguished by the nature of the first argument.
;  Calling this routine with no keyword arguments is a good way to check
;  structure syntax and/or convert irregular data structures to the current
;  format.
;
;Input/Output:
; FILE_OR_STRUCT (string) or (structure) specifies the echelle spectrum data
;   to be modified. The two cases are:
;     <1> FILE_OR_STRUCT is a string containing the root name of a ".ech"
;         disk file; an optional ".ech" extension may be included at the
;         end of the root name; the specified disk file will be read and
;         may subsequently be **overwritten** with altered contents
;     <2> FILE_OR_STRUCT is an IDL structure containing at least a FITS
;         header and a spectrum:
;              HEAD            STRING    Array[ncard]
;              SPEC            FLOAT     Array[ncol, nord]
;         additional tags are allowed; the data associated with certain
;         tags may be validated and/or **modified** by this routine; new
;         tags may be created; unrecognized tags are allowed and will be
;         propagated to the modified return structure.
;
;Inputs:
; [SIG=] (array(ncol,nord)) OR (vector(ncol)) uncertainty in spectrum for one
;   or more grating orders; units are the same as in SPEC; size must match
;   size of SPEC; uncertainty data will be written to the FITS file only if
;   this optional keyword is specified. specifying SIG=0 is equivalent to
;   calling this routine without the SIG= keyword.
; [CONT=] (array(ncol,nord)) OR (array(ndeg+1,nord)) continuum values for
;   SPEC, specified in one of 2 ways:
;     <1> continuum value given explicitly at each pixel;
;     <2> for each order, polynomial coefficients that can be used to
;         compute the continuum; the polynomial argument must be column
;         number (beginning with index 0) **divided by 1000** to improve
;         numerical precision; polynomial degree must be 20 or less to
;         allow the reading routine to decide which format is being used;
;   units are the same as in SPEC; continuum data is written to the FITS
;   file only if this optional keyword specified; saving coefficients is
;   preferable, since less disk space is required. specifying CONT=0 is
;   equivalent to calling this routine without the CONT= keyword.
; [WAVE=] (array(ncol,nord)) OR (array(ndeg+1,nord)) OR vector(ncoef)
;   wavelengths for SPEC, specified in one of 3 ways:
;     <1> wavelengths given explicitly for each pixel;
;     <2> for each order, polynomial coefficients that can be used to
;         compute wavelengths; the polynomial argument must be column
;         number (beginning with index 0) **divided by 1000** to improve
;         numerical precision; polynomial degree must be 20 or less to
;         allow the reading routine to decide which format is being used;
;     <3> global 2-dimension wavelength fit coefficients from mkwave.pro;
;   units must be Angstroms in the observatory (observed) reference frame,
;   otherwise the reading routine will improperly apply velocity corrections;
;   wavelength data is written to the FITS file only if this optional keyword
;   is specified; saving coefficients is preferable, since less disk space
;   is required. specifying WAVE=0 is equivalent to calling this routine
;   without the WAVE= keyword.
; [ORDERS=] (vector(nord)) or (scalar) grating orders contained in SPEC,
;   specified in one of 2 ways:
;     <1> list of orders given explicitly for each order contained in SPEC;
;         orders need not be consecutive or monotonic; the number of orders
;         in the list must match the number expected from the size of SPEC;
;         the order list will be written to disk as a FITS binary table
;         extension; the order list will not recorded in the FITS header,
;         nor will the initial order be recorded in an OBASE header card;
;         if an OBASE card exists in HEAD, it will **not** be written to
;         the FITS header on disk;
;     <2> order number for the first order (which we call the order base,
;         OBASE); if SPEC contains more than one order, orders in SPEC are
;         assumed to **increase** sequentially beginning with OBASE; OBASE
;         will be written to the FITS header on disk, using the tag name
;         "OBASE"; if such a card already exists, the value will be
;         overwritten; OBASE will not be written as a FITS binary table
;         extension;
;   by convention, order numbers are absolute grating orders, but this is not
;   enforced; order specifications are written to the FITS file only if this
;   optional keyword is specified; specifying ORDERS=0 is equivalent to
;   calling this routine without the ORDERS= keyword; the OBASE formalism is
;   preferred for spectra with sequential, increasing order numbers, since
;   the information can be compactly stored in the FITS header.
; [ZAPIND=] (vector(nzap)) list of **1-dimensional** indices giving the
;   pixels in SPEC that are "bad"; note that even if SPEC is 2-dimensional,
;   it can be indexed as a vector using the 1-dimensional indices in ZAPIND;
;   indices in ZAPIND must be consistent with the size of SPEC; if ZAPIND is
;   specified, then ZAPVAL must also be specified and both vectors must have
;   the same length; specifying ZAPIND=0 is equivalent to calling this routine
;   without the ZAPIND= keyword.
; [ZAPVAL=] (vector(nzap)) list of replacement values for bad pixels specified
;   by ZAPIND; essentially, the operation SPEC(ZAPIND) = ZAPVAL will be
;   performed by the spectrum reading routine; note that the reading routine
;   will also set SIG(ZAPIND) to a flag value (zero by default); if ZAPVAL
;   is specified, then ZAPIND must also be specified and both vectors must
;   have the same length; specifying ZAPVAL=0 is equivalent to calling this
;   routine without the ZAPVAL= keyword.
; [RESOLUT=] (scalar) dimensionless resolution (wavelength/FWHM) of the
;   spectrum; resolution is written to the FITS header on disk, using the
;   tag name "RESOLUT"; if such a card already exists, the value will be
;   overwritten; specifying RESOLUT=0 is equivalent to calling this routine
;   without the RESOLUT= keyword.
; [GAIN=] (scalar) gain of the readout amplifier (electrons/ADU) that is to
;   be used in converting SPEC, SIG, and CONT from analog-to-digital units
;   (ADU) to electrons; GAIN is written to the FITS header on disk, using
;   the tag name "GAIN"; if such a card already exists, the value will be
;   overwritten; specifying GAIN=0 is equivalent to calling this routine
;   without the GAIN= keyword.
; [BARYCORR=] (scalar) or (string 'calc') or (string 'none') barycentric
;   velocity correction (km/s) that is to be applied to the observatory frame
;   wavelength scale in WAVE; as a special case, specifying BARYCORR='calc'
;   will cause this routine to use baryhead.pro to calculate BARYCORR from
;   information in the FITS header (this may fail with an error message, if
;   necessary information is missing from the header); unlike the convention
;   for stellar radial velocities, BARYCORR is **negative** when the motion
;   of the earth around the Sun gives an extra component of radial velocity
;   **away** from the source; this is the convention used by bary.pro; to
;   shift the observatory frame wavelength scale to the barycentric frame,
;   apply the tranformation:
;     BARYWAVE = OBSWAVE * (1 + BARYCORR/2.9979246d5)
;   by default this correction is applied automatically by the spectrum
;   reading routine; the velocity correction is written to the FITS header
;   on disk, using the tag name "BARYCORR"; if such a card already exists,
;   the value will be overwritten; specifying BARYCORR='none' is equivalent
;   to calling this routine without the BARYCORR= keyword.
; [RADVEL=] (scalar) or (string 'none') radial velocity (km/s) of the source
;   represented by SPEC; using the standard convention, RADVEL is **positive**
;   when the motion of the source gives an extra component of radial velocity
;   **away** from the source; to shift the barycentric frame wavelength scale
;   to the reference frame at the source, apply the tranformation:
;     WAVE = BARYWAVE * (1 - RADVEL/2.9979246d5)
;   the radial velocity is written to the FITS header on disk, using the
;   tag name "RADVEL"; if such a card already exists, the value will be
;   overwritten; specifying RADVEL='none' is equivalent to calling this
;   routine without the BARYCORR= keyword
; [EXTRA] (undefined) or (structure) list of keyword arguments that were
;   *not* explicitly defined by name in the procedure declaration above. To
;   simplify code maintenance, parsing of spectrum data is done exclusively
;   in this routine. Routines (e.g. wdech.pro) may pass both valid and
;   invalid arguments via IDL's "_EXTRA=" mechanism, subverting IDL's
;   normal keyword validation logic. This routine **must** check for
;   unexpected keyword arguments in EXTRA. If EXTRA is undefined, then all
;   arguments were valid. Otherwise, EXTRA is a structure where the tag
;   names are invalid keywords and the associated data are the arguments.
;   Note that arguments inherited from the use of "_EXTRA" in the calling
;   routine **cannot** be modified by operations in this procedure, even
;   if argument is an explicitly named keyword in the definition of this
;   procedure.
; [/EXPAND] (switch) causes fit coefficients for WAVE and CONT to be
;   expanded into explicit values for each pixel; this option is used by
;   rdech.pro so that all information about compressed data formats can
;   be localized in this one routine.
; [SYNTAX=] (string) when defined, this keyword bypasses normal operation,
;   merely returning a customized syntax description for the calling routine;
;   the value of the keyword should contain the name of the calling routine
;   with all positional argument; this routine will append a list of allowed
;   keywords; see below for more information about why this is done.
;
;Output:
; Creates a FITS file with binary table extensions, containing all the
;   specified spectrum information.
;
;See Also:
; rdech, wdech, wdech_raw
;
;History:
; 27-Jul-1998 Valenti  Initial coding.
; put in loop for up through 500 spectra - mas 8/8/01 

;The syntax logic below allows external routines (e.g. wdech.pro) that pass
; keyword arguments via the "_EXTRA=" mechanism to specify their required
; positional arguments in the SYNTAX= argument and have this routine give
; the list of allowed keyword arguments. The hope is that external routines
; will not need to be modified at all, even as the data structures defined
; herein evolve.
if n_params() lt 1 then begin
  if not keyword_set(syntax) then syntax = 'modecha, file_or_struct'
  print, 'syntax: ' + syntax + ' [,sig= ,cont= ,wave=, orders= ]'
  print, '        [,zapind= ,zapval= ,resolut= ,gain= ,barycorr=, radvel=]'
  print, '        [,/expand ,syntax=]'
  return
end

;Warn of invalid keywords, but proceed anyway.
  if keyword_set(extra) then begin
    message, /cont, 'warning - the following keyword arguments were ignored:'
    help, /str, extra
  endif
;
s = ' '
trial=' '
sarray=strarr(500) 
k = 0
nspc = 1     ; number of spectra to be worked on  
filelist = 0 ; nonzero only if we read in a list of spectral files  
;  
;If we are dealing with a disk file, then read data from disk.
arg1sz = size(file_or_struct)				;variable info
; 
; test for list of input files by reading of first line of file_or_struct:  
if(arg1sz(1) eq 7) then begin  
get_lun,un
openr,un,file_or_struct 
readf,un,trial 
free_lun,un 
trial=string(trial) 
if(strpos(trial,'SIMPLE',0) ne 0) then filelist=1     ;  end test
;  
if(filelist eq 1) then begin      ; test for list of spectrum files 
    get_lun,un
    openr,un,file_or_struct 
    while not eof(un) do begin
      readf,un,s 
      sarray(k) = s 
      k = k+1
    endwhile
    free_lun,un
    nspc = k 
    sarray=sarray(0:nspc-1)  
  endif 
endif 
;  
for k=0,nspc-1 do begin  
 if(filelist eq 1) then file_or_struct = sarray(k) ; otherwise keep old name  
;  
;Internal parameters.
  echvers = 1.01					;version number
  case arg1sz(arg1sz(0)+1) of				;string or structure
    7: begin
         iext = strpos(file_or_struct, '.ech')		;look for extension
         if iext lt 0 then begin			;true: no extension
           fullfile = file_or_struct + '.ech'		;append extension
         endif else begin				;else: has extension
           fullfile = file_or_struct			;use as is
         endelse
         rdech, ech, fullfile, /raw			;string - read file
         end
    8: ech = file_or_struct				;structure - make copy
    else: begin
      message, /cont, 'first argument must be a filename or structure'
      return
      end
  endcase

;Extract FITS header from the data structure.
  if not tag_exist(ech, 'HEAD') then begin		;true: no header
    message, /cont, 'no FITS header in data structure - aborting'
    return
  endif
  head = ech.head					;extract header

;Determine dimensions of spectrum in data structure.
  if not tag_exist(ech, 'SPEC') then begin		;true: no spectrum
    message, /cont, 'no spectrum in data structure - aborting'
    return
  endif
  spec = ech.spec					;extract spectrum
  sz = size(ech.spec)					;variable info block
  ncol = sz(1)						;number of columns
  if sz(0) eq 1 then nord = 1 else nord = sz(2)		;number of orders

;Validate SIG data.
  if not keyword_set(sig) $				;no new value,
     and tag_exist(ech, 'SIG') then sig = ech.sig	; but have old value
  if keyword_set(sig) then begin
    sz = size(sig)					;variable info block
    if sz(1) ne ncol then begin				;true: wrong # of cols
      message, /cont, 'SIG has the wrong number of columns - aborting'
      return
    endif
    if sz(0) eq 1 then n = 1 else n = sz(2)		;number of orders
    if n ne nord then begin				;true: wrong # of ords
      message, /cont, 'SIG has the wrong number of orders - aborting'
      return
    endif
  endif

;Validate CONT data.
  ctype = 0						;clear cont type flag
  if not keyword_set(cont) $				;no new value,
     and tag_exist(ech, 'CONT') then cont = ech.cont	; but have old value
  if keyword_set(cont) then begin
    sz = size(cont)					;variable info block
    if sz(1) gt 20 and sz(1) ne ncol then begin		;true: wrong size
      message, /cont, 'CONT has a bad size (first dimension) - aborting'
      return
    endif
    if sz(0) eq 1 then n = 1 else n = sz(2)		;number of orders
    if n ne nord then begin				;true: wrong # of ords
      message, /cont, 'CONT has the wrong number of orders - aborting'
      return
    endif
    if sz(1) le 20 then ctype = 1			;set cont type flag
  endif

;Validate WAVE data.
  if not keyword_set(wave) $				;no new value,
     and tag_exist(ech, 'WAVE') then wave = ech.wave	; but have old value
  wtype = 0						;clear wave type flag
  if keyword_set(wave) then begin
    sz = size(wave)					;variable info block
    if sz(0) eq 1 then n = 1 else n = sz(2)		;number of orders
    if n eq 1 and nord gt 1 then begin			;true: 2-dim fit
      if wave(1) ne ncol then begin			;true: wrong # cols
        message, /cont, 'WAVE coefficient inconsistency (ncol) - aborting'
        return
      endif
      if wave(2) ne nord then begin			;true: wrong # ords
        wave(2) = nord					;fix # of orders
        message, /cont, 'warning - modified number of expected orders in WAVE'
        return
      endif
      wtype = 2					;set flag, 2-dim fit
    endif else begin					;else: not a 2-dim fit
      if sz(1) gt 20 and sz(1) ne ncol then begin	;true: wrong size
        message, /cont, 'WAVE has a bad size (first dimension) - aborting'
        return
      endif
      if n ne nord then begin				;true: wrong # of ords
        message, /cont, 'WAVE has the wrong number of orders - aborting'
        return
      endif
      if sz(1) le 20 then wtype = 1			;set flag 1-dim fit
    endelse
  endif

;Validate ORDERS. If scalar, create/update header card. If vector, include
;  list in structure and delete card from header. Also, check whether the
;  wavelength information from above is consistent with order specification.
  if not keyword_set(orders) and $			;no new value,
     tag_exist(ech, 'ORDERS') then orders = ech.orders	; but have old value
  if keyword_set(orders) then begin
    sz = size(orders)					;variable info block
    if sz(0) gt 1 then begin				;true: array
      message, /cont, 'ORDERS must be scalar or vector - aborting'
      return
    endif
    norders = n_elements(orders)			;number of orders
    if norders eq 1 then begin				;true: OBASE formalism
      obase = orders(0)					;force to be scalar
      if wtype eq 2 then begin				;true: 2-dim WAVE fit
        if wave(3) ne obase then begin			;true: wvc mismatch
          message, /cont, 'base order number in WAVE disagrees with ORDERS'
          message, /cont, '...changing OBASE in WAVE coefficients - check!'
          wave(3) = obase				;change obase in wvc
        endif
      endif
      sxaddpar, head, 'OBASE', obase, before='COMMENT'	;(re)write OBASE card
    endif else begin					;else: order list
      if norders ne nord then begin			;true: wrong # of ords
        message, /cont, 'ORDERS has the wrong number of orders - aborting'
        return
      endif
      sxdelpar, head, 'OBASE'				;delete OBASE card
    endelse
  endif

;Now that WAVE and ORDERS have been processed, check the OBASE card in HEAD.
;  Trouble should only be possible if WAVE is a 2-dimensional fit, ORDERS has
;  not been specified (by keyword or presence in old structure), and there
;  is an incorrect OBASE card in the FITS header.
  head_obase = sxpar(head, 'OBASE')			;OBASE from header
  if wtype eq 2 and head_obase ne 0 then begin		;true: test is possible
    if wave(3) ne head_obase then begin			;true: mismatch
      message, /cont, 'base order number in WAVE disagrees with OBASE card'
      message, /cont, '...changing OBASE in WAVE coefficients - check!'
      wave(3) = head_obase				;change obase in wvc
    endif
  endif

;Validate ZAPIND and ZAPVAL together.
  if not keyword_set(zapind) and $			;no new value,
     tag_exist(ech, 'ZAPIND') then zapind = ech.zapind	; but have old value
  if not keyword_set(zapval) and $			;no new value,
     tag_exist(ech, 'ZAPVAL') then zapval = ech.zapval	; but have old value
  if not keyword_set(zapval) $
     and keyword_set(zapind) then begin			;only zapind defined
    message, /cont, 'ZAPIND defined without ZAPVAL - aborting'
    return
  endif
  if not keyword_set(zapind) $
     and keyword_set(zapval) then begin			;only zapval defined
    message, /cont, 'ZAPVAL defined without ZAPIND - aborting'
    return
  endif
  if keyword_set(zapind) then begin
    if n_elements(zapval) ne $
       n_elements(zapind) then begin
      message, /cont, 'ZAPIND and ZAPVAL have different lengths - aborting'
      return
    endif
    zapmin = min(zapind, max=zapmax)			;get extremes
    if zapmin lt 0 or $
       zapmax gt n_elements(spec) then begin		;true: diff lengths
      message, /cont, 'ZAPIND indexes outside of SPEC - aborting'
      return
    endif
  endif

;Write (or rewrite) RESOLUT into header.
  if keyword_set(resolut) then begin
    sxaddpar, head, 'RESOLUT', resolut $
            , before='COMMENT'				;(re)write RESOLUT
  endif

;Write (or rewrite) GAIN into header.
  if keyword_set(gain) then begin
    sxaddpar, head, 'GAIN', gain, before='COMMENT'	;(re)write GAIN
  endif

;Write (or rewrite) BARYCORR into header.
  if keyword_set(barycorr) then begin
    if strlowcase(barycorr) eq 'calc' then begin	;true: must calculate
      baryhead, head, barycorr				;calculate from header
    endif
    if strlowcase(barycorr) ne 'none' then begin	;true: must calculate
      sxaddpar, head, 'BARYCORR', barycorr $
              , ' barycentric velocity correction (km/s)' $
              , before='COMMENT'			;(re)write BARYCORR
    endif
  endif

;Write (or rewrite) RADVEL into header.
  if keyword_set(radvel) then begin
    if strlowcase(radvel) ne 'none' then begin
      sxaddpar, head, 'RADVEL', radvel $
              , ' radial velocity of source (km/s)' $
              , before='COMMENT'			;(re)write RADVEL
    endif
  endif

;Write version number of data structure format to header.
  headvers = sxpar(head, 'ECHVERS', count=c)		;version in header
  if not keyword_set(silent) $
     and not keyword_set(process) $
     and c gt 0 $
     and headvers ne echvers then begin			;true: new version
    message, /info, 'updating version number of data structure format'
  endif
  sxaddpar, head, 'ECHVERS', echvers, before='COMMENT' $
    , ' version number of echelle data format'		;re(write) ECHVERS
  
;Process wavelengths to build array with values for each pixel.
  if keyword_set(process) and wtype ne 0 then begin	;true: need to process
    case wtype of
      1: begin						;1-dim fits
         coef = wave					;copy coefficients
         wave = dblarr(ncol, nord)			;init array
         xcol = dindgen(ncol) / 1000d0			;argument of poly
         for iord=0, nord-1 do begin			;loop thru orders
           wave(*,iord) = poly(xcol, coef(*,iord))	;calculate wavelengths
         endfor
         wtype = 0					;clear flag
         end
      2: begin						;2-dim fits
         wvc = wave					;copy coefficients
         mkwave, wave, wvc 				;calculate wavelengths
         wtype = 0					;clear flag
         end
      else: message, 'unregonized wavelength type flag - check code'
    endcase
  endif

;Process continuum to build array with values for each pixel.
  if keyword_set(process) and ctype ne 0 then begin	;true: need to process
    case ctype of
      1: begin						;1-dim fits
         coef = cont					;copy coefficients
         cont = dblarr(ncol, nord)			;init array
         xcol = dindgen(ncol) / 1000d0			;argument of poly
         for iord=0, nord-1 do begin			;loop thru orders
           cont(*,iord) = poly(xcol, coef(*,iord))	;calculate continuum
         endfor
         ctype = 0					;clear flag
         end
      else: message, 'unregonized continuum type flag - check code'
    endcase
  endif

;Build a new structure with whatever data are available. Enforce canonical
;  order for known tags. Building a new structure avoids resizing problems.
  ech2 = { HEAD: head }					;init structure
  if keyword_set(wave) then ech2 $
    = create_struct(temporary(ech2), 'WAVE', wave)	;append wave
  if keyword_set(spec) then ech2 $
    = create_struct(temporary(ech2), 'SPEC', spec)	;append spectrum
  if keyword_set(sig) then ech2 $
    = create_struct(temporary(ech2), 'SIG', sig)	;append uncertainties
  if keyword_set(cont) then ech2 $
    = create_struct(temporary(ech2), 'CONT', cont)	;append continuum fit
  if keyword_set(orders) then begin
    if norders gt 1 then begin
      ech2 = create_struct( temporary(ech2) $
                          , 'ORDERS', orders)		;append order list
    endif
  endif
  if keyword_set(zapind) then ech2 $
    = create_struct(temporary(ech2), 'ZAPIND', zapind)	;append zap indices
  if keyword_set(zapval) then ech2 $
    = create_struct(temporary(ech2), 'ZAPVAL', zapval)	;append zap values

;Copy remaining fields into new structure.
  tags = tag_names(ech)					;list of tags in ech
  tags2 = tag_names(ech2)				;list of tags in ech2
  for i=0, n_elements(tags)-1 do begin			;loop thru old tags
    tag = tags(i)					;current new tag
    iwhr = where(tag eq tags2, nwhr)			;look for tag in ech2
    if nwhr eq 0 then begin				;true: need to copy
      ech2 = create_struct( temporary(ech2) $
                          , tags(i), ech.(i) )		;copy into new ech2
    endif
  endfor
; 
; 
;If we are dealing with a disk file, then write data to disk.
  case arg1sz(arg1sz(0)+1) of				;variable type
    7: wdech_raw, ech2, fullfile			;overwrite file
    8: file_or_struct = ech2				;return structure
    else: message, 'internal error - filename/structure mode changed?!'
  endcase
endfor  ; end of k (spectrum) loop 

end
