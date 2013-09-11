pro rdech, ech, file, olist, zapsig=zapsig, silent=silent, gain=gain $
         , raw=raw, norv=norv, nobary=nobary, nocont=nocont, nozap=nozap
;Reads a ".ech" disk FITS file with binary table extensions, creating an
;  IDL spectrum structure. The primary FITS header from the disk file is
;  inserted into the structure that is returned. If possible, the default
;  behavior is to create a wavelength scale in the reference frame of the
;  source and to normalize the continuum.
;
;Input:
; FILE (string) root name of an existing ".ech" format disk file; an
;   optional ".ech" extension may be included at the end of the root name.
; [OLIST] (vector) or (scalar) list of orders to return; if the requested
;   orders are not present in the disk file, the read operation will fail.
; [ZAPSIG=] (scalar) or (vector(nzap)) bad pixel flag value or list of values
;   for spectrum uncertainties, i.e. ECH.SIG(ZAPIND) = ZAPSIG; by default
;   ZAPSIG is zero, but the flag value can be changed with this keyword; for
;   example, routines that don't check for SIG=0 can usually be tricked into
;   ignoring bad pixels by setting SIG to a value much larger than max(SIG);
;   if the vector format is used, the number of elements in ZAPSIG must agree
;   with the number of bad pixels flagged in the .ech file.
; [/SILENT] (switch) suppresses certain diagnostic and warning messages;
;   in particular, no warnings will be issued for situations where ECH.WAVE
;   cannot be placed in the reference frame of the source or where ECH.SPEC
;   cannot be normalized.
; [/GAIN] (switch) causes units of spectral data to be converted from ADU to
;   electrons, if a GAIN card exists in the FITS header; a warning is printed
;   if /GAIN is true, /SILENT is false, and the GAIN card is missing; if the
;   data structure contains a continuum description, then /NOCONT must also
;   be specified to supress continuum normalization; /GAIN leaves ECH.CONT
;   in electrons, regardless of whether of not /NOCONT is specified.
; [/RAW] (switch) suppresses the default processing of data read from FILE:
;   wavelength coefficients are not expanded into wavelengths for each pixel;
;   no barycentric or radial velocity corrections are applied; continuum fit
;   coefficients are not expanded; spectral orders are not normalized; bad
;   pixels are not zapped; spectrum uncertainties are not modified; none of
;   the other keywords may be used with /RAW; the main purpose of /RAW is to
;   create an IDL structure representation of the raw data structure in FILE;
;   the raw data can be modified using modech.pro or by some other means;
; [/NORV] (switch) suppresses radial velocity correction of ECH.WAVE, even if
;   ECH.HEAD contains a RADVEL card; barycentric correction is still applied;
;   if ECH.HEAD contains a BARYCORR card, ECH.WAVE will be in the barycentric
;   frame; otherwise, ECH.WAVE will be in the observatory frame and a warning
;   will be issued (unless /SILENT is set).
; [/NOBARY] (switch) suppresses barycentric correction of ECH.WAVE, even if
;   ECH.HEAD contains a BARYCORR card; also implies /NORV; ECH.WAVE will be
;   in the observatory frame.
; [/NOCONT] (switch) suppresses continuum normalization of ECH.SPEC and
;   adjustment of ECH.SIG, even if ECH.CONT is definied; the spectrum and
;   uncertainty will be left in ADU.
; [/NOZAP] (switch) suppresses replacement of bad pixels specified in ZAPIND
;   with values in ZAPVAL, even if these data fields are defined in FILE;
;   spectrum uncertainties are not modified either.
;
;Output:
; ECH (structure) echelle spectrum structure consisting of the primary
;   header from the disk file and all binary table extensions; HEAD is the
;   tag name for the header; FITS extension labels are used to tag all other
;   structure fields; see the documentation in modech.pro for a detailed
;   description of other fields, except for the following fields that are
;   *not* stored in the disk file:
;
;    .FRAME (string) flag indicating the reference frame of ECH.WAVE:
;       'source' = frame at rest with respect to the observed source
;       'bary' = frame at rest with respect to barycenter of solar system
;       'obs' = observatory rest frame
;       'none' = no wavelength scale
;       'raw' = exact copy of information in FILE
;    .UNITS (string) units for ECH.SPEC, ECH.SIG, and ECH.CONT:
;       'norm' = residual intensity with the continuum in ECH.SPEC normalized
;       'adu' = analog to digital units used by readout amplifier
;       'elec' = electrons recorded by detector
;       'erg/s/cm^2/A' = physical flux units (for future use)
;       'raw' = exact copy of information in FILE
;
;See also:
;  modech, wdech, wdech_build
;
;History:
; 27-Jul-1998 Valenti  Initial coding.
; 03-May-1999 Valenti  Fixed bug that crashed routine when file had ZAPIND.

if n_params() lt 2 then begin
  print, 'syntax: rdech, ech, file [ ,olist ,zapsig= ,/silent ,/gap]'
  print, '                [ ,/raw ,/norv ,/nobary ,/nocont ,/nozap ]'
  return
endif

if n_params() ge 3 then begin
  print, 'Order subset logic not yet implemented - contact Jeff Valenti.'
  return
endif

;Make sure input filename is a string.
  sz = size(file)				;variable info block
  if sz(sz(0)+1) ne 7 then begin		;true: not a string
    message, /cont, 'filename must be a string - aborting'
    return
  endif

;Append file extension, if not already present.
  iext = strpos(file, '.ech')			;look for extension
  if iext lt 0 then begin			;true: no extension
    fullfile = file + '.ech'			;append extension
  endif else begin				;else: already has extension
    fullfile = file				;use as is
  endelse

;Read header and data from disk FITS file with binary table extensions.
  junk = mrdfits(fullfile, 0, head, /silent)	;read main header
  ech = mrdfits(fullfile, 1, xhead, /silent)	;read binary data

;Check that structure is defined to avoid more cryptic errors later.
  if not keyword_set(ech) then begin		;true: not defined
    message, /cont, 'file does not contain expected data structure'
    return
  endif

;Prepend header in structure.
  ech = create_struct( temporary(ech) $
                     , 'HEAD', head )		;prepend header

;Initialize internal variables for tracking FRAME and UNITS.
  frame = 'raw'					;default is raw
  units = 'raw'					;default is raw

;Handle case where structure is to contain raw data from the file.
  if keyword_set(raw) then begin		;true: raw format
    ech = create_struct( temporary(ech) $
                       ,'FRAME', frame )	;flag raw WAVE format
    ech = create_struct( temporary(ech) $
                       ,'UNITS', units )	;flag raw SPEC, SIG, CONT
    return
  endif

;Expand data fields that have coefficients or other non-canonical formats.
  modech, ech, /process				;expand all fields

;Change our default FRAME and UNITS.
  if tag_exist(ech, 'WAVE') then begin		;true: have wavelengths
    frame = 'obs'				;observatory by convention
  endif else begin				;else: no wavelengths
    frame = 'none'				;may still not have WAVE
  endelse
  units = 'adu'					;analog digital units

;Shift wavelength scale to the desired reference frame.
  if not keyword_set(nobary) $
     and tag_exist(ech, 'WAVE') then begin	;true: try to shift to bary
    barycorr = sxpar(head, 'BARYCORR', count=c)	;look for bary correction
    if c le 0 then begin			;true: no BAYCORR card
      if not keyword_set(silent) then begin	;true: print warning
        print, '  no BARYCORR card in header - observatory wavelengths'
      endif
    endif else begin				;else: can shift to bary
      ech.wave = ech.wave $
               * (1d0 + barycorr / 2.9979246d5)	;shift to barycentric frame
      frame = 'bary'				;update frame flag
    endelse

    if not keyword_set(norv) then begin		;true: try to shift to source
      radvel = sxpar(head, 'RADVEL', count=c)	;look for radial velocity
      if c le 0 then begin			;true: no BAYCORR card
        if not keyword_set(silent) then begin	;true: print warning
          print, '  no RADVEL card in header - barycentric wavelengths'
        endif
      endif else begin				;else: can shift to bary
        ech.wave = ech.wave $
                 * (1d0 - radvel / 2.9979246d5)	;shift to barycentric frame
        frame = 'source'			;update frame
      endelse
    endif
  endif

;Process bad pixels before all other spectrum transformations.
  if not keyword_set(nozap) $
     and tag_exist(ech, 'ZAPIND') then begin	;true: fix bad pixels
    error = 0					;clear error flag
    if not tag_exist(ech, 'ZAPVAL') then begin	;true: no zap values
      message, /cont, 'ZAPIND without ZAPVAL - bad pixels not repaired'
      error = 1					;set error flag
    endif

    maxind = max(ech.zapind, min=minind)	;get extreme index values
    if maxind ge n_elements(ech.spec) or $
       minind lt 0 then begin			;bad index values
      message, /cont, 'ZAPIND out of range - bad pixels not repaired'
      error = 1					;set error flag
    endif

    nzapsig = n_elements(zapsig)		;number of zapsig values
    if nzapsig ne n_elements(ech.zapind) and $
       nzapsig gt 0 then begin			;true: size mismatch
      message, /cont, 'ZAPSIG and ECH.ZAPIND size mismatch' $
                    + ' - bad pixels not repaired'
      error = 1					;set error flag
    endif

    if not error then begin			;true: no error, fix pixels
      ech.spec(ech.zapind) = ech.zapval		;replace with new values
      if keyword_set(zapsig) then begin		;true: special value for sigma
        ech.sig(ech.zapind) = zapsig		;flag with special value
      endif else begin				;else: use default value
        ech.sig(ech.zapind) = 0.0		;zero sigma as flag value
      endelse
    endif
  endif

;Apply gain correction.
  if keyword_set(gain) then begin		;true: try to apply gain
    gain = sxpar(head, 'GAIN', count=c)		;look for gain
    if c le 0 then begin			;true: no BAYCORR card
      if not keyword_set(silent) then $		;true: print warning
        print, '  no GAIN card in header - units are still ADU'
    endif else begin				;else: can shift to bary
      ech.spec = gain * ech.spec		;convert to electrons
      if tag_exist(ech, 'SIG') then $		;true: have uncertainties
        ech.sig  = gain * ech.sig		;convert to electrons
      if tag_exist(ech, 'CONT') then $		;true: have continuum
        ech.cont  = gain * ech.cont		;convert to electrons
      units = 'elec'				;update units
    endelse
  endif

;Normalize continuum.
  if not keyword_set(nocont) $
     and tag_exist(ech, 'CONT') then begin	;true: normalize continuum
    ech.spec = ech.spec / ech.cont		;normalize continuum
    if tag_exist(ech, 'SIG') then $		;true: have uncertainties
      ech.sig  = ech.sig / ech.cont		;renormalize uncertainties
    units = 'norm'				;set appropriate units
  endif

;Create structure entries for FRAME and UNITS flags.
  ech = create_struct( temporary(ech) $
                     ,'FRAME', frame )		;wavelength reference frame
  ech = create_struct( temporary(ech) $
                     ,'UNITS', units )		;spectrum units

end
