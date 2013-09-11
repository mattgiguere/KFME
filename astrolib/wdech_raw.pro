pro wdech_raw, ech, file
;Writes an IDL spectrum structure to a ".ech" format disk FITS file with
;  binary table extensions. The FITS header is removed from the structure
;  and written as the primary header of the disk file. No error checking
;  is performed.
;
;Inputs:
; ECH (structure) echelle spectrum structure, which must contain a HEAD tag
;   corresponding to a valid ".ech" format FITS header; all other fields in
;   the structure are written to disk as binary table extension without
;   modification.
; FILE (string) root name of a ".ech" disk file; an optional ".ech"
;   extension may be included at the end of the root name; the specified
;   disk file will either be created or **overwritten** with altered
;   contents.
;
;Output:
; The specified disk file is created or modified.
;
;See also:
; modech, rdech, wdech
;
;History:
; 27-Jul-1998 Valenti  Initial coding.

if n_params() lt 2 then begin
  print, 'syntax: wdech_raw, ech, file (.ech assumed)'
  return
endif

;Make sure input data are in a structure.
  sz = size(ech)				;variable info block
  if sz(sz(0)+1) ne 8 then begin		;true: not a structure
    message, /cont, 'spectrum data must be a structure - aborting'
    return
  endif

;Make sure input structure contains header.
  if not tag_exist(ech, 'HEAD') then begin	;true: no HEAD tag
    message, /cont, 'spectrum structure does not contain HEAD tag - aborting'
    return
  endif

;If FRAME and UNITS tags exist, make sure they indicate raw structure data.
  raw = 1					;set raw flag
  if tag_exist(ech, 'FRAME') then begin		;true: tag exists
    if ech.frame ne 'raw' then raw = 0		;true: trouble, clear flag
  endif
  if tag_exist(ech, 'UNITS') then begin		;true: tag exists
    if ech.units ne 'raw' then raw = 0		;true: trouble, clear flag
  endif
  if not raw then begin				;true: processed data
    message, /cont, 'Data structure has been processed' $
                  + ' - only raw data may be saved.'
    message, /cont, 'Use "rdech, /raw" to obtain raw' $
                  + ' data structures from disk.'
    message, /cont, 'Then add or modify fields using' $
                  + ' modech.'
    message, /cont, 'Modified structure was not saved' $
                  + ' to disk.'
    return
  endif

;Create a new structure with the HEAD, FRAME, and UNITS fields removed.
  tags = tag_names(ech)				;list of tags
  ntags = n_elements(tags)			;number of tags
  icopy = where(tags ne 'HEAD' $
            and tags ne 'FRAME' $
            and tags ne 'UNITS', ncopy)		;find tags to copy
  if ncopy le 0 then begin			;true: no spectrum data
    message, /cont, 'structure does not contain spectrum data - aborting'
    return
  endif
  j = icopy(0)					;index of first copy tag
  newech = create_struct(tags(j), ech.(j))	;initialize structure
  for i=1, ncopy-1 do begin			;loop thru remaining tags
    j = icopy(i)				;index of current tag
    newech = create_struct(temporary(newech) $
                          ,tags(j), ech.(j))	;copy into new structure
  endfor

;Make a copy ofthe header and remove certain cards.
  head = ech.head
  sxdelpar, head, 'SIMPLE'
  sxdelpar, head, 'BITPIX'
  sxdelpar, head, 'NAXIS'
  sxdelpar, head, 'NAXIS1'
  sxdelpar, head, 'NAXIS2'
  sxdelpar, head, 'BSCALE'
  sxdelpar, head, 'BZERO'

;Write header and data as disk FITS file with binary table extensions.
  mwrfits, undefined, file, head, /create	;(re)create disk file
  mwrfits, newech, file				;append binary table extension

end
