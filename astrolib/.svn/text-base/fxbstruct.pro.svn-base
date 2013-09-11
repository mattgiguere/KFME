pro fxbstruct, file, struct, exten=exten, rowbeg=rowbeg, rowend=rowend
;Read fits binary table into an IDL structure.
;Input:
; file (string) name of fits file with binary table extension
; [exten=] (scalar) number of extension to read
; [rowbeg=] (scalar) first row number to read in table
; [rowend=] (scalar) last row number to read in table
;Output:
; struct (structure) binary table data extracted from file
;   .file_ (string) name of FITS file from which binary table was read
;   .head_ (string vector) main fits header from fits file
;   .xhead_ (string vector) extension header from fits file
;   .exten_ (scalar) number of extensions that were in FITS file
;   .ncol_ (scalar) number of columns read from binary table
;   .nrow_ (scalar) number of rows read from binary table
;   .bunit_ (string) default units for rows?
;   .ttype_ (string vector) type names for all data read from file
;   .tunit_ (string vector) units for all data read from file
;   ... one additional record for each data column in the file
;       tag names are simply the column types from the extension header
;Example:
; IDL> fxbstruct, 'o53p01010_jit.fits', jit
; IDL> fxbstruct, 'o53p01010_x1d.fits', spec
;History:
; 1999-Feb-17 Valenti  Wrote.
; 2001-Apr-06 Valenti  Added rowbeg= and rowend= feature.

if n_params() lt 2 then begin
  print, 'syntax: fxbstruct, file, struct [,exten= ,rowbeg=, rewend=]'
  retall
endif

;Set defaults for optional keywords.
  if n_elements(exten) eq 0 then exten = 1

;Test that file exists.
  if not keyword_set(findfile(file)) then begin
    message, /info, 'unable to find ' + file
    return
  endif

;Get main header and check for extensions.
  head = headfits(file)
  if n_elements(head) le 1 then begin
    message, /info, file + ' does not appear to be a fits file'
    return
  endif
  if not fxpar(head, 'EXTEND') then begin
    message, /info, file + ' does not appear to contain extension'
    return
  endif
  nextend = fxpar(head, 'NEXTEND', count=count)
  if count eq 0 then begin
;   message, /info, file + ' is missing NEXTEND card in header'
;   return
    nextend = 1					;assume 1 extension
  endif
  if exten gt nextend then begin
    if nextend eq 1 then plural = '' else plural = 's'
    message, /info, file + ' has ' + strtrim(nextend, 2) $
      + ' extension' + plural + ', you requested number ' $
      + strtrim(exten, 2)
    return
  endif

;Read header from desired extension.
  fits_read, file, data, xhead, exten=exten, /noscale, /no_pdu $
           , /no_abort, message=message, /header_only
  if !err eq -1 then begin
    message, /info, 'error reading extension in ' + file
    message, /info, message
    return
  endif
  rsiz = long(sxpar(xhead, 'NAXIS1'))
  nrow = long(sxpar(xhead, 'NAXIS2'))

;Read data from desired extension.
  if n_elements(rowbeg) eq 0 and n_elements(rowend) eq 0 then begin
    fits_read, file, data, xhead, exten=exten, /noscale, /no_pdu $
             , /no_abort, message=message
  endif else begin
    if n_elements(rowbeg) eq 0 then rowbeg = 0
    if n_elements(rowend) eq 0 then rowend = nrow-1
    nrow = rowend - rowbeg + 1
    fits_read, file, data, xhead, exten=exten, /noscale, /no_pdu $
             , /no_abort, message=message, first=rowbeg*rsiz $
             , last=(rowend+1)*rsiz-1
    data = reform(data, rsiz, nrow)
  endelse
  if !err eq -1 then begin
    message, /info, 'error reading extension in ' + file
    message, /info, message
    return
  endif

;Check that extension contains a binary table.
  xtension = fxpar(xhead, 'XTENSION', count=count)
  if count eq 0 then begin
    message, /info, 'header for extension ' + strtrim(exten, 2) $
      + ' is missing XTENSION card.'
    return
  endif
  if strtrim(strupcase(xtension), 2) ne 'BINTABLE' then begin
    message, /info, 'extension ' + strtrim(exten, 2) + ' of ' $
      + file + ' is not a binary table'
    return
  endif

;Extract number of rows.
  if count eq 0 then begin
    message, /info, 'no NAXIS2 card in header for extension ' $
      + strtrim(exten, 2) + ' of ' + file
    return
  endif

;Extract brightness units.
  bunit = fxpar(xhead, 'BUNIT', count=count)
  if count eq 0 then begin
;   message, /info, 'no BUNIT card in header for extension ' $
;     + strtrim(exten, 2) + ' of ' + file
;   return
    bunit = ''				;no units
  endif
  bunit = strtrim(bunit, 2)

;Extract information about table columns from extension header.
  ttype = fxpar(xhead, 'TTYPE*', count=ncol)
  if ncol eq 0 then begin
    message, /info, 'no TTYPE* cards in header for extension ' $
      + strtrim(exten, 2) + ' of ' + file
    return
  endif
  ncol = fix(ncol)
  ttype = strtrim(ttype, 2)

  tform = fxpar(xhead, 'TFORM*', count=count)
  if count ne ncol then begin
    message, /info, strtrim(ncol, 2) + ' TTYPE* cards, but ' $
      + strtrim(count, 2) + ' TFORM* header cards for extension ' $
      + strtrim(exten, 2) + ' of ' + file
    return
  endif
  tform = strtrim(tform, 2)

  tunit = strarr(ncol)
  for icol=0, ncol-1 do begin
    u = fxpar(xhead, 'TUNIT'+strtrim(icol+1, 2), count=count)
    if keyword_set(u) then tunit(icol) = u
  endfor
  tunit = strtrim(tunit, 2)

;Begin building output structure. (Use "_" to distinguish from column data).
  struct = $
   { file_  : file  $
   , head_  : head  $
   , xhead_ : xhead $
   , exten_ : exten $
   , ncol_  : ncol  $
   , nrow_  : nrow  $
   , bunit_ : bunit $
   , ttype_ : ttype $
   , tunit_ : tunit $
   }

;Extract fields from binary table, convert data type, put in structure.
  j = 0
  for i=0, ncol-1 do begin

;Separate numeric prefix, if present, from format code.
    tf = tform(i)					;extract current fmt
    count = long('0' + tf)				;ignores letters
    if count eq 0 then count = 1L			;use 1, if no number
    fchar = strmid(tf, strlen(tf)-1, 1)			;last character

;Parse format character.
    case fchar of
      'A' : begin & nbyte = 1 & func = 'string' & end
      'B' : begin & nbyte = 1 & func = 'byte'   & end
      'I' : begin & nbyte = 2 & func = 'fix'    & end
      'J' : begin & nbyte = 4 & func = 'long'   & end
      'E' : begin & nbyte = 4 & func = 'float'  & end
      'D' : begin & nbyte = 8 & func = 'double' & end
      else: begin
              message, /info , 'TFORM' + strtrim(i, 2) $
                + ' has unknown format (' + tform(i) $
                + ') in extension ' + strtrim(exten, 2) $
                + ' of ' + file
              return
            end
    endcase

;Extract and convert data.
    col = data(j:j+count*nbyte-1,*)			;extract data
    j = j + count*nbyte					;advance pointer
    if func eq 'string' then begin
      col = call_function(func, col)			;convert to string
    endif else begin
      col = call_function(func, col, 0, count*nrow)	;convert data type
      if count gt 1 then begin
        col = reform(col, count, nrow)
      endif
    endelse
    struct = create_struct(struct, ttype(i), col)
  endfor

end
