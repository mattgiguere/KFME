pro pad2880, filelist
;Checks whether the number of bytes in file is a multiple of 2880. If not,
;  pads the end of file with nulls to reach the next multiple of 2880. This
;  is useful for converting otherwise acceptable FITS files into a format
;  palatable to the readfits routine.
;Input:
;  filelist (string) name of one or more files to be modified.
;Notes:
;  Run pad2880.pro on the files *after* running fix_bitpix.pro.
;
;30-Mar-98 Valenti  Wrote.

if n_params() lt 1 then begin
  print, 'syntax: pad2880, filelist'
  return
endif

;Expand wildcards and check that file(s) exist.
  flist = findfile(filelist)			;expand wildcards
  nfile = n_elements(flist)			;number of files
  if not keyword_set(flist) then begin
    message, 'No files found matching the pattern "' + filelist + '"'
  endif

;Loop through files, padding as needed.
  for ifile=0, nfile-1 do begin
    openu, unit, flist(ifile), /get_lun		;open current file
    finfo = fstat(unit)				;get file info
    if finfo.size mod 2880 ne 0 then begin	;true: need to pad
      npad = 2880 - (finfo.size mod 2880)	;number of pad bytes
      point_lun, unit, finfo.size		;move beyond last byte
      writeu, unit, replicate(0b, npad)		;write pad bytes
    endif
    free_lun, unit				;close current file
  endfor

end
