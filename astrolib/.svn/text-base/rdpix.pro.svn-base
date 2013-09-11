pro rdpix, file, image
;Read an IRAF ".pix" file for Phoenix spectra into IDL.
;
;Input:
;  file (string) name of the IRAF ".pix" file to read.
;
;Output:
;  image (array(256,1024)) array containing the Phoenix image
;
;History:
; 98-Oct-05  Valenti  Wrote.

if n_params() lt 2 then begin
  print, 'syntax: rdpix, file, image'
  retall
endif

;Open file.
  openr, unit, file, /get_lun			;open file

;Read header and extract image size.
  header = intarr(1024)				;initialize image header
  readu, unit, header				;read binary header
  ncol = header(12)				;number of columns
  nrow = header(14)				;number of rows

;Read image.
  image = fltarr(ncol, nrow)			;initialize real*4 array
  readu, unit, image				;read image

;Close file.
  free_lun, unit				;close file

end
