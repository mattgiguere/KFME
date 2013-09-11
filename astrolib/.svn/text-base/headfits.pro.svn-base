function HEADFITS, filename, EXTEN = exten, Compress = compress
;+
; NAME:
;       HEADFITS
; PURPOSE:
;       Read a FITS (primary or extension) header into a string array.
; EXPLANATION:
;       Under Unix, HEADFITS() can also read gzip (.gz) or Unix compressed
;       (.Z) FITS files.   In IDL V5.3 or later, HEADFITS() can read gzip files
;       under any machine OS. 
;
; CALLING SEQUENCE:
;       Result = HEADFITS( filename ,[ EXTEN = , COMPRESS = ])
;
; INPUTS:
;       FILENAME = String containing the name of the FITS file to be read.
;                File names ending in '.gz' are assumed to be gzip'ed compressed
;                and under Unix file names ending in '.Z' are assumed to be
;                Unix compressed.    If this default behaviour is not sufficient
;                then use the COMPRESS keyword.
;
; OPTIONAL INPUT KEYWORD:
;      EXTEN  = integer scalar, specifying which FITS extension to read.
;               For example, to read the header of the first extension set
;               EXTEN = 1.   Default is to read the primary FITS header 
;               (EXTEN = 0).
;     COMPRESS - If this keyword is set and non-zero, then then treat the file
;              as compressed.  If 1 assume a gzipped file.   Where possible use
;              IDLs internal decompression facilities (i.e., v5.3 or greater) 
;              or on Unix systems spawn off a process to decompress and use its
;              output as the FITS stream.  If the keyword is not 1, then use 
;              its value as a string giving the command needed for 
;              decompression.   See FXPOSIT for more info.

;
; OUTPUTS:
;       Result of function = FITS header, string array
;
; EXAMPLE:
;       Print the main FITS header of a file 'test.fits' into a string 
;       variable, h
;
;       IDL>  print, headfits( 'test.fits')
;
;       Print the second extension header of a gzip compressed FITS file
;       'test.fits.gz' (Unix only).  Use HPRINT for pretty format
;
;       IDL> hprint, headfits( 'test.fits.gz', ext=2)
;
; PROCEDURES CALLED
;       FXPOSIT(), MRD_HREAD
; MODIFICATION HISTORY:
;       adapted by Frank Varosi from READFITS by Jim Wofford, January, 24 1989
;       Keyword EXTEN added, K.Venkatakrishna, May 1992
;       Make sure first 8 characters are 'SIMPLE'  W. Landsman October 1993
;       Check PCOUNT and GCOUNT   W. Landsman    December 1994
;       Major rewrite, work for Unix gzip files,   W. Landsman  April 1996
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added COMPRESS keyword  W. Landsman   April 2000
;-
 On_error,2

 if N_params() LT 1 then begin
     print,'Sytax - header = headfits( filename, [ EXTEN = ])'
     return, -1
 endif

  if not keyword_set(exten) then exten = 0
  unit = fxposit( filename, exten, /READONLY,compress = compress)
  if unit EQ -1 then return,-1
  if eof(unit) then begin
        free_lun,unit
        message,'ERROR - Extension past EOF',/CON
        return,-1
  endif
  mrd_hread, unit, header, status
  free_lun, unit
 
  return, header
  end
