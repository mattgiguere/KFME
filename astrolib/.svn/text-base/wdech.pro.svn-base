pro wdech, file, head, spec, overwrite=overwrite, _EXTRA=extra
;Write a reduced echelle spectrum to a ".ech" disk file. The disk file
;  will be a FITS file with binary table extensions.
;
;Inputs:
;
; >> The following positional arguments are mandatory.
;
; FILE (string) root name for the FITS file that will be created; the file
;   extension will be ".ech"; if FILE already ends with ".ech", then this
;   is interpreted as the extension and a second ".ech" is not appended.
; HEAD (string array(ncard)) FITS header for the spectrum; written as the
;   primary header for the FITS file; additional information is included
;   in the header written to disk, but the input variable HEAD is not
;   modified.
; SPEC (array(ncol,nord)) OR (vector(ncol)) spectrum for one or more grating
;   orders; by convention the units are ADU, but this is not enforced; the
;   number of columns must be at least 21, otherwise the automatic format
;   logic for CONT and WAVE will fail when the spectrum is read; a vector
;   argument is interpreted as a single grating order.
; [/OVERWRITE] (switch) required if an existing file is to be overwritten.
;
; >> The following optional keyword parameters are bundled into EXTRA and
; >> passed to modech.pro, which actually checks argument validity and
; >> incorporates auxiliary data into the spectrum structure.
; >> SEE modech.pro FOR A DETAILED DESCRIPTION OF THE FOLLOWING PARAMETERS!
;
; [SIG=] (array(ncol,nord)) OR (vector(ncol)) uncertainty in each spectrum
;   pixel.
; [CONT=] (array(ncol,nord)) OR (array(ndeg+1,nord)) continuum values for
;   SPEC, specified for each pixel or as polynomial coefficients for each
;   order.
; [WAVE=] (array(ncol,nord)) OR (array(ndeg+1,nord)) OR vector(ncoef)
;   observatory wavelengths for SPEC: specified for each pixel, or as
;   polynomial coefficents for each order, or as a 2-dim polynomial fit.
; [ORDERS=] (vector(nord)) or (scalar) grating orders contained in SPEC,
;   specified as an explicit list or as a base order number.
; [RESOLUT=] (scalar) resolution (wavelength/FWHM) of the spectrum.
; [GAIN=] (scalar) gain of the readout amplifier (electrons/ADU)
; [ZAPIND=] (vector(nzap)) list of 1-dimensional indices giving the pixels
;   in SPEC that are bad.
; [ZAPVAL=] (vector(nzap)) list of replacement values for pixels specified
;   by ZAPIND.
; [BARYCORR=] (scalar) barycentric velocity correction (km/s) that is to be
;   applied to the observatory frame wavelength scale given by WAVE.
; [RADVEL=] (scalar) radial velocity (km/s) of the source that was observed
;   to produce SPEC.
;
;Output:
; Creates a FITS file with binary table extensions, containing all the
;   specified spectrum information. Will not overwrite an existing file
;   unless /OVERWRITE is specified.
;
;History:
; 27-Jul-1998 Valenti  Initial coding; supersedes wdisp.pro
;
;See also:
; modech, rdech, wdech_raw

if n_params() lt 3 then begin
  modech, syntax='wdech, file, head, spec'
  print, '        [,/overwrite ]
  return
end

;Append file extension, if not already present.
  iext = strpos(file, '.ech')			;look for extension
  if iext lt 0 then begin			;true: no extension
    fullfile = file + '.ech'			;append extension
  endif else begin				;else: already has extension
    fullfile = file				;use as is
  endelse

;Check whether we would inadvertently be overwriting an existing file.
  if not keyword_set(overwrite)	then begin	;true: file should not exist
    dummy = findfile(fullfile, count=count)	;look for file
    if count gt 0 then begin			;true: but file does exist
      message, /cont, 'use /OVERWRITE to overwrite existing file - aborting'
      return
    endif
  endif

;Check that SPEC appears to be valid.
  sdim = (size(spec))(0)			;dimensionality of SPEC
  if sdim ne 1 and sdim ne 2 then begin		;true: not a spectrum
    message, /cont,'spectrum must be vector or 2-dimensional array.'
  endif

;Create a structure with the minimum required data.
  ech = { head: head, spec: spec }		;minimal data structure
      
;Call the spectrum updating routine to add auxiliary information.
  modech, ech, _extra=extra			;add auxiliary data

;Write spectrum to disk with no further modification.
  wdech_raw, ech, fullfile			;write spectrum to disk

end
