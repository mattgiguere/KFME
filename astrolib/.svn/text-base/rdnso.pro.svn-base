pro rdnso, w, s, wmin, wmax
;Reads requested portion of National Solar Observatory Atlas #1, which is an
;  optical, full-disk, FTS spectrum of a relatively inactive Sun.
; w (output vector) wavelength scale for s
; s (output vector) reesidual intensity segment of atlas.
; wmin (input scalar) lowest wavelength to return in w
; wmax (input scalar) highest wavelength to return in w
;Uses the data file nso.bin which has format:
;  intarr(1073):  header containing "base wavelengths" (in Angstroms) for each
;		   atlas segment, i.e. the integer part of the first wavelength
;		   in each segment.
;  fltarr(1024):  wavelengths increments for points in atlas segment 0.  Add
;		   the base wavelength for this segment to recover the actual
;		   wavelength points.
;  intarr(1024):  disk integrated, solar, residual intensity, multiplied by
;		   a scale factor of 30000 to match range of signed integers.
;  Plus an additional {fltarr(1024), intarr(1024)} pairs for the remaining
;   1072 atlas segments.  The total file length is 6,594,658 bytes, which is
;   2*1073 + 1073 * (4*1024 + 2*1024).
;03-Aug-90 JAV	Create.
;13-Oct-90 JAV	NSO atlas spectrum returned as double precision.
;12-Aug-90 JAV	Cleaned up Paul Butler's IDL adaptation of the ANA version.
;19-Oct-94 JAV	New data file location for use on casa machines.
;10-Apr-95 JAV	Reformatted and commented to improve readability of code.
;		Extra pixels no longer added outside requested range.
;01-May-99 JAV	Made initial value of for loop 0L instead of 0 to allow for
;                more than 32K segments in a single read.

if n_params() lt 4 then begin
  print, 'syntax: rdnso, w, s, wmin, wmax'
  retall
endif

;Verify requested wavelength range.
  if wmin ge wmax then begin			;true: bad wavelength range
    print, 'rdnso: bad wavelength range, wmin must be less than wmax.'
    retall
  endif

;Open atlas data file.
  dir = '/home/fischer/sme/'		;data directory
  file = 'nso.bin'				;data file
  openr, unit, dir + file, /get_lun		;open file, read only

;Get approximate starting wavelengths (I*2, Angstroms) of atlas segments.
  nseg = 1073					;number of wavelength segments
  wseg = intarr(nseg)				;init segment start wavelengths
  readu, unit, wseg				;read segment starts f/ header

;Create associated variable (skipping header) to allow random file access.
;Note that as of 10-Apr-95, making "segstr" anonymous leads to bus errors.
  pts = 1024					;spectrum points per segment
  segstr = {seg, w:fltarr(pts), s:intarr(pts)}	;record structure
  dskseg = assoc(unit, segstr, 2*nseg)		;associate, skip header

;Check blue edge.  Identify first segment to use.  Subtract one from
;  "ilo(nlo - 1)" in case wseg(i) < wmin < wstart(i) < wseg(i) + 1,
;  where wstart is the untruncated floating point beginning of segment i.
  ilo = where(wseg le wmin, nlo)		;segments below wmin
  if nlo le 0 then begin			;true: wmin too blue
    print, 'rdnso: "wmin" bluer than blue edge of atlas.'
    retall					;return with undefined spectrum
  endif
  iseg0 = ilo(nlo - 1) - 1			;index of first segment to use

;Check red edge.  Identify last segment to use.
  ihi = where(wseg ge wmax, nhi)		;segments above wmax
  if nhi le 0 then begin			;true: wmin too blue
    print, 'rdnso: "wmax" redder than red edge of atlas.'
    retall					;return with undefined spectrum
  endif
  iseg1 = ihi(0)				;index of last segment to use

;Read required spectrum segments.
  nseg = iseg1 - iseg0 + 1			;number of segments to use
  w = dblarr(pts * nseg)			;init wavelength buffer
  s = dblarr(pts * nseg)			;init spectrum buffer
  for is = 0L, nseg-1 do begin			;loop thru segments
    seg = dskseg(iseg0 + is)			;read current spectrum segment
    j0 = pts * is				;first index to use in w, s
    j1 = pts * (is + 1) - 1			;last index to use in w, s
    w(j0:j1) = wseg(iseg0+ is) $
             + double(seg.w)			;insert wavelength segment
    s(j0:j1) = double(seg.s)			;insert spectrum segment
  endfor

;Free logical unit, closing file.
  free_lun, unit				;free logical unit

;Trim excess spectrum from ends.  Renomalize residual intensity spectrum.
  npad = 0					;no extra padding
  ikeep = where(w ge wmin $
            and w le wmax, nkeep)		;spectrum points in range
  j0 = (ikeep(0) - npad) > 0			;add a tiny bit of padding
  j1 = (ikeep(nkeep-1) + npad) $
         < (pts * nseg - 1)			;add a tiny bit of padding
  w = w(j0 : j1)				;trim wavelengths
  s = s(j0 : j1) / 30000.0			;trim and renormalize spectrum

end
