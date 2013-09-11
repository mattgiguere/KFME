pro rdisp,spec,file,onum,bcol,ecol,silent=silent,comm=comm $
	 ,relative=relative,obase=obase
;Reads hamilton spectrum from .isp disk file. Each order is recovered from
;  an I*2 vector with two real*4 scale factors (bscale, bzero) prepended.
;  Scaling transformation is:  real = (bscale * integer) + bzero.
; spec (output array (ncol, nord)) spectrum array to read from disk.
; file (input string) name of file to access.
; onum (input scalar) if blank, then all orders are read from file; otherwise,
;   only the order specfied by onum is read. Onum is interpreted as an
;   absolute order number, unless /relative is specified.
; bcol (input scalar) lowest column number desired for single order read.
; ecol (input scalar) highest column number desired for single order read.
; /silent (flag) supresses header information display.
; /relative (flag) relevant only when onum is specified; forces onum to be
;   interpreted as relative order number (i.e. top order in spectrum is zero),
;   rather than absolute order number (i.e. order of the diffraction grating).
; obase= (output scalar) returns the absolute order number of the zeroeth
;   order stored in file, as recorded in the .isp header. Returns 0 if the
;   information is unknown.
; comm= (output string) comment string read from header file (may be null).
;3-Mar-92 jav Adapted from wdisp.pro.
;19-Sep-92 jav Changed to support version 2 headers with obase. Added /relative
;		keyword. Changed default onum interpretation to absolute order.
;17-Jul-93 jav No functional change; printed information modified slightly.
;07-Jun-94 jav Passing negative ord, bcol, and ecol is now equivalent to
;		not passing these arguments at all.

if n_params() lt 2 then begin
  print,'syntax: rdisp,spec,file(.isp assumed)' $
    + ' [onum,bcol,ecol,/sil,/rel,obase=,comm=]'
  retall
end

  if n_elements(onum) eq 0 then onum = -1	;flag no argument passed
  if n_elements(bcol) eq 0 then bcol = -1	;flag no argument passed
  if n_elements(ecol) eq 0 then ecol = -1	;flag no argument passed

;Open file for input.
  openr,unit,file+'.isp',/get_lun		;get free unit

;Verify specified file is a .isp file.
  vrfy_vers1 = 123456789L			;flag value for version 1
  vrfy_vers2 = 234567891L			;flag value for version 2
  vrfy = long(0)				;init I*4 verify flag
  readu,unit,vrfy				;read first long word
  case vrfy of					;handle vrfy cases
    vrfy_vers1: v1 = (0 eq 0)			;set v1 flag to true
    vrfy_vers2: v1 = (0 eq 1)			;set v1 flag to false
    else: message,'Specified file does not have .isp format.'
  endcase

;Read remaining header information.
  ncol = long(0)				;init I*4 number of columns
  nord = long(0)				;init I*4 number of orders
  obase = long(0)				;init I*4 base order number
  clen = long(0)				;init I*4 comment length
  fill = long(0)				;init I*4 reserved filler
  if v1 then begin				;true: version 1
    readu,unit,ncol,nord,clen			;read sizes from header
    obase = 0L					;flag unknown base order
  endif else begin				;else: version 2
    readu,unit,ncol,nord,obase,clen		;read sizes from header
  endelse
  if clen gt 0 then begin			;true: comment text in file
    comm = string(replicate(32b,clen))		;init string of proper length
    readu,unit,comm				;read string
    outcom = comm				;special output string
  endif else outcom=''				;else: null comment
  if not v1 then readu,unit,fill,fill,fill,fill	;for version 2, skip filler
  if not keyword_set(silent) then begin		;true: print header info
    print,'Cols = '+strtrim(string(ncol),2) $
      + ', Ords = '+strtrim(string(nord),2) $
      + ', Base = '+strtrim(string(obase),2) $
      + ', ',outcom
  endif

;Decide how much data to read and read it.
  bscale = float(1.0)				;init real*4 scale factor
  bzero = float(0.0)				;init real*4 zero point
  i2ord = intarr(ncol)				;init I*2 data for 1 order

;Single order case:
  if onum ge 0 then begin			;true: get single order
    if keyword_set(relative) then begin		;true: onum is relative
      ronum = onum				;just copy onum
    endif else begin				;else: onum is absolute
      if obase eq 0 then begin			; true: can't find absolute
	message,/info $
	  ,'Base order unknown. Use /relative and specify a relative onum.'
	retall
      endif
      ronum = onum - obase			;convert to relative order
    endelse
    if ronum lt 0 or ronum gt nord-1 then begin	;true: no such order in file
      message,'File does not contain requested order.'
    endif
    if bcol lt 0 then bcol = 0			;no bcol given, use 0
    if ecol lt 0 then ecol = ncol-1		;no ecol given, use ncol-1
    if bcol lt 0 or ecol gt ncol-1 then begin	;true: no such columns
      message,'File does not contain requested columns.'
    endif
    offset = 2 * ronum * (ncol + 4)		;number of bytes to skip over
    point_lun,-unit,posn			;get current position
    point_lun,unit,posn+offset			;point at desired order
    readu,unit,bscale,bzero,i2ord		;read scale factors and data
    i2ord = i2ord(bcol:ecol)			;trim unwanted spectrum
    spec = (bscale * float(i2ord)) + bzero	;map back to original data
 
;Entire spectrum case:
  endif else begin				;else: get entire spectrum
    spec = fltarr(ncol,nord)			;init spectrum array
    for i=0,nord-1 do begin			;loop thru orders
      readu,unit,bscale,bzero,i2ord		;read scale factors and data
      spec(*,i) = (bscale*float(i2ord)) + bzero	;map back to original data
    endfor					;loop back for next order
  endelse

;Close disk file. Exit.
  free_lun,unit
  return

end
