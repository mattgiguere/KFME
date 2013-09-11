pro rdisp,spec,file,onum,bcol,ecol,silent=silent,comm=comm $
	 ,relative=relative,obase=obase,unc=unc
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
; unc= (output vector or array) uncertainty values for spectrum.
;3-Mar-92 jav Adapted from wdisp.pro.
;19-Sep-92 jav Changed to support version 2 headers with obase. Added /relative
;		keyword. Changed default onum interpretation to absolute order.
;17-Jul-93 jav No functional change; printed information modified slightly.
;07-Jun-94 jav Passing negative ord, bcol, and ecol is now equivalent to
;		not passing these arguments at all.
;07-May-98 CMJ Adapted from rdisp.pro to read new fits format reduced spectra

if n_params() lt 2 then begin
  print,'syntax: rdisp,spec,file(.isp assumed)' $
    + ' [onum,bcol,ecol,/sil,/rel,obase=,comm=,unc=]'
  retall
end

;Read disk file and determine some things about it
  fname = file+'.isp'                           ;build full filename
  sp = readfits(fname,head)                     ;read file + header
  ncol = (size(sp))(1)                          ;get number of columns
  nord = (size(sp))(2)/2                        ;get number of orders

;Set up a few flags and useful quantities
  if n_elements(onum) eq 0 then onum = -1	;flag no argument passed
  if n_elements(bcol) eq 0 then bcol = 0 	;set to 0 if not requested
  if n_elements(ecol) eq 0 then ecol = ncol-1   ;set to max if not requested

;Now look through header to get object, base order number, and comments
  iobject = 1                                   ;set object flag
  icomm = 1                                     ;set the comment flag
  obase = 0                                     ;set obase to 0
  ncard = n_elements(head)                      ;get number of cards in header
  for i=0,ncard-1 do begin                      ;loop through cards
     card=head(i)                               ;extract current card
     eqpos=strpos(card,'=')                     ;find position of '='
     tag=strcompress(strmid(card,0,eqpos-1),/remove_all)  ; idnet. tag
     if tag eq 'OBJECT' then begin              ;object name
        iobject = 0                             ;flag object found
        slashpos=strpos(card,'/')               ;find the slash
        object=strmid(card,eqpos+1,slashpos-eqpos+1) ;pull out object name
     endif
     if tag eq 'OBASE' then begin               ;found base order number
        obase=fix(strmid(card,eqpos+1,10))      ;extract base order number
     endif
     if tag eq 'COMMENT' then begin             ;any comments?
        if icomm eq 1 then begin                ;first comment?
           comm=card                            ;load comment
           icomm = 0                            ;flag first comment
        endif else begin                        ;subsequent comment?
           comm=[comm,card]                     ;add comment
        endelse
     endif
  endfor   

;Now print some info to the screen
  if iobject eq 1 then begin                    ;any object found?
     object='No object name found'
  endif
  if icomm eq 1 then begin                      ;any comments set?
     comm = 'There were no comments.'
  endif
  if not keyword_set(silent) then begin         ;true: print header info
     print,'RDISP: '+object                     ;print object name
     print,'Cols = '+strtrim(string(ncol),2) $  ;and columns
      + ', Ords = '+strtrim(string(nord),2) $   ;and orders
      + ', Base = '+strtrim(string(obase),2)    ;and base order number
     hprint,comm                                ;print comments to screen
  endif

;Now load the spectrum and uncertainty and trim as requested
  if onum lt 0 then begin                       ;return entire spectrum
     spec=sp(bcol:ecol,0:nord-1)                ;load spectrum
     unc=sp(bcol:ecol,nord:2*nord-1)            ;load uncertainty
  endif else begin
     if keyword_set(relative) then begin        ;force relative order number?
        if onum lt 0 or onum gt nord-1 then begin  ;is request in the spectrum?
           message,'Requested order not in spectrum - aborting'
        endif else begin
           spec=sp(bcol:ecol,onum)              ;load spectrum
           unc=sp(bcol:ecol,onum+nord)          ;load uncertainty
        endelse
     endif else begin
        index=onum-obase                        ;find the proper index
        if index lt 0 or index gt nord-1 then begin ;is request in the spectrum?
           message,'Requested order not in spectrum - aborting'
        endif else begin
           spec=sp(bcol:ecol,index)             ;load spectrum
           unc=sp(bcol:ecol,index+nord)         ;load uncertainty
        endelse
     endelse
  endelse
  
  return

end
