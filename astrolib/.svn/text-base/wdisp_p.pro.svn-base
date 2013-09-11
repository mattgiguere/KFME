pro wdisp,spec,sunc,file,comm,obase=obase,head=head
;Writes hamilton spectrum to .isp disk file. Each order is written as an
;  I*2 vector with two real*4 scale factors (bscale, bzero) prepended.
;  Scaling to I*2 is performed to halve the disk space required.
;  Scaling transformation is:  integer = (real - bzero) / bscale.
; spec (input array (ncol, nord)) spectrum array to write to disk.
; file (input string) name of file to create.
; comm (input string) coment to include in file header.
;2-Mar-92 jav Adapt from wdhsp.ana.
;19-Sep-92 jav Upgrade to version 2, new header format with obase.
;17-Jul-93 jav Avoid divide by zero when bscale=0.
;01-Feb-95 CMJ Make syntax statement reflect fact that comm is not a keyword.
;05-May-98 CMJ Adapted from old wdisp, but made it FITS.  Include image header.

if n_params() lt 3 then begin
  print,'syntax: wdisp,spec,sunc,file(.isp assumed) [,comm,obase=,head=]'
  retall
end

;Parse optional arguments and set defaults.
  if n_params() gt 3 then $
    clen = long(strlen(comm)) else clen = 0L	;get comment length
  if n_elements(obase) eq 0 then obase = 0L	;flag unknown base order

;Define useful quantities.
  vrfy_value = 234567891L			;flag as version 2 .isp file
  sz = size(spec)				;get variable information
  if sz(0) ne 2 then begin			;true: not a spectrum array
    message,'Spectrum is not an array.'
  endif
  ncol = long(sz(1))				;number of columns
  nord = long(sz(2))				;number of orders
  ob = long(obase) 				;conver obase to long integer
  fill = -1L					;filler for reserved long words

;Initialize total array and load with spectrum and uncertainty
  arr = fltarr(ncol,2*nord)                     ;initialize array
  arr(*,0:nord-1) = spec                        ;load spectrum
  arr(*,nord:2*nord-1) = sunc                   ;load uncertainty

;Now write the fits file
fname = file + '.isp'                           ;set the filename
if not keyword_set(head) then begin             ;no passed in header
   writefits,fname,arr                          ;write it out
endif else begin                                ;using a header
   card = 'COMMENT = Reduced spectrum:'
   addcard,head,card
   card = 'COMMENT = First '+strtrim(string(nord),2)+' rows contain spectrum.'
   addcard,head,card
   card = 'COMMENT = next '+strtrim(string(nord),2)+' rows contain uncertainties.'
   addcard,head,card
   if keyword_set(obase) then begin
      card = 'OBASE   = '+strtrim(string(obase),2)
   endif else begin
      card = 'OBASE   = 0'
   endelse
   addcard,head,card
   if n_params() gt 3 then begin                ;is there a comment?
      addcard,head,'COMMENT = '+comm            ;insert the comment
   endif
   ncard = n_elements(head)                     ;find length of current header
   for i=0,ncard-1 do begin                     ;loop through cards
      card=head(i)                              ;extract card
      eqpos=strpos(card,'=')                    ;find position of '='
      tag=strcompress(strmid(card,0,eqpos-1),/remove_all)  ; idnet. tag
      if tag eq 'NAXIS1' then begin             ;find number of columns tag
         slashpos=strpos(card,'/')              ;find the '/' in the axis size
         space=32b                              ;ASCII code for space
         pad=string(replicate(space,47))        ;make pad of spaces
         newcard=strmid(card,0,slashpos-6)+string(ncol,form='(i4)')+ $
                 '  /'+pad                      ;make new card
         head(i)=newcard                        ;load new card into header
      endif
      if tag eq 'NAXIS2' then begin             ;find number of columns tag
         slashpos=strpos(card,'/')              ;find the '/' in the axis size
         space=32b                              ;ASCII code for space
         pad=string(replicate(space,47))        ;make pad of spaces
         newcard=strmid(card,0,slashpos-6)+string(nord*2,form='(i4)')+ $
                 '  /'+pad                      ;make new card
         head(i)=newcard                        ;load new card into header
      endif
   endfor
   writefits,fname,arr,head
endelse

;;Open file for output.
;  openw,unit,file + '.isp',/get_lun		;get free unit
;
;;Write header information.
;  writeu,unit,vrfy_value			;write .isp ID to header
;  writeu,unit,ncol,nord,ob,clen			;write info to header
;  if clen gt 0 then writeu,unit,comm		;write comment to file
;  writeu,unit,fill,fill,fill,fill		;reserved for later use
;
;;Loop through orders. Scale data. Write scale factors and data to file.
;  for i=0,nord-1 do begin
;    ord =spec(*,i)				;extract one order
;    omin = min(ord,max=omax)			;get min,max values
;    bscale = (omax - omin) / 65535.0		;scale to fill I*2 range
;    bzero = 32768 * bscale + omin		;zero point shift into I*2
;    if bscale ne 0 then begin			;true: normal case
;      ord = (ord - bzero) / bscale		;map ord into I*2 range
;      ord = ord + 0.5*(ord gt 0)-0.5*(ord lt 0);prepare for truncation round
;      i2ord = fix(ord)				;round to I*2 by truncation
;    endif else i2ord = intarr(nCol)		;else: write zeros
;    writeu,unit,bscale,bzero,i2ord		;write binary to file
;  end						;loop back for next order
;
;;Close disk file. Exit.
;  free_lun,unit

  return

end
