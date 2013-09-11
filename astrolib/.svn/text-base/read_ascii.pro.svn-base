pro read_ascii, file, data
;Reads an numeric table from a disk file (with comments).

if n_params() lt 2 then begin
  print, 'syntax: read_ascii, file, data'
  retall
endif

;Program constants.
  ilen = 100						;guess 100 lines
  sep1 = (byte(' '))(0)					;byte space
  sep2 = (byte('	'))(0)				;byte tab
  cmark = ['*', ';', '#', '%']				;comment delimiters
  ncmark = n_elements(cmark)				;number of marks

;Trap IO Errors.
  on_ioerror, ioerror

;Open file for read.
  openr, unit, file, /get_lun

;Loop thru and process lines.
  line = 0
  indx = 0
  sbuff = ''
  while not eof(unit) do begin
    readf, unit, sbuff					;read line as string
    line = line + 1					;increment lines
    sbuff = strtrim(strcompress(sbuff), 2)
    if sbuff ne '' then begin				;ignore blank string
      cflag = 0						;assume not a comment
      for i=0,ncmark-1 do begin				;loop thru cmarks
        if strmid(sbuff, 0, 1) eq cmark(i) then begin
          cflag = 1					;mark comment
        endif
      endfor
      if cflag eq 0 then begin				;ignore comments
        bbuff = byte(sbuff)				;byte array equiv
        isep = where(bbuff eq sep1 $
                  or bbuff eq sep2, nsep)		;count words
        if n_elements(nw) eq 0 then begin		;true: 1st pass
          nw = nsep + 1					;store words/line
          data = dblarr(nw, ilen)			;init data array
        endif else begin
          if nsep + 1 ne nw then begin
            stop, 'readascii: line ' + strtrim(line, 2) $
                  + ', expected ' + strtrim(nw, 2) $
                  + ' words, got ' + strtrim(nsep+1, 2)
          endif
        endelse
        fbuff = dblarr(nsep + 1)			;init float buffer
        reads, sbuff, fbuff				;parse into numbers
        if indx gt ilen-1 then begin
          data = [ reform(temporary(data), nw * ilen) $
                 , replicate(0, nw * ilen) ]		;double size of data
          data = reform(data, nw, 2 * ilen)
          ilen = 2 * ilen
        endif
        data(*, indx) = fbuff				;insert data
        indx = indx + 1					;increment pointer
      endif
    endif
  endwhile 

;Trim unused portions of data.
  data = data(*, 0:indx-1)

  free_lun, unit
  return

ioerror:
  stop, !err_string

end
