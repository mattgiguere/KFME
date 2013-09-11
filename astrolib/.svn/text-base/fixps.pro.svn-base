pro fixps, infile, outfile, debug=debug
;Fix those damn equal signs that sometimes appear in emailed postscript.
;Input:
; infile (string) name of existing pseudo-postscript file
; /debug (switch) print before and after versions of every modified line.
;   After each pair of lines is printed, the following options exist:
;     'q' - quits fixps, leaving incomplete output file
;     'c' - leaves debug mode, but continues processing
;     any other keypress will continue stepping through line pairs
;Output:
; outfile (string) name to use when creating true postscript file
;History:
; 16-Aug-1998 Valenti  Wrote.

if n_params() lt 2 then begin
  print, 'syntax: fixps, infile, outfile [,/debug]'
  retall
endif

  elist = ['=3D', '=2E']
  nelist = n_elements(elist)

  openr, inunit, infile, /get
  openw, outunit, outfile, /get

  ecnt = lonarr(nelist)
  eolcnt = 0L

  buff = ''
  buff2 = ''
  while not eof(inunit) do begin

;Get from input file.
    readf, inunit, buff

;Handle embedded '=3D' sequences.
    for ie=0, nelist-1 do begin
      estr = elist(ie)
      while strpos(buff, estr) ge 0 do begin
        ipos = strpos(buff, estr)
        if ipos eq 0 then begin
          bnew = ''
        endif else begin
          bnew = strmid(buff, 0, ipos)
        endelse
        if ipos lt strlen(buff)-1 then begin
          bnew = bnew + strmid(buff, ipos+strlen(estr), 999)
        endif
        if keyword_set(debug) then begin
          print, buff
          print, bnew
          char = get_kbrd(1)
          print, ''
          if char eq 'c' then begin
            print, 'leaving debug mode'
            debug = 0
          endif
          if char eq 'q' then begin
            print, 'aborting - ' + outfile + ' is incomplete!'
            free_lun, inunit
            free_lun, outunit
            return
          endif
        endif
        buff = bnew
        ecnt(ie) = ecnt(ie) + 1
      endwhile
    endfor

;Handle end of line '=' marks.
    if strmid(buff, strlen(buff)-1, 1) eq '=' then begin
      readf, inunit, buff2
      bnew = strmid(buff, 0, strlen(buff)-1) + buff2
      if keyword_set(debug) then begin
        print, buff
        print, bnew
        char = get_kbrd(1)
        print, ''
        if char eq 'c' then begin
          print, 'leaving debug mode'
          debug = 0
        endif
        if char eq 'q' then begin
          print, 'aborting - ' + outfile + ' is incomplete!'
          free_lun, inunit
          free_lun, outunit
          return
        endif
      endif
      buff = bnew
      eolcnt = eolcnt + 1
    endif

;Write to output file.
    printf, outunit, buff

  endwhile

  free_lun, inunit
  free_lun, outunit

;Print statistics.
  print, 'Replacement counts:'
  for ie=0, nelist-1 do begin
    print, string(ecnt(ie), form='(i8)') $
         + '  "' + elist(ie) + '" --> ""' $
         + ' replacements'
  endfor
  print, string(eolcnt, form='(i8)') + '  "=" at end of line closures'

end
