pro add_path, dirspec, prepend=prepend
;Adds a new subdirectory to the current IDL path (in system variable !path).
;Requested subdirectories that are already in path are not added again.
; dirspec (input string) specification of subdirectories to add to !path.
;   Filename pattern matching (tilde expansion, wildcard substitution, etc.)
;   is performed, as appropriate for the shell specified in the environment
;   variable SHELL. Only directories and symbolic links that match the pattern
;   are included in !path. Note that symbolic links that match the pattern
;   will be included in !path, even if they are not subdirectories. Support
;   for VMS has not yet been implemented, but it can and should be.
; /prepend (input switch) prepends the subdirectory at the beginning of !path.
;   By default, the new subdirectories are added at the end of !path.
;22-Feb-95 JAV Create.

if n_params() lt 1 then begin
  print, 'syntax: add_path, dirspec [, /prepend]'
  retall
endif
on_error, 2						;return and stop

;First make sure the requested directory exists.
  spawn,'find ' + dirspec + ' -type d -prune -print', dirlist
  if strmid(dirlist(0),0,1) ne '/' then begin		;true: not a filename
    print,'add_path: no such directory (though there may be a symbolic link)'
    return						;nonfatal error
  endif

;Loop thru subdirectories in list, {pre|ap}pending if not already in !path.
  for idir = 0, n_elements(dirlist) - 1 do begin	;loop thru dirs
    ipos = strpos(!path, dirlist(idir))			;already in path?
    if ipos lt 0 then begin				;true: not in !path
      if keyword_set(prepend) then begin		;true: prepend
        !path = dirlist(idir) + ':' + !path		;prepend one dir
      endif else begin					;else: append
        !path = !path + ':' + dirlist(idir)		;append one dir
      endelse
    endif
  endfor

end
