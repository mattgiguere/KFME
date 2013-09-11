function ang,font
; font (input scalar) Hershey font number to use for "A" and to wind up in.
;Returns a Hershey character sequence that produces a *perfect* Angstrom
;  symbol. Original font is restored in function call. Assumes call is
;  made while at the "normal" level and "original" character size.
;09-Jun-92 JAV	Create.

  if n_params() ne 1 then begin
    print,'syntax: string=ang(font)  [where font is 3, 5, 6, or 8]'
    retall
  endif

  case font of
    3: return,'!S!A!U!E !9 !10iiiii!20P!N!R!3A'
    5: return,'!S!A!U!E !9 !10iiiiii!20P!N!R!5A'
    6: return,'!S!A!U!E !9  !20P!N!R!6A'
    8: return,'!S!A!U!E   !10iii!20P!N!R!8A'
    else: message,'Bad font number (only 3, 5, 6, or 8 allowed)'
  endcase

end
