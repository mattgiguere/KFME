function lookup,items,table,tol,items2,table2,tol2
;Finds the corresponding entries in "table" for each element in "items".
;Returns indicies such that abs(items(*)-table(lookup(items,table))) lt tol.
;If multiple entries are within the tolerance, the closest match is selected.
;If there are multiple "closest" entries, the first is selected.
;To test for items without corresponding table entries, use the following:
;  IDL> indicies = lookup(items,table,tol)
;  IDL> if min(indicies) lt 0 then print,'Some items not in table.'
;The optional second set of arguments provides a parallel search constraint.
;02-Jun-92 JAV	Create.
;03-Jun-92 JAV	Added second constraint logic.

  if n_params() lt 2 or n_params() eq 4 then begin
    print,'syntax: indicies=lookup(items,table[,tol] [,items2,table2[,tol2]])'
    retall
  endif
  if n_params() lt 3 then tol = 0.0		;assume exact match required

  nitems = n_elements(items)			;# of items
  indicies = lonarr(nitems)			;init return vector

  if n_params() le 3 then begin			;true: only one constraint

;Case of only one table constraint.
    for i=0,nitems-1 do begin			;loop thru items
      diff = min(abs(items(i)-table),imin)	;difference
      if diff le tol then begin			;true: found a match
        indicies(i) = imin			;save index into table
      endif else begin				;else: no match w/i tolerance
        indicies(i) = -1			;flag with impossible index
      endelse
    endfor

  endif else begin				;else: second constraint

;Case of two table constraints.
    if n_elements(items2) ne nitems then $
      message,'Item lists have unequal lengths.'
    if n_elements(table2) ne n_elements(table) then $
      message,'Lookup tables have unequal lengths.'
    if n_params() lt 6 then tol2 = 0.0		;assume exact match required
    for i=0,nitems-1 do begin			;loop thru items
      iw = where(abs(items(i)-table) le tol $
	and abs(items2(i)-table2) le tol2,nw)	;find all matches w/i tols
      if nw gt 0 then begin			;true: found match(es)
	ferr = abs(items(i)-table(iw))/(tol>1e-6) $
	  + abs(items2(i)-table2(iw))/(tol2>1e-6)
        dummy = min(ferr,imin)			;minimize fractional error
	indicies(i) = iw(imin)			;save index into table
      endif else begin				;else: no match w/i tolerance
	indicies(i) = -1			;flag with impossible index
      endelse
    endfor

  endelse
  return,indicies

end
