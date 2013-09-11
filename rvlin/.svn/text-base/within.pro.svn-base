function within, array, inmin, inmax, lims = lims
  nc = n_elements(inmin)
  if nc eq 1 then begin
    if inmin gt inmax then begin
      min = inmax
      max = inmin
    endif else begin
      min = inmin
      max = inmax
    endelse
  endif 

  na = n_elements(array)

  if nc eq 1 then begin

    if n_elements(lims) eq 0 then lims = 0
    
                                ;KEY:  lims = 0  => []
                                ;      lims = 1  => (]
                                ;      lims = 2  => [)
                                ;      lims = 3  => ()

    if lims mod 2 eq 1 then cond1 = array gt min else cond1 = array ge min
    if lims  /  2 eq 1 then cond2 = array lt max else cond2 = array le max
    
    return, cond1 and cond2
    
  endif else begin   ;min must be sorted
    min = inmin
    
;    usage:

;
;  arr=[0,2,2,4,5]
;  lims=[0,4,5]                   ;bins
;  inds=within(arr,lims)
;  inds2=within(arr,lims,lims=[2,2])
;  print, arr[where(inds[0,*])]   ;bin 1 is half-open interval by default
;  print, arr[where(inds[1,*])]   ;bin 2 is closed because it's the
;  last one
;  print, arr[where(inds2[1,*])]  ;we can force bin 2 to be half-open,
;  if we choose


    one = fltarr(nc-1)+1
    onea = fltarr(na)+1
    if n_elements(lims) ne nc then begin  
      if n_elements(lims) eq 0 then begin ; defaults to [) [) [) [) [] 
        lims = bytarr(nc-1)+2
        lims[nc-2] = 0
      endif
      limarr = lims#onea
    endif
    
    arrfan = one#array
    arrfan = arrfan 

    eqs3 = byte(lims#onea) mod 2 eq 0
    eqs4 = byte(lims#onea)  /  2 eq 0

    cond1 = arrfan gt min[0:nc-2]#onea
    cond2 = arrfan lt min[1:*]#onea
    cond3 = (arrfan eq min[0:nc-2]#onea) and eqs3
    cond4 = (arrfan eq min[1:*]#onea) and eqs4

    return, (cond1 or cond3) and (cond2 or cond4)

  endelse

end
