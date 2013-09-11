pro excise, ar,el,out=ot

if n_params() lt 2 then begin
	print,'EXCISE, ar,el,out=ot'   
return
endif
;remove element with subscript el from ar
     if keyword_set(ot) then print,ar
     lst = n_elements(ar) - 1     ;last valid subscript in ar
     if el eq 0 then ar = ar(1:lst) else $     ;el is the first element of ar
          if el eq lst then ar = ar(0:lst-1) else $;el is the last element of ar
             ar = [ar(0:el-1),ar(el+1:lst)]     ;otherwise 
     if keyword_set(ot) then print,ar
return
end

