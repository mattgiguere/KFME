function str,number,length=length,format=format,trail=trail,char=char
on_error,2
n = n_elements(number)
s = strtrim(string(number,format=format),2)
if 1-keyword_set(char) then char = '0'
if n_elements(length) gt 0 then begin
    ilen = strlen(s)
    for i = 0,n-1 do begin
        nz = length-ilen[i]
        if nz gt 0 then begin
            for j=0,nz-1 do begin
                if keyword_set(trail) then s[i] = s[i]+char else $
                  s[i] = char+s[i] 
            endfor
        endif
    endfor
    endif
return,s
end
