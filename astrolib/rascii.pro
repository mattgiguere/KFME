pro rascii,vname,ncol,fname,skip=skip
;skip is the number of lines at the top of the file that are to be skiped
    fud='?'  &  crow=-1
	openr,1,fname
    if n_elements(skip) eq 1 then for n=1,skip do readf,1,fud
	vec=fltarr(ncol)  &  vname=fltarr(ncol,long(1.E5/ncol))
	while (eof(1) eq 0) do begin
	   readf,1,vec  &  crow=crow+1  &  vname(*,crow)=vec
    end  ;while
	close,1
	vname=vname(*,0:crow)
return
end
