pro zap,sp,nsp
;  This routine allows you to zap cosmic rays in a spectrum using the
;  cursor to locate them.  When you click on a cosmic ray (lined up in the
;  x-axis, the routine will replace the value at the picked x-value with
;  the average of the pixels 2 pixels away on either side.  Position 
;  cursor below the x-axis to end.
if n_params() lt 2 then begin
   print,'Syntax is: Zap,sp,nsp'
   retall
endif
x=indgen(n_elements(sp))
nsp=sp
ist=1
while ist eq 1 do begin
   plot,nsp,ysty=3,ps=10,xsty=3
   point,x,sp,xx,yy,ip
   if yy lt !y.crange(0) then begin
      ist=0
   endif else begin
      nsp(ip)=total([nsp(ip-1),nsp(ip+1)])/2
   endelse
endwhile
end
   
