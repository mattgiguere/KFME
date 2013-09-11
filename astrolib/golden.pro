pro golden,ax,bx,cx,func,tol,xmin,fmin
;  this function evaluates the minimum of a function FUNC, to some tolerance
;  TOL.  The minimum must be between ax and cx.  bx must be between ax and cx
;  and FUNC(bx) must be between FUNC(ax) and FUNC(cx).  xmin is the abscissa
;  where the minimum occurs and the value of fmin returned is FUNC(xmin).
;  Taken from Numerical Recipes function GOLDEN in sec. 10.1.
npar=n_params()
if npar lt 6 then begin
   print,'Syntax is : Golden,ax,bx,cx,func(str),tol,xmin,fmin'
   retall
endif
r=6.1803399d-01
c=1.0d00-r
x0=ax
x3=cx
if abs(cx-bx) gt abs(bx-ax) then begin
   x1=bx
   x2=bx+c*(cx-bx)
endif else begin
   x2=bx
   x1=bx-c*(bx-ax)
endelse
f1=mindetail(x1)
f2=mindetail(x2)
while abs(x3-x0) gt tol*(abs(x1)+abs(x2)) do begin
   if f2 lt f1 then begin
      x0=x1
      x1=x2
      x2=r*x1+c*x3
      f0=f1
      f1=f2
      f2=mindetail(x2)
   endif else begin
      x3=x2
      x2=x1
      x1=r*x2+c*x0
      f3=f2
      f2=f1
      f1=mindetail(x1)
   endelse
endwhile
if f1 lt f2 then begin
   fmin=f1
   xmin=x1
endif else begin
   fmin=f2
   xmin=x2
endelse
end
