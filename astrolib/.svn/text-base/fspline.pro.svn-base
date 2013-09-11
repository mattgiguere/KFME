function fspline,x,y,xn
;Fast spline routine. Originally handled via a call_external, but converted
; to internal Numerical Recipes routines in version 3.5 and later of IDL.
;12-May-92 JAV	Added syntax statement.
;27-Sep-94 JAV  Changed module called for SOLARIS compatibility
;24-Oct-94 JAV	Ported to casa machine and back to SunOS 4.1.3.
;08-Dec-94 JAV	Switched to Numerical Recipes.

if n_params() lt 3 then begin
  print,'syntax: ynew = fspline(xold, yold, xnew)'
  retall
endif

;Calculate spline using internal numerical recipes routines.
  y2 = nr_spline(x, y)				;second derivatives
  return,nr_splint(x, y, y2, xn)		;calc spline and return

;SunOS 4.1 call_external version.
; n = n_elements(x)				;length of old vector
; nn = n_elements(xn)				;length of new vector
; yn = dblarr(nn,/nozero)			;init return variable
; dummy = call_external('/users1/casa/jvalenti/lib/spline.so' $
;    ,'_spline',no,double(x),double(y),nn,double(xn),yn)
; return,float(yn)

end
