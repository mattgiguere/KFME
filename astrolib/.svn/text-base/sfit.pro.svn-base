function sfit, z, degree, kx=kx
;+
; NAME:
;	SFIT
;
; PURPOSE:
;	This function determines a polynomial fit to a surface.
;
; CATEGORY:
;	Curve and surface fitting.
;
; CALLING SEQUENCE:
;	Result = SFIT(Data, Degree)
;
; INPUTS:
; 	Data:	The two-dimensional array of data to fit. The sizes of
;		the dimensions may be unequal.
;
;	Degree:	The maximum degree of fit (in one dimension).
;
; OUTPUT:
;	This function returns a fitted array.
;
; OUTPUT KEYWORDS:
;	Kx:	The array of coefficients for a polynomial function
;		of x and y to fit data.
;		This parameter is returned as a (Degree+1) by (Degree+1) 
;		element array.
;
; PROCEDURE:
; 	Fit a 2D array Z as a polynomial function of x and y.
; 	The function fitted is:
;  	    F(x,y) = Sum over i and j of kx(j,i) * x^i * y^j
; 	where kx is returned as a keyword.
;
; MODIFICATION HISTORY:
;	July, 1993, DMS		Initial creation
;	Dec 1995, Valenti	Modify to use less memory.
;
;-

   on_error, 2

   s = size(z)
   nx = s(1)
   ny = s(2)
   m = nx * ny		;# of points to fit
   n2=(degree+1)^2		;# of coefficients to solve
   x = findgen(nx) # replicate(1., ny)   ;X values at each point
   x = reform(temporary(x), 1, m)
   y = replicate(1.,nx) # findgen(ny)
   y = reform(temporary(y), 1, m)

   ut = dblarr(n2, m, /nozero)
   for i=0, degree do begin
     for j=0,degree do begin
       ut(i*(degree+1) + j, 0) = x^i * y^j
     endfor
   endfor
   junk = size(temporary(x))
   junk = size(temporary(y))

   kk = invert(ut # transpose(ut))
   kk = kk # ut
   kx = fltarr(degree+1, degree+1) $
      + float(temporary(kk) # reform(z, m, 1))
   return, reform(reform(kx,n2) # temporary(ut), nx, ny)
end


