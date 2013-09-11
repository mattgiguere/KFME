pro parabcoff,pcof1,pcof2
;  This routine translates the coefficients of a parabaloid fit from the 
;  notation y=pcof1(0)+pcof1(1)*x+pcof1(2)*x^2 to the notation 
;  y=pcof2(0)*(x-pcof2(1))^2 + pcof2(2).  The primary purpose of this is
;  to determine the place of the minima or maxima (pcof2(1)) of the parabola
;  in question.  Pcof1 is the input vector, pcof2 is an output vector
;  created by the routine.
;
;  26-Apr-1995  CMJ - Written
;
if n_params() lt 2 then begin
   print,'Syntax is: ParabCoff,pcof1,pcof2'
   retall
endif

if n_elements(pcof1) ne 3 then begin           ; make sure it is a parabola
   print,'Wrong Number of elements in the input coefficient vector.'
   retall
endif

pcof2=fltarr(3)                                ; declare pcof2
pcof2(0)=pcof1(2)
pcof2(1)=-1.*pcof1(1)/2./pcof1(2)
pcof2(2)=pcof1(0)-pcof1(1)^2/4./pcof1(2)

end
