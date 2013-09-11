function mgauss,x,par
;Calculate sum of multiple gaussians plus a quadratic baseline.
; x (input vector) independent variable for function.
; par (input vector) parameters controlling baseline and gaussians.
;   par(0:2) specify baseline:  par(0) + x*par(1) + x*x*par(2)
;   par(3:5) specify 1st gaussian:  par(3) * exp(-0.5*((x-par(4))/par(5))^2)
;            i.e. par(3)=amplitude, par(4)=center, par(5)=sigma
;   par(6:8) specify 2nd gaussian:  same as first gaussian, but with
;            3 --> 6,  4 --> 7,  5 --> 8.
;   etc.
;Any number of gaussians (including zero) may be specified. The routine
; uses the number of elements in par to decide how many gaussians to use.
;This function is designed to be called from cfit.pro.

if n_params() lt 2 then begin
  print,'result = mgauss(x,par)'
  print,'  par(0:2) specify quadratic baseline [constant,linear,quadratic]'
  print,'  par(3*i:3*i+2) i>0, specify gaussian [amplitude,center,sigma]'
  retall
endif

;Start with baseline.
  result = par(0) + x*(par(1) + x*par(2))	;quadratic baseline

;Crash if parameter count is wrong.
  npar = n_elements(par)			;number of parameters
  if npar mod 3 ne 0 then $			;true: bad parameter count
    message,'parameter vector has bad length - check instructions.'

;Loop through gaussians. We only calculate the gaussian where the argument
; to the exponent will be safely between the single precision underflow and
; overflow limits. This prevent those annoying underflow errors.
  ngauss = (npar - 3) / 3			;number of gaussians
  for igauss = 1,ngauss do begin		;loop thru gaussians
    p = par(3*igauss:3*igauss+2)		;extract current parameters
    exponent = -0.5*((x - p(1)) / p(2))^2.0	;precalculate exponent
    iwhr = where(exponent ge -37,nwhr)		;single precision filter
    if nwhr gt 0 then begin			;true: some significant points
      result(iwhr) = result(iwhr) + p(0) $		
           * exp(exponent(iwhr))		;add in this gaussian
    endif
  endfor

  return,result

end
