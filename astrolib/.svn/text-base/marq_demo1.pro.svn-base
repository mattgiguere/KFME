pro marq_demo
;Illustrate the use of marq, using the mgauss routine.
;See mgauss.pro for an explanation of the function parameters.

;Build true function value.
  nx = 201
  x = 8.0*findgen(nx)/(nx-1) - 4.0		;uniform scale from -4 to +4
  p = [0.1,0.01,0.0, 1.0,0.0,1.0, 0.2,0.2,0.5]	;"true" parameters
  y = mgauss(x,p)				;"true" function value

;Construct "observation" by adding noise with sigma of 0.01 to truth.
  noise = 0.05					;gausian noise
  obs = y + noise * randomn(seed,nx)		;make "observation"

;Let's also add a couple of nongaussian noise spikes.
  obs(nx/2+10) = 0.0				;drop out
  obs(10) = max(y)				;cosmic ray

;Construct a vector of uncertainties. We think the noise is 0.01.
  sig = replicate(noise,nx)			;construct sigma vector

;Mask out the pixel we don't want to fit by setting sig=0.
  sig(nx/2+10) = 0.0				;mask drop out
  sig(10) = 0.0					;mask cosmic ray

;Set initial guess for parameters. Note that the initial guess is poor.
;  Also, the centers of the gaussians are offset to distinguish them.
;  ipar = float([1,1,0, 1,-1,1, 1,1,1])		;initial parameter guess
  ipar = float([1.1,0.01,0.0, 1.0,0.0,1.0, 0.2,0.2,0.5])		;initial parameter guess

;Set step sizes for partial derivative calculation. Note that by setting
;  dpar(2)=0, we are fixing par(2) at its initial value of 0.
  dpar = replicate(0.00,9)			;start with all dpar at 0.01
  dpar(0) = 0.1					;fix parameter 2.

;Fit the observation.
  par = ipar					;save ipar for print later
  mdl = marq('mgauss',x,obs,sig $
           ,par,dpar,sigpar $
           ,chi=chi,/trace)			;find best fit

;Compare original and final parameters.
  print,'Chi-squared =',strcompress(chi)	;we know final chi-square
  print,'Init parameters =',ipar,form='(a,9f7.3)'
  print,'True parameters =',p,form='(a,9f7.3)'
  print,'Fit parameters  =',par,form='(a,9f7.3)'
  print,'Formal Error    =',sigpar,form='(a,9f7.3)'
  print,'Note that the identity of the two gaussians is sometimes switched.'

;Compare true, observed, and fit functions.
  plot,x,y,ps=3					;plot "true" function
  oplot,x,obs,ps=10,thick=0.2			;plot observation
  oplot,x,mdl,thick=2,line=2			;plot observation
  oplot,x,mgauss(x,ipar),ps=2,symsiz=0.5	;plot initial guess

end
