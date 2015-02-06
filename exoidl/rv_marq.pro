Function rv_marq,func, inpx, inpy, sig, par, dpar, sigpar, chi=chi, trace=trace
;+
; NAME:
;	marq
;
; PURPOSE:
;	Find parameters of a user-specified, vector-valued function
;	which give the best agreement between the function value and
;	user-supplied target vector.
;
; CATEGORY:
;	E2 - Curve and Surface Fitting.
;
; CALLING SEQUENCE:
;	fit = marq(func, x, y, sig, par, dpar, sigpar, chi=chi)
;
; INPUTS:
;	func:  name of the function which will be used to fit y.
;	x:  vector of independent variables which are fed to func.
;	y:  target vector which is be matched by func(x;par).
;	sig:  vector of weights to use in chi-squared fit. A zero
;	      indicates that the points should be ignored in the fit.
;	par:  initial guess for parameters of best fit.
;	dpar:  step sizes to use in calculating numerical partial
;	       derivatives. Entries set to zero flag parameters that
;	       are to be fixed at their input value (not optimized).
;
; OPTIONAL INPUTS:
;	trace:  if set, causes trace information to be printed.
;
; OUTPUTS:
;	par:  parameters that give agreement between y and func(x;par).
;	sigpar:  one sigma uncertainties in par.
;
; OPTIONAL OUTPUTS:
;	chi:  chi squared value of final (best) fit.
;
; PROCEDURE:
;	Adapted from IDL distribution routine "curvefit.pro"
;	Based on Marquardt method described in
;	  Bevington, P. R. & Robinson, D. K. 1992, Data Reduction
;           and Error Analysis for the Physical Sciences, 2e
;	    (New York:McGraw Hill), p. 161.
;
; MODIFICATION HISTORY:
;	Written, DMS, RSI, September, 1982.
;	Does not iterate if the first guess is good.  DMS, Oct, 1990.
;	Added CALL_PROCEDURE to make the function's name a parameter.
;		(Nov 1990)
;	12/14/92 - modified to reflect the changes in the 1991
;		   edition of Bevington (eq. II-27) (jiy-suggested by CreaSo)
;	26-Jan-95 - J. Valenti: adapted from curvefit.pro; argument list
;	  changed, partials computed numerically in this routine, rather than
;	  in function; added capability to fix parameters; etc.
;	26-Feb-95 - J. Valenti: fixed convergence test to handle chi=0; fixed
;	  bug that printed trace header even when /trace not set; allow degf
;	  to be zero (constraints=free parameters); prevent divide by zero.
;-      28-Dec-95 - G.Marcy:  Constrained the velocity amplitude, K=par(4)
;         to be positive.  Constrained eccentiricity to be: 0<e<1 .

;Print syntax if too few arguments.
if n_params() lt 6 then begin
  print,'syntax: fit = marq(func, x, y, sig, par, dpar [sigpar, chi=, trace=])'
  retall
endif

x=double(inpx)
y=double(inpy)

;Sizes.
  npar = n_elements(par)			;number of total parameters
  ny = n_elements(y)				;size of data vectors 

   if par(4) lt 0. then par(4) = abs(par(4))  ;force k>0
;   if par(2) lt 0. then par(2) = abs(par(2))  ;force e>0
   if par(2) lt 0. then par(2) = .001  ;force e>0
   if par(2) gt .99 then par(2) = 0.9          ;force e<1

;Figure out which parameters are free and which are fixed, based on dpar.
  ifree = where(dpar ne 0,nfree)		;find and count free parameters

;Convert input sig into reciprocal-square weights and count the actual number
; of constraints.
  iwhr = where(sig ne 0,ncon)			;find which points to fit
  wt = fltarr(ny)				;init weights
  wt(iwhr) = 1.0 / (sig(iwhr)^2.0)		;calculate weights

;Calculate degrees of freedom and flag too few constraints.
  degf = ncon - nfree				;degrees of freedom
  if degf lt 0 then message,'Too few data points - not enough constraints.'
  degf = degf > 1				;no div by zero, if degf=0

;Set initial value of lambda, which is a factor which selects between a pure
; gradient search  (lambda>>1) and an approx parabolic minimization (lambda<<1).
  lambda = double(0.001)				;selection factor

;Calculate indicies of elements on main daigonal of curvature matrix and
; a vector of length nfree contains all ones (used to build curvature matrix).
  idiag = indgen(nfree) * (nfree+1)		;indicies of main diagonal
  ones = replicate(1.0,nfree)			;vector of ones

;Initialize partial derivative array. Use single precision, unless target
; vector (y) is double precision in which case use double precision.
  vinfo = size(y)				;variable info block
;  prec = vinfo(vinfo(0)+1)			;precision code (5=double)
  prec = 5
  if prec eq 5 then begin			;true: double precision
    pder = dblarr(ny,nfree)			;init partial derivative array
  endif else begin				;else: use single precision
    pder = fltarr(ny,nfree)			;init partial derivative array
  endelse

;Calculate the initial fit.
  fit = call_function(func,x,par)		;calculate initial fit

;Print header for trace output, if requested.
  if keyword_set(trace) then begin
    print,'     Log       '			;print 1st header line
    print,'Iter Lam   ChiSq'			;print 2nd header line
  endif

;Begin parameter optimization loop.
  iter = 0					;init iteration count
  repeat begin					;parameter optimization loop
    iter = iter + 1				;increment iteration count

;Calculate initial value and partial derivatives of func. Note that partial
; derivative is only "one-sided". Two-sided partial are more accurate (and
; more time consuming). Note also that the fit at the current parameter
; values (used to calculate partial derivatives) is first calculated before
; entering the loop and subsequently is carried over from the bottom of the
; loop.
    for i=0,nfree-1 do begin			;loop thru free parameters
      j = ifree(i)				;actual index of varying par
      newpar = par				;copy parameters
      newpar(j) = newpar(j) + 0.5*dpar(j)		;step + in one parameter
      advfit = call_function(func,x,newpar)	;calculate new fit
      newpar(j) = newpar(j) - 0.5*dpar(j)		;step - in one parameter
      bakfit = call_function(func,x,newpar)	;calculate new fit
      pder(*,i) = (advfit - bakfit) / dpar(j)	;calculate partial derivatives
    endfor

;Now calculate solution vector (beta) and curvature matrix (alpha). Note that
; terms with wt(i)=0 do not contribute to alpha and beta.
    beta = ((y - fit) * wt) # pder		;calculate solution vector
    alpha = transpose(pder) $
          # ((wt # ones) * pder)		;calculate curvature matrix

;Calculate reduced chi-squared. (Again, points with wt(i) do not contribute.)
    chi = double(total(wt * (y - fit)^2.0) / degf)	;reduced chi-square

;Try different values of lambda (chooses between gradient search and parabolic
; minimization) until chi sqaured improves.
; added limit on iterations here GB*******
    repcnt=0
    Repeat begin				;try different lambda values
    repcnt=repcnt+1

;Normalize curvature matrix and modify diagonal to select between different
; search techniques.

      norm = sqrt(alpha(idiag) # alpha(idiag)) > 1d-19	;normalization matrix
      array = alpha / norm			;normalize (main diagonal = 1)
      array(idiag) = array(idiag) $
                   * (1.0 + lambda)		;modify main diagonal

;Invert curvature matrix (alpha) to get covariance matrix (array). Modify
      array = invert(array)			;invert to get covariances


;Solve system to get suggested changes in parameters.
      dfree = (array / norm) # transpose(beta)	;change in free parameters
      newpar = par				;copy current parameters

      newpar(ifree) = newpar(ifree) + 0.8*dfree	;set new parameters

   if newpar(4) lt 0. then newpar(4) = abs(newpar(4))  ;force k>0
   if newpar(2) lt 0. then newpar(2) = .001              ;force e>0
   if newpar(2) gt 1. then newpar(2) = 0.5             ;force e<1

;Calculate function and quality of fit at new parameter values.
      fit = call_function(func,x,newpar)	;new fit


      newchi = total(wt * (y-fit)^2.0) / degf	;new reduced chi-square

;Increase the factor which selects between approximate parabolic minimization
; and a gradient search. Increasing factor tends towards gradient search.
       lambda = 10.*lambda

;Test whether chi squared improved. If not, try the new lambda value.
    endrep until (newchi le chi or repcnt gt 40 or alog10(lambda) gt 3)

;Keep new parameter values.
    par = newpar				;keep best parameter so far
    
;Report current fit status. Prevent overflow or underflow in alog10(newchi).
    if keyword_set(trace) then begin
      if iter/5. eq fix(iter/5.) or iter eq 1 then begin
        print,form='(i4,i4,f10.4)' $
          ,iter,alog10(lambda)+0.5 $
          ,(1e37<(sqrt(newchi)>1e-37))		;print status
      end
    endif

;Decrease selection factor two steps back towards parabolic minimization.
    lambda = lambda / 100.0			;tend to parabolic minimization

;Test for convergence.
    done = 0					;clear flag
    if chi eq 0 then begin			;true: handle chi=0 case
      done = 1					;perfect fit, so done
    endif else begin				;else: test change in chi
      chich = (chi - newchi) / chi		;fractional change in chi
      if chich le 0.000001 or iter gt 40 then done = 1		;true: change in chi small
    endelse
if strtrim(string(newchi),2) eq 'NaN' then begin
  done = 1
  newchi=100.
end

;Test if chi squared is still improving significantly (0.01% change by default,
; but feel free to play with convergence criterion).
  Endrep until done				;until small change or chi=0

;Calculate final parameter uncertainties (putting zero for fixed parameters).
  chi = sqrt(newchi)			;return final value of chi
  sigpar = par					;init sigpar (fill in zeros)
  iv = where(alpha(idiag) ne 0 $
          and array(idiag) gt 0, nv)		;avoid errors in sigpar calc
  if nv gt 0 then begin				;true: some valid values
    idiagv = idiag(iv)				;valid diagonal elements
    sigpar(ifree(iv)) = sqrt(array(idiagv) $
                     / alpha(idiagv))		;calculate uncertainties
  endif

;Return best fit.
  return,fit

end







