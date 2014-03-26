;+
; NAME: 
;      RVLIN
;
;
; PURPOSE:
;      Solve for the linear parameters omega, K, gamma, & dv/dt for a 
;		system of multiple planets give P, t_p, and e for each planet, 
;		and a set of RV data. 
;
;
; CATEGORY:
;      Curve fitting 
;
;
; CALLING SEQUENCE:
;  residuals = rvlin (pars, dp, time = time, velocity = vel, error = err, 
;		pars = parset, trend = trend, yfit = model, telvec=telvec, 
;		epoch = epoch, offset=offset) 
;
;
; INPUTS:
;     pars: vector of nonlinear parameters, 3 per planet:  P, (tp-t0), 
;		e*1000 (where tp is time of periastron passage, and e is the eccentricity)
;    time: times of observations
;    velocity: RV measurements
;    error: uncertainties of the RV measurements
;
; OPTIONAL INPUTS:
;   
;    telvec: array of flags indicating telescope of origin for time, 
;		velocity, error
;    epoch: user-specified epoch, t0.  Defaults to 14000.  Set to 0 
;		for custom call to RVLIN.  RV_MP uses default value.
;
; KEYWORD PARAMETERS:
;
;    trend: force RVLIN to fit for a linear trend 
;    twotel: force RVLIN to fit for an offset between data from two 
;		telescopes
;
; OUTPUTS:
;    residuals: vector of (velocities - model) / error, as required 
;		for MPFIT.  Has same length as [time,time2].
;
; OPTIONAL OUTPUTS:
;
;    pars: vector of best-fit values for linear parameters: [h0,c0,h1,
;			c1,...v0,d,offset]
;    yfit: model output evaluated at times given.  Has same length 
;				as [time,time2].
;    dp: derivative matrix required by MPFIT for explicit derivatives
;    offset: array of offsets between the various telescopes
;
; DEPENDENCES:
;     calls KEPLER.pro, RESTRICT.pro
;
; SIDE EFFECTS:
;     dp is filled.
;
;
; EXAMPLE:
;      See RV_MP for calling example.
;
;
; MODIFICATION HISTORY:
; v1.0 Jason Wright & Andrew Howard; Aug 2008
; 
;-

function calcnu, P, tp, e, t

;Calculates the true anomoly from the time and orbital elements.
;P = period
;tp = time of periastron passage
;e = eccentricity
;t = time
  
;In Wright & Howard, nu is denoted by little 'f'.  Here we must distinguish from big F, a matrix, so we use nu

  phase = (t-tp)/P
  M = 2d*!dpi*((phase) - floor(phase))
  E1 = kepler(M, e)

  n1 = 1d + e
  n2 = 1d - e
  nu = 2d*atan(sqrt(n1/n2)*tan(E1/2d))
  
  return, nu

end

function rvlingrad, t, pars, beta, F, vW, WFT, epsilon, requested, epoch = epoch

;calcualtes explicit derivatives in a manner rvlin expects for mpfit

;  t: times of obvervation
;  pars: nonlinear parameters P, tp-t0, e*1000, for each planet
;  beta: linear parameters h,c for each planet; v0, d
;  F: matrix of [sin(nu), cos(nu), 1, t-t0] (sin & cos for each planet)  
;  vW: array of v/err^2
;  WFT: transpose of F with each element divided by corresponding err^2
;  epsilon: matrix such that beta = v##WFT##epsilon -- solution to linear least
;                                        squares problem
;  requested: array specifying which parameters to calculate
;  derivatives for (fixed parameters should be set to 0 -- not
;  calculating them saves matrix multiplications)
;
;  trend: true if fit includes free linear trend

  np = n_elements(pars)/3 ;number of planets
  nt = n_elements(t)      ;number of times

  t0 = 14000d             ;conveniently chosen epoch of observations
  if n_elements(epoch) eq 1 then t0 = epoch  ;user override

  dp = make_array(nt, 3*np, value = t[0]*0d) ;for output for MPFIT

  dFdpar = F*0d      ;derivative of F w.r.t. a given parameter

  Ft = transpose(F)
  Ftepsilon = Ft##epsilon  ;precomputation for speed later.

  for j = 0, np-1 do begin      ;for each planet...

    if total(requested[j*3:j*3+2]) gt 0 then begin    ;if we need to calculate a given derivative...

      P =  pars[3*j]         ;Period of this planet
      tp = pars[3*j+1]+t0    ;put these back to their real values 
      e = pars[3*j+2]/1000d  ;eccentricity of this planet
                                ;we need the actual values for these parameters, not the "normalized" versions.
      
      phase = (t-tp)/P
      M = 2d*!dpi*(phase-floor(phase)) ;Mean anomaly, restricted to [0,2*!pi)
      E1 = kepler(M, e)                ;E1 is the Eccentric anomoly -- E in Wright & Howard

      sE1 = sin(E1)                    ;Precomputations for speed
      cE1 = cos(E1)
      onemecosE1 = 1d - e*cE1           
      s1e1e = sqrt((1d + e)/(1d - e))

      nu = 2d*atan(s1e1e*tan(E1/2d))  ;true anomoly == f in Wright & Howard
      sn = sin(nu)                    
      cn = cos(nu)

      dnudE1 = s1e1e*(cn+1d)/(cE1+1d)  ;d(nu)/dE1 

      for x = 0, 2 do begin   ;for each nonlinear parameter...
        if requested[j*3+x] then begin  ;if we need to calculate a given derivative...
          
;          dE1/dpar,  where par =  {P,  e,  tp}:
          if x eq 0 then dE1dpar = -2d*!dpi/P/onemecosE1*phase
          if x eq 1 then dE1dpar = -2d*!dpi/P/onemecosE1
          if x eq 2 then dE1dpar = sE1/onemecosE1
         

          dnudpar = dnudE1*(dE1dpar+   sE1/(1d - e^2)*(x eq 2))
          ;if x eq 2 then we need the second term here.

          dFdpar = dFdpar*0
          ;gotta clear dFdpar from previous iterations

          dFdpar[*, j*2:j*2+1] = [[-sn*dnudpar], [cn*dnudpar]]
                                ;fill only the rows for this planet --
                                ;all other derivatives are 0

          N = dFdpar##WFT       ;for clarity below

          dadpar = vW##(transpose(dFdpar)-Ftepsilon##(N+transpose(N)))##epsilon
                                ;chain rule.  beta = vW##transpose(F)##epsilon 
                                ;by definition of linear least squares
                                ;minimization. 
                                ;Note that epsilon=invert(F##WFT), so 
                                ;depsilon/dx = -epsilon##(d/dx(F##WFT))##epsilon


          dvdpar = beta##dFdpar+dadpar##F  ;chain rule. v = beta##F by model definition
          if x eq 2 then dvdpar = dvdpar/1000d  ;put things back to e*1000
          dp[*, j*3+x] = dvdpar           
        endif
      endfor
    endif
  endfor

  return, dp

end


function rvlin, pars, dp, time = time, velocity = vel, error = err, pars = parset, trend = trend, yfit = model, telvec = telvec, offset = offset, epoch = epoch

  ; model:
  ; u = h*Cos nu + c*Sin nu + v0 + d*(t-t0)
  ; trend: flag indicating that d (dv/dt) should be solved for 

  ; parset: returns parametrs in the format [P, t0, e, om, K, gamma,
  ; dvdt].  For np planets, returns np*7 parameters.  gamma and dvdt are
  ; zero for planets 2 through np.
  
  ; yfit: returns model evaluted at times "time".  If twotel is set, then at times [time,tim2]
  

  ; twotel: if data from two telescopes needs to be merged, this flag
  ; should be thrown and the tim2, vel2, err2 keywords should be used
  ; to supply the second data set.  The offset will be solved for
  ; linearly and the result returned in the keyword "offset".  This
  ; represents the number to be subtracted from vel2 to put the two
  ; data sets on the same scale.


  ;Jason Wright & Andrew Howard 
  ;March 23, 2008

  np = n_elements(pars)/3  ;Number of planets

  t0 = 14000d ;conveniently chosen epoch of observations
  if n_elements(epoch) eq 1 then t0 = epoch  ;user override

  p = double(pars[3*indgen(np)]) ;Periods
  tp_in = double(pars[1+3*indgen(np)])
  e_in = double(pars[2+3*indgen(np)])
  tp = tp_in+t0   ;we need to "un normalize" these parameters which have been adjusted to have similar magnitudes
  e = e_in/1000d 

  nt = n_elements(time)

  utel = telvec[uniq(telvec, sort(telvec))]
  ntels = n_elements(utel)

  t = double(time)+t0;"un normalize" the times of observation

  error = double(err)
  v = double(vel)
  

  for j = 0, np-1 do begin

    nu = calcnu(P[j], tp[j], e[j], t)  ;true anomoly (called little f in Wright & Howard)

    sn = sin(nu) 
    cn = cos(nu)

    if j eq 0 then  F = [[cn], [sn]] else F = [[F], [cn], [sn]]

  endfor


  ;We need to add the final linear parameters, if needed:

  F = [[F], [replicate(1d, nt)]]   ;gamma
  if keyword_set(trend) then   F = [[F], [t-t0]] ;dvdt
  for i = 1, ntels-1 do begin
    offsetarr = intarr(nt)
    offsetarr[where(telvec eq utel[i])] = 1
    F = [[F], [offsetarr]]      ;offset for telescope i
  endfor

  ; u = beta##F (definition of model, see above)

  W = dblarr(nt, nt)
  for i = 0, nt-1 do W[i, i] = 1d/error[i]^2  ;diagonal matrix of weights

  vW = v/error^2  ;precompute for clarity and speed below
  Ft = transpose(F)
  WFT = W##Ft     
  epsilon = invert(F##WFT, /double) ;This is the "curvature matrix" or "variance covariance matrix" 

  beta = vW##Ft##epsilon   ;solution to linear least-squares problem, a in Bevington

  u = beta##F       ;definition of model (see above)

  K = dblarr(np)
  omega = dblarr(np)
  v0_index = np*2     ;vector beta has length 2*np+1(+trend)(+offset)

  dtor = 2d*!dpi/360d

  for i = 0, np-1 do begin
    K[i] = sqrt(beta[0+i*2]^2+beta[1+i*2]^2)   ;definition of K for this planet
    omega[i] = restrict(atan(-beta[1+i*2], beta[0+i*2])/dtor, 0d, 360d) ;def. of om.
  endfor
  
  cosom = cos(omega*dtor)

  if ntels gt 1 then offset = beta[np*2+keyword_set(trend)+1:*]  ;Subtract this from vel2 to put on same scale as vel

  gamma = beta[v0_index]-total(K*e*cosom)  ;definition of gamma

  parset = dblarr(np*7)
  for j = 0, np-1 do begin
    parset[j*7:j*7+6] = [P[j], tp[j], e[j], omega[j], K[j], 0, 0]
  endfor

  parset[5] = gamma
  if keyword_set(trend) then begin
    parset[6] = beta[v0_index+1]
  endif

  if n_params() GT 1 then begin
                                ; Create derivative and compute derivative array
    requested = dp              ; Save original value of DP
    dp = make_array(n_elements(t), n_elements(pars), value = t[0]*0d)
                                ; Compute derivative if requested by caller
    if total(requested) gt 0 then begin
      dp = RVLINGRAD(t, pars, beta, F, vW, WFT, epsilon, requested, epoch = epoch) / (error#(1+dblarr(n_elements(pars))))
    endif

  endif

  model = u

  return, (v-u)/error

end

