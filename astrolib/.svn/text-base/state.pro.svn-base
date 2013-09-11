pro state,T,Pe,Pg,ionf=ionf
;Calculates LTE ionization equilibrium. See Mihalas 1967, Methods in
;  Computational Physics, 7, 1.
; T (input scalar or vector) temperature(s) to consider.
; Pe (input scalar or vector) electron pressure(s) corresponding to each T.
; Pg (output scalar or vector) gas pressure for corresponding T, Pe.
;
; reproduces n_e from AH NG models +/-2% @ 3250 K
;                                  +/-2% @ 4000 K
;                                  +/-2% @ 5000 K (except 2 deepest pts,+/-5%)
;
;22-Jul-94 JAV	Create.
;08-May-99 SHS  include Sauval & Tatum partition functs, AH abundances

if n_params() lt 3 then begin
  print,'syntax: state,T,Pe,Pg'
  retall
endif

;Abundance data for  He, C, N, O,  Na, Mg, Al, Si,  S, K, Ca, Cr,  Fe.
;Mihalis (1967):
; alpha = [1.5d-1, 5.3d-4, 9.6d-5, 9.1d-4 $
;  ,2.0d-6, 2.6d-5, 1.6d-6, 3.2d-5 $
;  ,2.0d-5, 5.0d-8, 1.4d-6, 7.8d-7 $
;  ,3.7d-6 ]
;Fortran "scale" routine:
; alpha = [1.00d-1, 3.55d-4, 5.90d-4, 1.51d-6 $
; 	  ,3.02d-5, 2.51d-6, 3.55d-5, 1.62d-5 $
; 	  ,3.16d-5]
;Holweger-Muller (1974):  [He, Mg, Si, Fe]
; alpha = [1.0d-1, 3.981d-5, 4.467d-5, 3.981d-5]
;Gray (1992), p. 318: [elements as in Mihalas]
;  alpha = [7.94d-2, 4.57d-4, 9.55d-5, 8.13d-4 $
; 	  ,2.04d-6, 4.17d-5, 3.24d-6, 4.37d-5 $
; 	  ,1.70d-5, 1.41d-7, 2.19d-6, 5.75d-7 $
; 	  ,3.89d-5 ]
;
;  AH values
;;  main missing e- contributors: Ni Mn Ti

 alpha=[8.8907E-02,3.2280E-04,8.4905E-05,6.7443E-04,1.9451E-06, $
          3.4589E-05,2.6849E-06, 3.2280E-05,1.4755E-05,1.1993E-07, $
          2.0842E-06,4.2554E-07,2.8770E-05]/9.0978E-01

  nA = n_elements(alpha)				;# different atoms

;Ionization potentials for elements (as above).
  nI = 2
  chi = dblarr(nI,nA)					;ion potentials (eV)
;Mihalas (1967):
  chi(0,*) =  [24.581d0, 11.256d0, 14.530d0, 13.614d0 $
  	      , 5.138d0,  7.644d0,  5.984d0,  8.149d0 $
  	      ,10.357d0,  4.339d0,  6.111d0,  6.764d0 $
  	      , 7.87d0 ]				;1st ionization (eV)
  chi(1,*) =  [54.403d0, 24.376d0, 29.593d0, 35.108d0 $
  	      ,47.290d0, 15.031d0, 18.823d0, 16.34d0  $
  	      ,23.400d0, 31.81d0,  11.868d0, 16.49d0  $
  	      ,16.18d0 ]				;2nd ionization (eV)
;Fortran "Scale" routine:
; chi(0,*) = [24.581, 11.256, 13.614, 5.138, 7.644, 5.984, 8.149, 10.357, 7.83]
; chi(1,*) = replicate(40.0,nA)
;Holweger-Muller (1974):
; chi(0,*) = [24.581, 7.644, 8.149, 7.87]
; chi(1,*) = replicate(40.0,nA)

;Twice the partition function ratio for elements (as above).
;  log2Urat = dblarr(nI,nA)				;partition func ratios
;Mihalas (1974):
;  log2Urat(0,*) = [ 0.60d0, 0.10d0, 0.62d0,-0.05d0 $
;  	          , 0.00d0, 0.60d0,-0.48d0, 0.12d0 $
;  	          ,-0.05d0, 0.00d0, 0.60d0,-0.07d0 $
;  	          , 0.38d0 ]				;part fcn ratio 2U1/U0
;Fortran "Scale" routine:
; log2Urat(0,*) = [ 0.602, 0.110,-0.004,-0.010, 0.600 $
; 		  ,-0.470, 0.080, 0.000, 0.491]
; log2Urat(1,*) = replicate(0.0,nA)
;Holweger-Muller (1974):
; log2Urat(0,*) = [ 0.60, 0.60, 0.12, 0.38]
; log2Urat(1,*) = replicate(0.0,nA)

;Mihalas (1974):     (retained for part fcn ratio 2U2/U1)

  log2Urat1 = [ 0.00d0,-0.48d0, 0.12d0, 0.65d0 $
  	          , 1.08d0, 0.00d0, 0.60d0,-0.48d0 $
  	          , 0.65d0, 1.08d0, 0.00d0, 0.92d0 $
  	          , 0.22d0 ]				;part fcn ratio 2U2/U1

; Sauval & Tatum (1984)  poly coeffs to compute U0, U1

  cU0 = [[0.d0,0,0,0,0],[.96752,-0.09452,0.08055,0,0], $                ;He C
        [0.60683,-0.08674,.30565,-0.28114,0],[0.95033,-0.05703,0,0,0], $ ; N O
         [0.30955,-0.17778, 1.10594, -2.42847, 1.70721], $   ; Na
         [0.00556,-0.1284,.81506,-1.79635,1.26292], $   ; Mg
 [0.76786,-0.05207,0.14713,-0.21376,0],[0.97896,-0.19208,0.04753,0,0], $ ;Al Si
         [0.95254,-0.15166,0.0234,0,0], $   ; S
         [0.34419,-0.48157,1.92563,-3.17826,1.83211], $   ; K
         [0.0746,-0.75759,2.58494,-3.5317,1.6524], $   ; Ca
         [1.02332,-1.0254,2.02181,-1.32723,0], $   ; Cr
         [1.44701,-0.6704,1.01267,-0.81428,0]]    ; Fe

  cU1 = [[.30103d0,0,0,0,0],[.77239,-0.0254,0,0,0], $     ; He C
       [.94968,-0.06463,-0.01291,0,0],[0.60405,-0.03025,0.04525,0,0], $  ; N O
         [0,0,0,0,0.], [0.30257,-0.00451,0,0,0], $    ;  Na Mg
         [0.00334,-0.00995,0,0,0],[0.75647,-0.0549,-0.10126,0,0], $ ; Al Si
         [0.61971,-0.17465,0.48283,-0.39157,0], [0,0,0,0,0.], $  ;  S K
         [0.34383,-0.41472,1.0155,0.3193,0], $   ; Ca
         [0.85381,-0.71166,2.18621,-0.9759,-2.72893], $   ; Cr
         [1.63506,-0.47118,0.57918,-0.12293,0]]    ; Fe

;Sizes.
  nT = n_elements(T)					;# temperature points

;Useful internal variables.
  theta = 5039.7473d0 / T				;log10(e)/kboltz (K/eV)
  logPe = alog10(Pe)					;log electron pressure

logu0=fltarr(nA,nT)                                 ; setup part fct arrays
logu1=logu0
for i=0,na-1 do begin
   logu0(i,*)=poly(alog10(theta),cu0(*,i))         ; loop on poly coeffs
   logu1(i,*)=poly(alog10(theta),cu1(*,i))
endfor 

;Twice the partition function ratio for elements (as above).  2*U1/U0
log2Urat=alog10(2.*10.^logu1/10.^logu0)


;Calculate atomic ionization using Saha equation. The constant in front is
;  1.5*alog10(2*!dpi*melec) + 2.5*alog10(kboltz) - 3*alog10(hplanck)  [in cgs].
  vecT = replicate(1d0,nT)				;to build outer prod.
  vecA = replicate(1d0,nA)				;to build outer prod.
  logphi = dblarr(nI,nA,nT)
  logphi(0,*,*) = -0.47705013d0 $
	        + vecA # [2.5d0*alog10(T) - logPe] $
	        + log2Urat $
	        - reform(chi(0,*)) # [theta]		;Saha equation
  logphi(1,*,*) = -0.47705013d0 $
	        + vecA # [2.5d0*alog10(T) - logPe] $
	        + log2Urat1 # vecT $
	        - reform(chi(1,*)) # [theta]		;Saha equation

;Compute ionization fractions (relative to ground state).
  lognorm = alog10(1d0 + 10d0^reform(logphi(0,*,*)) $
	               + 10d0^total(logphi,1))		;ionf normalization
  ionf  = dblarr(nI,nA,nT)				;ionization fraction
  ionf(0,*,*) = 10d0 ^ (logphi(0,*,*) - lognorm)
  ionf(1,*,*) = 10d0 ^ (total(logphi,1) - lognorm)

;print,1/reform(10^lognorm(12,*)),form='(6f12.8)'
;print,reform(ionf(0,12,*)),form='(6f12.8)'
;print,reform(ionf(1,12,*)),form='(6f12.8)'

;Compute Q, the ratio of the electron pressure to the partial pressure of all
;  atoms and ions (but not electrons), i.e. Q = Pe/Patom, where Pg = Pe+Patom.
;  Expression only valid for nI=2.
  Q = total((alpha#vecT) * reform(ionf(0,*,*) + 2*ionf(1,*,*)),1)

;Compute dissociation constants (K's) for molecules involving hydrogen, i.e.
;  i.e. K such that partial pressures satisfy p_A * p_B = K_AB * p_AB.
  logKH2  = 12.533505d0 + theta*(-4.9251644d0     $
		        + theta*(+0.056191273d0   $
		        + theta*(-0.0032687661d0)))	;molecular hydrogen
  logKH2p = 11.206998d0 + theta*(-2.7942767d0     $
		        + theta*(-0.079196803d0   $
		        + theta*(+0.024790744d0)))	;molecular hydrogen ion
  logKHm =  0.1249d0 -  0.747d0*theta + 2.5*alog10(T)	;H-minus ion
  logKH  = -0.4772d0 - 13.595d0*theta + 2.5*alog10(T)	;neutral H atom

;Calculate G's used in algebra (see Mihalas reference).
  logG2 = -logPe + logKH
  logG2 = -logPe + logKH
  logG3 =  logPe - logKHm
  logG4 =  logPe - logKH2p
  logG5 =  logPe - logKH2
  G2 = 10d0 ^ logG2
  G3 = 10d0 ^ logG3

;Calculate a,b,c,d,e used in algebra (see Mihalas reference).
  e = 10d0^(logG2+logG4-logG5)
  d = G2 - G3
  c = 10d0^logG5
  b = 2d0 * (1d0 + e)
  a = 1d0 + G2 + G3

;Calculate quadratic coefficients c1, c2, c3 (see Mihalis reference).
  c1 = b*(b*c + a*d) - a*a*e
  c2 = 2d0*a*e - b*d + a*b*Q
  c3 = -(e + b*Q)

;Calculate the f's.
  f1 = (-c2 + sqrt(c2*c2 - 4d0*c1*c3)) / (2d0 * c1)
  f2 = G2 * f1
  f3 = G3 * f1
  f5 = (1d0 - a*f1) / b
  f4 = e * f5

;Calculate ratio of gas to electron pressure (Pg_Pe).
  Pg_Pe = 1d0 + (f1 + f2 + f3 + f4 + f5 + total(alpha)) $
	      / (f2 -f3 + f4 + Q)

;Finally, calculate the gas pressure.
  Pg = Pg_Pe * Pe

end
