pro co,vup,vlo,dJ,Jvec,wn,isotope=isotope
;Calculate wavelengths of CO transistions. 
;Molecular constants from Farrenq et al. 1991, J. of Mol. Spec., 149, 375.
; vup (input scalar) vibrational quantum number of upper state.
; vlo (input scalar) vibrational quantum number of lower state.
; dJ (input scalar) change in rotational quantum number (-1=P, 0=Q, +1=R)
; Jvec (input vector) list of J values in lower state.
; wn (output vector) wavenumber of specified transitions (cm^-1).
; isotope= (input scalar) isotopic specifier (0=C12O16, 1=C12O17, 2=C12O18,
;   3=C13O16).
;JAV 26-Mar-94	Create.
 
if n_params() lt 5 then begin
  print,'co,vup,vlo,dJ,Jvec,wn [isotope=,/help]'
  retall
endif

;Define common block, used to speed up subsequent calls.
  common common_co,Y_CO

;Defaults.
  if n_elements(isotope) eq 0 then isotope=0		;assume C12O16

;If this is the first call, then load atomic and molecular constants.
  if n_elements(Y_CO) eq 0 then begin			;true: first call

;Atomic masses in a.m.u. (from Table III)
;Note: melec = 0.548579910d-3, according to Particle Data Booklet.
    mass = [12.00000000d0, 15.99491502d0 $		;C12, O16
	   ,12.00000000d0, 16.99913290d0 $		;C12, O17
	   ,12.00000000d0, 17.99916002d0 $		;C12, O18
	   ,13.00335440d0, 15.99491502d0 ]		;C13, O16
    niso = n_elements(mass) / 2				;number of isotopes
    mass = reform(mass,2,niso)				;make array
    mu = 1d0 / (1d0/mass(0,*) + 1d0/mass(1,*))		;reduced masses
    melec = 0.548580228d-3				;mass of electron

;Isotopically invariant Dunham Parameters (from Table IV).
;U has units of cm^-1 amu^(i/2+k).
    ni = 10						;Dunham i terms
    nk = 7						;Dunham k terms
    U = dblarr(ni,nk)
    U(1:9,0) = [5681.3676166d0,    -91.1055149d0,     0.18703287d0, $
		   0.3260748d-2,     0.20671d-4,      0.6637d-6,    $
		  -0.71632d-6,       0.273954d-7,    -0.476404d-9 ]
    U(0:7,1) = [  13.243473898d0,   -0.3142988561d0,  0.337229d-4,  $
		  -0.26419d-5,       0.142950d-5,    -0.114861d-6,  $
		   0.27528d-8,      -0.122960d-9 ]
    U(0:4,2) = [  -0.2878458471d-3,  0.1273849d-6,   -0.5961686d-7, $
		   0.205163d-8,     -0.230581d-9 ]
    U(0:2,3) = [   0.189666901d-8,  -0.12056492d-9,  -0.270763d-11 ]
    U(0:2,4) = [  -0.79898533d-13,  -0.4284994d-14,  -0.7658d-16   ]
    U(0:1,5) = [  -0.6901397d-18,   -0.23494d-18 ]
    U(0,6)   = [  -0.15785d-21 ]

;Mass scaling parameters, DelC and DelO, are dimensionless.
    DelC = dblarr(ni,nk)
    DelC(1:3,0) = [ 0.700930d0,  0.42815d0, -12.2868d0 ]
    DelC(0:1,1) = [-2.057208d0, -1.2452d0 ]
    DelC(0,2)   = [-7.353d0 ]
    DelO = dblarr(ni,nk)
    DelO(1:3,0) = [-0.171324d0, -0.91035d0,  -3.534d0 ]
    DelO(0:1,1) = [-2.119921d0, -3.0206d0 ]
    DelO(0,2)   = [ 1.350d0 ]

;Build Dunham coefficients.
    ii = dindgen(ni)					;i indicies
    kk = dindgen(nk)					;k indicies
    Y_CO = dblarr(ni,nk,4)
    for iso=0,3 do begin				;loop thru isotopes
      Y_CO(*,*,iso) $
	= (mu(iso)^(-ii/2d0) # mu(iso)^(-kk)) * U $
	* (1d0 + melec * (DelC/mass(0,iso) + DelO/mass(1,iso)))
    endfor
  endif
  
;Get sizes (in case we skipped the definition block above).
  sz = size(Y_CO)					;variable info
  ni = sz(1)						;Dunham i terms
  nk = sz(2)						;Dunham k terms
  nJ = n_elements(Jvec)					;number of lines

;Loop thru J values, calculating energies.
  Y = Y_CO(*,*,isotope)					;get Dunham coeffs
  vlovec = (vlo + 0.5d0) ^ dindgen(ni)
  vupvec = (vup + 0.5d0) ^ dindgen(ni)
  kk = dindgen(nk)
  wn = dblarr(nJ)
  for iJ=0,nJ-1 do begin
    Jlo = Jvec(iJ)
    Elo = total(Y * (vlovec # (Jlo*(Jlo+1d0))^kk))
    Jup = Jlo + dJ
    Eup = total(Y * (vupvec # (Jup*(Jup+1d0))^kk))
    wn(iJ) = Eup - Elo
  endfor
end
