function s2rhkp, s, bv
;  This function computes R_HK prime from the S value and B-V.
;  Formulae taken from Noyes et al. 1984, ApJ, 279, 763.
;
;  19-Mar-1997  CMJ  Written
;
if n_params() lt 2 then begin
   print,'Syntax: rhkp = s2rhkp(s, bv)'
   retall
endif

  rphot = 10.^(-4.898 + 1.918*bv^2. - 2.893*bv^3.)
  ccf = 10.^(1.13*bv^3. - 3.91*bv^2. + 2.84*bv - 0.47)
  x = (0.63 - bv) > 0.
  dlogc = 0.135*x - 0.814*x^2. + 6.03*x^3.
  ccfp = 10.^(alog10(ccf) + dlogc)
  rhk = 1.340e-4*ccfp*s
  rhkp = rhk - rphot
  return,rhkp

end
