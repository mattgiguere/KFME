function mpf_mass, per, mstar, k, ecc, inc
;+
;
;PURPOSE: This calculates the mass given the other variables.
;
;INPUT:
;  PER: The orbital period (in days)
;	MSTAR: The mass of the star (in M_SUN)
;	K: The semi-amplitude (in m/s)
;	ECC: The eccentricity
;	INC: The inclination (in radians)
;
; ~MJG
;-

a_pl=((per/365.2564d)^2*mstar)^(1./3.)

m_pl = 1.92d4 * k * per * mstar * sqrt(1 - ecc^2) / $
		(1d5 * a_pl * 2d * !dpi *sin(inc))
		
  return, m_pl
end;mpf_mass.pro