function mpf_K, a_pl, m_pl, period, m_star, inc, ecc

; PURPOSE
;
; Calculate K, the amplitude of the radial velocity from the other parameters. This is
; useful since we usually enter the other parameters, but rv_fit use K rather than a_pl
; to fit the data
;
; INPUT
;
; a_pl semi major axis of the planet, in AU
; m_pl mass of the planet in Earth masses
; period of the orbit in days
; m_star mass of the star in solar masses
; inc inclination of the orbit in degrees
; ecc eccentricity of the orbit
;
; OUTPUT
; 
; K the amplitude of the radial velocity in m/s
;
; 
;
; Thomas Ader, Mai 16, 2008
;

Knum = a_pl*m_pl*2*!pi*sin(inc*!dtor)
Kden = period*m_star*sqrt(1-ecc^2)

K = Knum/Kden * 100000./19200.

return, K

end