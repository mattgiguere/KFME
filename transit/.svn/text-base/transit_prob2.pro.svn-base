;+
;
;  NAME: 
;     transit_prob2
;
;  PURPOSE: 
;   To calculate the probability of transit given the planetary
;	orbital parameters. 
;
;  CATEGORY:
;      CHIRON
;
;  CALLING SEQUENCE:
;
;      transit_prob2
;
;  INPUTS:
;
;  OPTIONAL INPUTS:
;
;  OUTPUTS:
;
;  OPTIONAL OUTPUTS:
;
;  KEYWORD PARAMETERS:
;    
;  EXAMPLE:
;      transit_prob2
;
;  MODIFICATION HISTORY:
;        c. Matt Giguere 2011.08.30 01:02:52 PM
;
;-
pro transit_prob2, $
a_au = a_au, $
r_rsun = r_rsun, $
ecc = ecc, $
om = om, $
tprob = tprob, $
noprint = noprint

if ~keyword_set(r_rsun) then r_rsun = 1d


prob = 4.5d-3 / a_au * (r_rsun + 0.1d) * $
(1d + ecc*cos(!dpi/2d - om)) / (1d - ecc^2)

tprob = prob*1d2

if ~keyword_set(noprint) then print, 'the probability of transit is: ', strt(tprob), ' %'
end;transit_prob2.pro