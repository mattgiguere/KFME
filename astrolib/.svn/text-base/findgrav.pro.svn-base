Pro FindGrav,Teff,G,M,R,cgs=cgs
;Uses the empirical relation of Harmanec (Bull. Astron. Inst. Czechosl.,
;  Vol. 39, pp. 329-345 (1988)) to deduce surface gravity, mass, and radius
;  of luminosity class V stars with 2661 < Teff < 41690 Kelvin.
; Teff (input scalar) effective temperature in Kelvin.
; G (optional output scalar) cgs=0: log10(gravity) in cgs; cgs=1: gravity
;   in grams/cm/cm.
; M (optional output scalar) cgs=0: mass in solar masses; cgs=1: mass in grams
; R (optional output scalar) cgs=0: radius in solar radii; cgs=1: radius in cm
; =cgs (optional flag) 0: results in "common units"; 1: results in cgs.
;18-Sep-91 JAV Create.

If N_Params() eq 0 Then Begin
  Message,/Info,'Syntax: FindGrav,Teff [,gravity,Mass,Radius] [,/cgs].'
  RetAll
EndIf

;Define useful constants.
  Msun = 1.9892e33                            ;mass of sun (grams)
  Rsun = 6.95997e10                           ;radius of sun (cm)
  Gsun = 27400.                               ;surface gravity of sun (cgs)

;Use relations from Harmanec's paper.
  X = aLog10(Teff)                            ;log effective temperature
  M = (((-1.744951 *X + 30.31681)*X - 196.2387)*X + 562.6774)*X - 604.0760
  R = (((-0.8656627*X + 16.22018)*X - 112.2303)*X + 341.6602)*X - 387.0969
  G = aLog10(Gsun) + M - 2*R


;Convert mass and radius from log10.
  M = 10^M                                    ;convert mass from log10
  R = 10^R                                    ;convert radius from log10

;Output to screen, if no return variables were specified.
  If N_Params() eq 1 Then Begin                       ;true: output to screen
    Message,/Info,'Log(g) = ' + StrTrim(String(G),2) + ' (cgs)        = Log(' $
      + StrTrim(String(10^G/100.0),2) + ' m/sec).'
    Message,/Info,'Mass   = ' + StrTrim(String(M),2) + ' solar masses = ' $
      + StrTrim(String(Msun*M),2) + ' grams.'
    Message,/Info,'Radius = ' + StrTrim(String(R),2) + ' solar radii  = ' $
      + StrTrim(String(Rsun*R),2) + ' cm.'
  EndIf

;Convert output to cgs, if required.
  If Keyword_Set(cgs) Then Begin              ;true: convert to cgs
    G = 10^G                                  ;convert to cm/sec/sec
    M = Msun * M                              ;convert to grams
    R = Rsun * R                              ;convert to cm
  EndIf

End
