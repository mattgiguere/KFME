Pro Zeeman,Trans,Theta,Comps,gi=g_init,gf=g_final $
          ,geff=g_eff,Ji=J_init,Jf=J_final,LS=LS,qn=qn,merge=merge
;Computes Zeeman components based on either LS coupling or empirical g-values.
; Trans (input string) transition of the form, e.g. '3p2-3p1' or '6H1.5-4H2.5'.
; Theta (input scalar) angle (degrees) between line-of-sight and field lines.
;   [mg.for expects Comps computed for Theta = 90 degrees.]
; Comps (output array(# of components,3)) zeeman pattern for transition.
;   Comps(0,*) are the Lande-g values for each component.
;   Comps(1,*) are the intensity weights of each component, normalized such
;     that the sum of all components is 2.
;   Comps(2,*) are the polarization types: 0=linear, +/-1=right/left circular.
; =g_init (optional input scalar) empirical Lande-g value of initial state.
;   A value of -1.0 is equivalent to not specifying a value for g_init.
; =g_final (optional input scalar) empirical Lande-g value of final state.
;   A value of -1.0 is equivalent to not specifying a value for g_final.
; =g_eff (optional output scalar) effective Lande-g value of transition, based
;   on empirical Lande-g values, if available, otherwise based on LS coupling.
; =J_init (optional output scalar) J quantum number of initial state.
; =J_final (optional output scalar) J quantum number of final state.
; /LS (optional input flag) (0)1: (don't) force use of LS g-values.
; =qn (input array(6)) S_init, L_init, J_init, S_final, L_final, J_final.
;   Only J_init and J_final are used when empirical g_init and g_final are
;   specified.
;Edit History:
; 12-Sep-91 JAV Adapted from Marcy's ANA adaptation of similar FORTRAN code,
;		which in turn is based on Condon and Shortley, but with
;		pi -> pi * Sin(Theta)^2 , sigma -> sigma * (1+Cos(Theta)^2).
; 20-Sep-91 JAV Added J_init, J_final keywords, let g=-1.0 flag no value.
; 29-Sep-93 JAV Added "qn" keyword and logic.
; 31-Jul-98 JAV Added logic to trim components with zero weight.

If N_Params() lt 3 Then Begin
  Message,/Info, $
    'Syntax: Zeeman,Trans,Theta,Comps [,gi= ,gf=] [,geff= ,Ji= ,Jf= ,/LS]'
  RetAll
EndIf

  if n_elements(merge) eq 0 then merge = 0.005

  If N_Elements(qn) eq 0 then Begin		;transition info from trans
;Prepare to parse transition.
    LStr = ['S','P','D','F','G','H','I','J','K'] ;letter designations of L
    iDash = StrPos(Trans,'-')			;index of dash separator

;Parse initial state.
    State = StrMid(Trans,0,iDash)		;initial state string
    Dummy = Max(Byte(State),iL)			;index of initial L-value
    S1 = (Float(StrMid(State,0,iL)) - 1) / 2.0	;initial S-value
    L1 = Where(LStr eq StrUpCase(StrMid(State,iL,1)))	;initial L-value
    L1 = Float(L1(0))				;convert to real scalar
    J1 = Float(StrMid(State,iL+1,10))		;initial J-value
    J_init = J1					;load return argument

;Parse final state.
    State = StrMid(Trans,iDash+1,10)		;final state string
    Dummy = Max(Byte(State),iL)			;index of initial L-value
    S2 = (Float(StrMid(State,0,iL)) - 1) / 2.0	;final S-value
    L2 = Where(LStr eq StrUpCase(StrMid(State,iL,1))) ;final L-value
    L2 = Float(L2(0))				;convert to real scalar
    J2 = Float(StrMid(State,iL+1,10))		;final J-value
    J_final = J2				;load return argument
  EndIf Else Begin				;transition info from qn
    S1 = (qn(0)-1)/2.0				;extract quantum numbers
    L1 = qn(1)
    J1 = qn(2)
    S2 = (qn(3)-1)/2.0
    L2 = qn(4)
    J2 = qn(5)
  EndElse

;print,s1,l1,j1,s2,l2,j2

;Verify adherence to selection rules.
  If Abs(J2-J1) gt 1 Then Begin			;true: forbidden transition
    Message,'Change in J due to transition must be -1, 0, or +1.'
  EndIf
  If (J1 eq 0) and (J2 eq 0) Then Begin		;true: forbidden transition
    Message,'Transitions between J=0 states are strictly forbidden.'
  EndIf

;Calculate g_init from LS coupling, unless provided as keyword.
  If Keyword_Set(LS) Then g_init = -1.0		;true: force LS coupling
  If N_Elements(g_init) eq 0 Then g_init = -1.0	;define g_init,flag LS coupling
  If g_init eq -1.0 Then Begin			;true: use LS coupling
    If J1 eq 0 Then g_init = 0.0 Else Begin	;avoid divide by zero
      g_init = 1.5 + (S1*(S1+1.0) - L1*(L1+1.0)) / (2.0*J1*(J1+1.0))
    EndElse
  EndIf

;Calculate g_final from LS coupling, unless provided as keyword.
  If Keyword_Set(LS) Then g_final = -1.0		;true: force LS coupling
  If N_Elements(g_final) eq 0 Then g_final = -1.0  ;def g_final,flag LS coupling
  If g_final eq -1.0 Then Begin			;true: use LS coupling
    If J2 eq 0 Then g_final = 0.0 Else Begin	;avoid divide by zero
      g_final = 1.5 + (S2*(S2+1.0) - L2*(L2+1.0)) / (2.0*J2*(J2+1.0))
    EndElse
  EndIf

;Calculate the effective Lande-g of the transition.
  g_eff = 0.5  * (g_init + g_final) $
        + 0.25 * (g_init - g_final) * (J1*(J1+1.0) - J2*(J2+1.0))

;Prepare to generate Zeeman components.
  dJ = J2 - J1					;delta J (-1,0,+1)
  nSig = J1 + J2				;# of L/R sigma components
  nPi = nSig - Abs(dJ) + 1			;# of pi components
  nComp = 2*nSig + nPi				;total # of Zeeman components

;Generate initial and final m-values.
  mLCP = FIndGen(nSig) - J2 + 1			;initial left circ. m-values
  mLP = FIndGen(nPi) - (J1 < J2)		;initial linear m-values
  mRCP = FIndGen(nSig) - J1			;initial right circ. m-values
  m1 = [mLCP, mLP, mRCP]			;all initial m-values
  m2 = [mLCP-1, mLP, mRCP+1]			;all final m-values

;Calculate intensities of each component.
  dm = m2 - m1					;change in m-value for comp.
  dm0 = dm eq 0.0				;1.0 where dm=0, else 0.0
  Case dJ Of 					;find intensities for given dJ
    -1: Wt = (J1 - dm*m1) * (J1 - dm*m2) - dm0*m1*m1
     0: Wt = (J1 - dm*m1) * (J1 + dm*m2) - dm0*(J1*J1 - m1*m1)
    +1: Wt = (J2 + dm*m1) * (J2 + dm*m2) - dm0*m1*m1
  EndCase

;Adjust component weights for viewing angle.
  ThetaRad = 3.14159265359 * Theta / 180.0	;express Theta in radians
  Wt = Wt * (0.25*(1-dm0)*(1.0+Cos(ThetaRad)^2) + dm0*Sin(ThetaRad)^2)

;Store results in return array, combining degenerate components as needed.
  Split = g_init * m1 - g_final * m2		;splittings of components
  If Abs(g_final - g_init) lt merge Then Begin	;true: nearly degenerate comps.
    iLCP = IndGen(nSig)				;indicies of LCP components
    iLP = nSig + IndGen(nPi)			;indicies of LP components
    iRCP = nSig + nPi + iLCP			;indicies of RCP components
    WSplit = Split * Wt				;weighted splittings
    NewWt = [Total(Wt(iLCP)), Total(Wt(iLP)), Total(Wt(iRCP))]
    Comps = FltArr(3,3,/NoZero)			;init component storage
    Comps(0,0) = Total(WSplit(iLCP)) / NewWt(0)	;weighted splitting of LCP
    If Theta eq 0.0 Then Begin			;true: pi intensity is zero
      Comps(0,1) = 0.0				;set zero intensity
    EndIf Else Begin				;else: pi intensity nonzero
      Comps(0,1) = Total(WSplit(iLP)) / NewWt(1)  ;weighted splitting of LP
    EndElse
    Comps(0,2) = Total(WSplit(iRCP)) / NewWt(2)	;weighted splitting of RCP
    Comps(1,*) = 2.0 * NewWt / Total(NewWt)	;normalized intensities
    Comps(2,*) = [-1.0, 0.0, +1.0]		;type (-1,0,+1)=(LCP,LP,RCP)
    If nComp gt 3 and g_final ne g_init Then Begin
;     Message,/Info,'Combining nearly degenerate components.'
    EndIf
  EndIf Else Begin				;else: no degenerate comps.
    Comps = FltArr(3,nComp,/NoZero)		;init component storage
    Comps(0,*) = Split				;splittings
    Comps(1,*) = 2.0 * Wt / Total(Wt)		;normalized intensity
    Comps(2,*) = dm				;type (-1,0,+1)=(LCP,LP,RCP)
   EndElse

;Remove components with zero weight.
  ikeep = where(comps(1,*) gt 0)		;weghts > 0
  comps = comps(*,ikeep)			;trim

End
