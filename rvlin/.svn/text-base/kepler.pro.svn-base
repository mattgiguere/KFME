;Solve Kepler's equation

;Iteratively solve for E (anomoly) given M (mean anomoly) and e (eccentricity)

;JTW Written at Berkeley 
;Adopted for TEDI Dec 2007
;From Murray & Dermott p. 35, from Danby (1988)

function kepler, inbigM, inecc
  Marr = double(inbigm)
  eccarr = double(inecc)
  nm = n_elements(marr)
  nec = n_elements(eccarr)
  if nec eq 1 and nm gt 1 then eccarr = fan(eccarr, nm)
  if nec gt 1 and nm eq 1 then marr = fan(marr, nec)
  conv = 1d-12
  k = 0.85
  mphase = restrict(Marr/2/!dpi, 0, 1)
  ssm = (mphase le 0.5) - (mphase ge 0.5 or mphase eq 0)*1; = sign(sin(Marr)), but faster
  Earr = Marr+ssm*k*eccarr  ;first guess at E
  fiarr = (Earr-eccarr*sin(Earr)-Marr)  ;E - e*sin(E)-M    ; should go to 0 when converges
  convd = where(abs(fiarr) gt conv, nd) ;which indices have converged?
  count = 0
  while nd gt 0 do begin                ;while unconverged elements exist...
    count = count+1
    M = Marr[convd]                     ;just the unconveged elements, please...
    ecc = eccarr[convd]
    E = Earr[convd]

    fi = fiarr[convd]  ;fi = E - e*sin(E)-M    ; should go to 0
    fip = 1-ecc*cos(E) ;d/dE(fi) ;i.e.,  fi^(prime)
    fipp = ecc*sin(E)  ;d/dE(d/dE(fi)) ;i.e.,  fi^(\prime\prime)
    fippp = 1-fip ;d/dE(d/dE(d/dE(fi))) ;i.e.,  fi^(\prime\prime\prime)
    
    d1 = -fi/fip                             ;first  order correction to E
    d2 = -fi/(fip+d1*fipp/2.)                ;second order correction to E
    d3 = -fi/(fip+d2*fipp/2.+d2*d2*fippp/6.) ;third  order correction to E
    E = E+d3
    Earr[convd] = E
    fiarr = (Earr-eccarr*sin(Earr)-Marr)     ;how well did we do?
    convd = where(abs(fiarr) gt conv, nd)    ;test for convergence
    if count gt 100 then begin
      print, 'WARNING!  Keplers equation not solved!!!'
;      junk = get_kbrd(1)
      nd = 0
    endif
  endwhile

  if n_elements(inbigM) eq 1 then return, Earr[0] else return, Earr

end
