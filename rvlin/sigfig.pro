function isint, int

  return, int eq fix(int)

end

function sigfig, inval, inunc, divider = div, parens = parens, scientific = sci, exps1 = exps1, printerr = printerr, sigfig = sigfig

  if n_elements(div) lt 1 then div = ' $\pm$ '
  if n_elements(exps1) lt 1 then exps = ' $\times 10^{' else exps = exps1[0]
  if n_elements(exps1) lt 2 then exps2 = '}$ ' else exps2 = exps1[1]

  if n_elements(inval) gt 1 then begin
    out = strarr(n_elements(inval))
    if n_elements(inunc) eq 1 then begin
      uncs = inval*0+inunc
    endif else begin
      if n_elements(inunc) ne n_elements(inval) then begin
        print, 'Arrays inval and inunc must be the same length'
      endif else begin
        uncs = inunc
      endelse
    endelse
    for i = 0, n_elements(inval)-1 do out[i] = sigfig(inval[i], uncs[i], div = div, par = parens, sci = sci, exps1 = exps1, print = printerr, sigfig = sigfig)
    return, out
  endif

  minus = inval lt 0 ? '-' : ''

  if n_elements(sigfig) gt 0 then unc = inval/10d^(sigfig-2) else   unc = abs(inunc)
  scientific, unc, uman, uex      ;get mantissa, exponent for uncertainty
  unc = ulong64(abs(unc*10d^(1-uex)) + 0.5)/10d^(1-uex) ;two sig digits for uncertainty
  scientific, unc, uman, uex ;recalculate exponent to treat rounding
  if inval lt 1d15 then val = ulong64(abs(inval*10d^(1-uex)) + 0.5)/10d^(1-uex)$ ;round to two digits past uncertainty
    else val = inval
  scientific, val, man, ex        ;get mantissa, exponent for value
  if ~keyword_set(sci) and ex gt 15 then begin
    exps = 'e'
    exps2 = ''
    sci = 1
  endif
  if val eq 0 then ex = uex    ;default to exponent of uncertainty if no sig figs for val
  sigfigs = (ex+1-uex) > 1     ;# of significant figures after decimal in mantissa
  places =     (ex+1)-uex+(uex lt 1)-(ex lt 0)*ex+1  ;total places needed to express number
  placespast = 1-uex                                 ;places past decimal
  formsci = '(d'+strtr(sigfigs+2)+'.'+strtr(sigfigs)+')' ;for sci notation
  form = '(d'+strtr(places)+'.'+strtr(placespast)+')'    ;for regular notation
  ssman = strtr(man, form = formsci)   ;number as mantissa


  if keyword_set(parens) then begin
    if keyword_set(sci) then begin
      spar = strtr(round(unc/10d^(uex-1)))
      exp = ex > uex  ;we'll use the higher of the two exponents for the format
      if uex ge ex then begin
        ssman = strtr(man/10d^(exp-ex), form = '(f3.1)')   ;move decimal in the mantissa over
        spar = strtr(uman, form = '(f3.1)')         ;two decimals here
      endif
      spar = '('+spar+')'
      if ~strmatch(ssman, '*[1-9]*') then minus = '' ;no -0
      out = minus+ssman+spar+exps+strtr(exp)+exps2 ;sci notation with precision
      return, out
    endif else begin
      case sign(uex) of 
        -1: begin
          spar = strtr(round(unc/10d^(uex-1))) ;two digits right of decimal
          out = strtr(val, form = form)
        end
        0: begin 
          spar = strtr(unc, form = '(f3.1)') ;need a decimal
          out = strtr(val, form = form)
        end
        1: begin
          spar = strtr(long(round(unc)))
          out = strtr(long(round(val))) ;no decimal form for ints
        end
      endcase
      spar = '('+spar+')'
      if ~strmatch(out, '*[1-9]*') then minus = '' ;no -0
      out = minus+out+spar
      return, out
    endelse
  endif
  

  if keyword_set(sci) then begin
    if ~strmatch(ssman, '*[1-9]*') then minus = '' ;no -0
    out = minus+ssman+exps+strtr(ex)+exps2
    if keyword_set(printerr) then out = out+div+string(uman, form = '(d3.1)')+exps+strtr(uex)+exps2
    return, out
  endif

  if uex ge 1 then out = strtr(ulong64(val+0.5)) else out = strtr(val, form = form) ;no decimal form for ints
  if strmatch(out, '*[1-9]*') then out = minus+out
  if uex ge 1 then err = strtr(ulong64(unc+0.5)) else err = strtr(unc, form = form)
  if uex eq 1 and strmid(out, strlen(out)-1) eq '0' and strmatch(out, '[1-9]') then out = out+'.'   
          ;need a trailing decimal to show significance of multiples of 10
  if keyword_set(printerr) then out = out+div+err 

  
  return, out

end
