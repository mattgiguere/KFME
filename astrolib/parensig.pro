function parensig, value, sigma, tex=tex, idl=idl, silent=silent
;Print a measured value and associated uncertainty in notation where
; the two most significant digits of the uncertainty are enclosed in
; parentheses after the corresponding two least significant digits
; of the measured value. Decimal notion is used where practical.
; Scientific notation is used otherwise.
;
;Input:
; value (scalar) measured value to return as string
; sigma (scalar) uncertainty in measured value
; /tex (switch) format output for TeX or LaTeX
; /idl (switch) format output for IDL annotation strings
; /silent (switch) supress warnings when sigma=0 (returns null string)
;
;Output:
; Returns a string with value and sigma in parenthetical notation
;
;Examples:
;
; print, parensig(-4618.42, 2.49)
; % -4618.4(2.5)
;
; print, parensig(-4618.42, 24.9)
; % -4618(25)
;
; print, parensig(-4618.42, 249)
; % -4.62(25)e+03
;
; parensig(-4618.42, 249, /tex)
; % $-4.62(25)\times10^{3}$
;
; print, parensig(-4618.42, 249, /idl)
; % -4.62(25)×10!u3!n
;
;Notes:
; The IDL form of scientific notation uses the isolatin × character
;  for multiplacation. If necessary, an additional keyword (/usex) could
;  be added to optionally use a lower case "x" instead of ×.
;
;History:
; 2000-Nov-25 Valenti  Initial coding.

if n_params() lt 2 then begin
  print, 'syntax: print, parensig(value, sigma [,/tex ,/idl])'
  retall
endif

;Trap nonpositive sigma.
  if sigma le 0 then begin
    if not keyword_set(silent) then begin
      message, /info, 'sigma is not positive - unable to construct string'
    endif
    return, ''
  endif

;Figure out which two digits (isig and isig+1) will contain sigma.
; ..., 4:thousands, 3:hundreds, 2:tens, 1:ones, 0:tenths, -1:hundreths, ...
  isig = ceil(alog10(sigma))

;Special handling when error is an exact multiple of 10.
  if abs(alog10(sigma)) mod 1 eq 0 then isig = isig + 1

;Figure out which digit (ival) will be the start of value.
; ..., 4:thousands, 3:hundreds, 2:tens, 1:ones, 0:tenths, -1:hundreths, ...
  if value ne 0 then begin
    ival = ceil(alog10(abs(value)))
    if alog10(abs(value)) mod 1 eq 0 then ival = ival + 1
    ival = ival > isig
  endif else begin
    ival = isig
  endelse

;At this point we know to print digits from ival to isig+1.
;Calculate number of digits to print.
  ndig = ival - isig + 2

;One extra digit for decimals between 0.1 and 0.9999....
  if ival eq 0 then extra = 1 else extra = 0

;One extra digit for negative numbers.
  if value lt 0 then extra = extra + 1

;Decide between scientific notation and decimal representation.
  if isig gt 2 or ival lt 0 then scinot = 1 else scinot = 0

;Handle decimal case.
  if scinot eq 0 then begin

;Write value to output string.
    if isig eq 2 then begin				;integer value
      fmt = '(i99.' + strtrim(ndig, 2) + ')'
      out = strtrim(string(round(value), form=fmt), 2)
      if value lt 0 then out = '-' + out		;retain sign
    endif else begin					;decimal value
      fmt = '(f' + strtrim(ndig+extra+1, 2) $
          + '.' + strtrim(ndig-ival, 2) + ')'
      out = string(value, form=fmt)
    endelse

;Append uncertainty.
    if isig eq 1 then begin				;decimal in sigma
      outsig = string(sigma, form='(f3.1)')
    endif else begin					;no decimal in sigma
      sigscale = 10.0 ^ (2-isig)
      outsig = strtrim(round(sigma*sigscale), 2)
    endelse

;Concatenate value string and uncertainty string.
    out = out + '(' + outsig + ')'

;For TeX format, enclose negative numbers in math mode.
    if keyword_set(tex) then begin
      ipos = strpos(out, '-')
      if ipos ge 0 then out = '$' + out + '$'
    endif

;Return decimal strings.
    return, out

;Handle scientific notation case.
  endif else begin

;Determine exponent.
    expo = ival - 1
    scale = 10d0 ^ (-expo)

;Generate mantissa by recursive call to this routine (TeX suppressed).
    mant = parensig(scale*value, scale*sigma)

;Handle various encodings of exponent.
    if keyword_set(tex) then begin
      expstr = '\times10^{' $
             + strtrim(expo, 2) $
             + '}'
    endif else if keyword_set(idl) then begin
      expstr = string(215b) $
             + '10!u' $
             + strtrim(expo, 2) $
             + '!n'
    endif else begin
      if expo lt 0 then esign = '' else esign = '+'
      expstr = 'e' $
             + esign $
             + strtrim(string(expo, form='(i9.2)'), 2)
    endelse

;Concatenate value string and uncertainty string.
    out = mant + expstr

;For TeX format, all numbers must be in math mode.
    if keyword_set(tex) then begin
      out = '$' + out + '$'
    endif

;Return scientific notation strings.
    return, out

  endelse

end
