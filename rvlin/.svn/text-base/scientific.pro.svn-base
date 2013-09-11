pro scientific, inval, mantissa, exponent

  s = sign(inval)
  n = n_elements(inval)
  z = where(s eq 0, nz)
  val = abs(inval)
  if nz gt 0 then val[z] = 1 ;0 will break this procedure
  exponent = floor(alog10(val))
  mantissa = inval/10d^(exponent)
  if nz gt 0 then exponent[z] = 0. ;correct values for 0
  if nz gt 0 then mantissa[z] = 0. ;correct values for 0

end

