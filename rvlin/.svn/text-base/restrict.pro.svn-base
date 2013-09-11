function restrict, in, inmin, inmax, lims = lims

  min = double(inmin)
  max = double(inmax)
  n = n_elements(in)
  nl = n_elements(lims)
  inf = where(~finite(in), ninf)
  if nl ne 1 then lims = 2 else if lims eq 3 then lims = 2
  data = in
  if n eq 1 then data = replicate(in, 1)
  range = max-min
  sdata = (data-min)/range
  repeat begin
    w = where(~within(data, min, max, lims = lims), nw)
    if nw gt 0 then data[w] = data[w]+(fix(abs(data-min)/range) >  1)*range*(2*(data[w] lt max)-1)
  endrep until nw-ninf eq 0

  return, n eq 1 ? data[0] : data

end

