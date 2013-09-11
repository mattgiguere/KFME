pro arrowsym,angle,origin=origin,tail=tail,twid=twid,size=size,nofill=nofill
;Redefines the user-defined plot symbol (psym=8) to be an arrow.
;15-Feb-94 JAV Create.

  if n_params() lt 1 then begin
    print,'syntax: arrowsym,angle [align=,tail=,twid=,size=,/nofill]'
    print,'  angle: clockwise from right (0.0 through 360.0 degrees).'
    print,'  origin (1.0): from tip of tail (0.0) to head (1.0).'
    print,'  tail (3.0): length of tail relative to length of head.'
    print,'  twid (0.3): width of tail relative to width of head.'
    print,'  size (1.0): relative size of arrow.'
    print,'  /nofill (0): forces arrow to be unfilled.'
    retall
  endif

;Parse keywords, use defaults as needed.
  if n_elements(size) eq 0 then size = 1.0
  if n_elements(origin) eq 0 then origin = 1.0
  if n_elements(tail) eq 0 then tail = 3.0
  if n_elements(twid) eq 0 then twid = 0.3
  if keyword_set(nofill) then fill = 0 else fill = 1

;Define basic arrow shape.
  x = [0, -1.6, -1.6,    -1.6-tail, -1.6-tail, -1.6,    -1.6, 0]
  y = [0,  0.6,  twid,  twid,   -twid,   -twid, -0.6, 0]

;Adjust origin of arrow.
  x = x + (1 - origin) * (1 + tail)

;Rotate arrow.
  r = sqrt(x*x + y*y)
  ifix = where(r gt 0)
  signfix = 1 - 2*(y lt 0)
  theta = signfix(ifix) * acos(x(ifix)/r(ifix))
  theta = theta + angle * !dtor
  x(ifix) = r(ifix)*cos(theta)
  y(ifix) = r(ifix)*sin(theta)

;Magnify size of arrow.
  x = x * size
  y = y * size

;Define user plot symbol (psym=8).
  usersym,x,y,fill=fill

end
