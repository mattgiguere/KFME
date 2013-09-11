pro psym,name,fill=fill,thick=thick
;Sets user defined plot symbol to various useful shapes.

if n_elements(name) eq 0 then begin
  print,'syntax: psym,name [,/fill,thick=thick]'
  print,'  name={circle,square,diamond,downtri,tilde,spiral}'
  retall
endif

  if n_elements(fill)  eq 0 then fill  = 0
  if n_elements(thick) eq 0 then thick = 1

  case strlowcase(name) of
    'circle': begin
       theta = 2*!pi * findgen(21)/20
       usersym, cos(theta), sin(theta) $
              , fill=fill, thick=thick
       end
    'square': usersym,[1,-1,-1,1,1],[1,1,-1,-1,1] $
                     ,fill=fill,thick=thick
    'diamond': usersym,[1,0,-1,0,1],[0,1,0,-1,0] $
                     ,fill=fill,thick=thick
    'tri': usersym,[1,-1,0,1],[-1,-1,1,-1] $
                      ,fill=fill,thick=thick
    'downtri': usersym,[1,-1,0,1],[1,1,-1,1] $
                      ,fill=fill,thick=thick
    'righttri': usersym,[1,-1,-1,1],[0,1,-1,0] $
                      ,fill=fill,thick=thick
    'lefttri': usersym,[-1,1,1,-1],[0,1,-1,0] $
                      ,fill=fill,thick=thick
    'tilde': begin
       x = -1.0 + 2.0*findgen(21)/20
       y = 0.2*sin(-1.30*!pi*x)
       yfudge = 0.05*(1-abs(x)) + 0.025*thick
       usersym,[x,reverse(x),-1.0] $
              ,[y-yfudge,reverse(y)+yfudge,y(0)-yfudge(0)] $
              ,fill=fill,thick=thick
       end
    'spiral': begin
       r = findgen(21)/20
       r = [1-r, r]
       theta = 2*!pi * findgen(21)/20
       usersym,r*[cos(theta),-cos(theta)] $
	      ,r*[sin(theta),sin(theta)] $
              ,fill=fill,thick=thick
       end
    'earth': begin
       theta = 2.5 * !pi * findgen(16) / 15
       usersym,[-1,1,cos(theta),0,0] $
              ,[0,0,sin(theta),1,-1] $
              ,fill=fill,thick=thick
       end
  else: begin
    print,'psym: unrecognized plot symbol name.'
    return
    end
  endcase

end
