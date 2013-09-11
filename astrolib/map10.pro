;====================================================================
function Transform,order,alpha,beta,xx
;
; Coordinate transformation:
;
; If order==1 then
;   1. Tilt Z-axis by alpha degrees.
;   2. Rotate around Z-axis by beta degrees.
; else
;   1. Rotate around Z-axis by beta degrees.
;   2. Tilt Z-axis by alpha degrees.
;
; Coordinate system: X - horizontal in image plane pointing right
;                    Y - vertical in image plain pointing up
;                    Z - towards the observer

  TORAD=!DPI/180
  COSA=COS(alpha*TORAD) & SINA=SIN(alpha*TORAD)
  COSB=COS(beta *TORAD) & SINB=SIN(beta *TORAD)

; Tilt of Z axis
  a=[[1.d0, 0.d0, 0.d0], $
     [0.d0, COSA,-SINA], $
     [0.d0, SINA, COSA]]

; Rotation around Z axis
  b=[[COSB,-SINB, 0.d0], $
     [SINB, COSB, 0.d0], $
     [0.d0, 0.d0, 1.d0]]

  s=size(xx)
  if(s(0) lt 3) then begin
    if(order eq 1) then begin
      x=a#xx & x=b#x
    endif else begin
      x=b#xx & x=a#x
    endelse
  endif else if(s(0) eq 3) then begin
    n=s(2) & m=s(3)
    x=dblarr(3,n,m)
    if(order eq 1) then begin
      for j=0,m-1 do x(*,*,j)=a#xx(*,*,j)
      for j=0,m-1 do x(*,*,j)=b# x(*,*,j)
    endif else begin
      for j=0,m-1 do x(*,*,j)=b#xx(*,*,j)
      for j=0,m-1 do x(*,*,j)=a# x(*,*,j)
    endelse
  endif
  return,x
end


;====================================================================
; This IDL program generates maps of magnetic field and chemical
; composition for magnetic imaging code INVERS10
; Magnetic field can be set in terms of offset dipolar and
; axially symmetric quadrupolar components. The result contains the last
; section of input file for INVERS10 and is written to outfile.
; The input parameters:
;   ntot       - number of stellar surface elements,
;   incl       - inclination of rotation axis (degrees), only for the
;                plot, not used otherwise,
;   beta       - angle between rotation and magnetic axes (degrees),
;   psi        - rotation angle (phase) of North magnetic pole (degrees),
;   decenter   - decentering parameter of dipol/quadrupole normalized
;                by stellar radius,
;   dipole     - polar field of of dipolar component (Gauss),
;   quadrupole - field strength of the quadrupole component (Gauss).
;   octupole   - field strength of the octupole component (Gauss).
;   outfile    - output filename.
;
;  Example: map10,1995,10,20,60,-0.1,10000,2000,-1000,file='out'

Pro map10,ntot,incl,beta,psi,decenter,dipole,quadrupole,octupole,file=outfile

  if(n_params() lt 7) then begin
    print,'map10,ntot,incl,beta,psi,decenter,dipole,quadrupole,octupole[,file=outfile]'
    print,'   ntot       - number of stellar surface elements,'
    print,'   incl       - inclination of rotation axis (degrees), only for'
    print,'                plotting, not used otherwise,'
    print,'   beta       - angle between rotation and magnetic axes (degrees),'
    print,'   psi        - rotation angle (phase) of North magnetic pole (degrees),'
    print,'   decenter   - decentering parameter of dipol/quadrupole normalized'
    print,'                by stellar radius,'
    print,'   dipole     - polar field of of the dipole component (Gauss),'
    print,'   quadrupole - field strength of the quadrupole component (Gauss).'
    print,'   octupole   - field strength of the octupole component (Gauss).'
    print,'   outfile    - output filename.'
    print,''
    print,'  Example: map10,1995,10,20,60,-0.1,10000,2000,-1000,file=''outfile'''
    return
  endif

  ntot=fix(ntot)
  beta=double(beta)
  psi=double(psi)
  decenter=double(decenter)
  dipole=double(dipole)
  quadrupole=double(quadrupole)

; Generate stellar surface grid

  nlat=fix(0.5D0*(1.D0+sqrt(1.D0+!DPI*ntot)))-1
  nlon=intarr(nlat)
  nlatsm=360/nlat+1
  dlat=!DPI/nlat
  xlat=!DPI*(findgen(nlat)+0.5D0)/nlat-!DPI/2.D0
  nn=0
  xcirc=2.D0*!DPI*cos(xlat(1:nlat-1))
  nlon(1:nlat-1)=fix(xcirc/dlat)+2
  nlon(0)=ntot-total(nlon(1:nlat-1))

; Generate Descartes coordinates for the surface grid in stellar coordinates

  x0=dblarr(3,ntot)
  latitude=dblarr(ntot)
  longitude=dblarr(ntot)
  j=0
  for i=0,nlat-1 do begin
    coslat=cos(xlat(i)) & sinlat=sin(xlat(i))
    xlon=2*!DPI*(dindgen(nlon(i))+0.5)/nlon(i)
    sinlon=sin(xlon) & coslon=cos(xlon)
    x0(0,j:j+nlon(i)-1)= coslat*sinlon
    x0(1,j:j+nlon(i)-1)=-coslat*coslon
    x0(2,j:j+nlon(i)-1)= sinlat
    latitude(j:j+nlon(i)-1)=xlat(i)
    longitude(j:j+nlon(i)-1)=xlon
    j=j+nlon(i)
  endfor

; Transform stellar coordinates to magnetic coordinates
; Z-axis is used as field symmetry line.

  x1=Transform(0,beta,psi,x0)

  b1=dblarr(3,ntot)
  if(abs(dipole) gt 0.001) then begin
    D2=(1.0+decenter*decenter-2.0*decenter*x1(2,*))
    D5=sqrt(D2)^2.5
    b1(0,*)=b1(0,*)+dipole*3.0*x1(0,*)*(x1(2,*)-decenter)/(2.0*D5)
    b1(1,*)=b1(1,*)+dipole*3.0*x1(1,*)*(x1(2,*)-decenter)/(2.0*D5)
    b1(2,*)=b1(2,*)+dipole*(3.0*((x1(2,*)-decenter)^2)-D2)/(2.0*D5)
  endif
  if(abs(quadrupole) gt 0.001) then begin
    b1(0,*)=b1(0,*)+quadrupole*x1(0,*)*(5.0*(x1(2,*)^2)-1.0)/2.0
    b1(1,*)=b1(1,*)+quadrupole*x1(1,*)*(5.0*(x1(2,*)^2)-1.0)/2.0
    b1(2,*)=b1(2,*)+quadrupole*x1(2,*)*(5.0*(x1(2,*)^2)-3.0)/2.0
  endif
  if(abs(octupole) gt 0.001) then begin
    b1(0,*)=b1(0,*)+octupole*5.0*(7.0*x1(2,*)^3-3.0*x1(2,*))*x1(0,*)/8.0 
    b1(1,*)=b1(1,*)+octupole*5.0*(7.0*x1(2,*)^3-3.0*x1(2,*))*x1(1,*)/8.0 
    b1(2,*)=b1(2,*)+octupole*(35.0*x1(2,*)^4-30.0*x1(2,*)^2+3.0)/8.0
  endif

;
; Two spots around two symmetric latitudes and logitude 0. In each
; the abundance is decreased while magnetic field is radial and of
; opposite polarity.
;

;  abund=replicate(-4.3d0,ntot)
;  r1=reform(sqrt((x1(0,*))^2+(x1(2,*)-0.4)^2))
;  i1=where(r1 lt 0.3 and reform(x1(1,*)) gt 0,n1)
;  if(n1 gt 0) then begin
;    b1(1,i1) = 4.d3
;    abund(i1)=-3.3
;  endif

;  r2=reform(sqrt((x1(0,*))^2+(x1(2,*)+0.4)^2))
;  i2=where(r2 lt 0.3 and reform(x1(1,*)) gt 0,n2)
;  if(n2 gt 0) then begin
;    b1(1,i2) =-4.d3
;    abund(i2)=-3.3
;  endif

; Transform magnetic field back to stellar coordinates

  b0=Transform(1,-beta,-psi,b1)

  Bmod=sqrt(total(b0^2,1))
  Br=total(x0*b0,1)                  ; Radial field
  Bm=dblarr(ntot)                    ; Meridianal field
  Bp=dblarr(ntot)                    ; Longitudinal field
  for i=0,ntot-1 do begin
    b_local=Transform(0,90-latitude(i)/!DPI*180, $
                           longitude(i)/!DPI*180,b0(*,i))
    Bp(i)=b_local(0)
    Bm(i)=b_local(1)
;    Br(i)=b_local(2)
  endfor
  bphi=acos(Br/Bmod)
  bpsi=atan(Bm,Bp)

; Plot results

  window,0,xs=600,ys=600
  if(!d.n_colors gt 256) then begin
    red  =          255l
    green=     256l*255l
    blue =256l*256l*255l
    white=256l*256l*256l-1
  endif else begin
    tvlct,rc,gc,bc,/get
    white=1 & red=2 & green=3 & blue=4
    rc(white)=255b & gc(white)=255b & bc(white)=255b
    rc(red  )=255b & gc(red  )=  0b & bc(red  )=  0b
    rc(green)=  0b & gc(green)=255b & bc(green)=  0b
    rc(blue )=  0b & gc(blue )=  0b & bc(blue )=255b
    tvlct,rc,gc,bc
  endelse

  !p.position=[0,0,1,1]
  plot,[-1.1,1.1],[-1.1,1.1],/nodata,xs=-1,ys=-1

  bnorm=0.15/max(bmod)
  xx0=Transform(1,incl,0,x0)
  bb0=Transform(1,incl,0,b0)*bnorm
  iplus=where(xx0(2,*) ge 0, nplus)
  iminus=where(xx0(2,*) lt 0)
  oplot,xx0(0,iminus),xx0(1,iminus),psym=3,color=blue
  oplot,xx0(0,iplus ),xx0(1,iplus ),psym=2,color=red
  inward=where(total((xx0(*,iplus)+bb0(*,iplus))^2,1) lt $ 
               total(xx0(*,iplus)^2,1), n_inward)
  for i=0,nplus-1 do begin
    oplot,xx0(0,iplus(i))+[0,bb0(0,iplus(i))], $
          xx0(1,iplus(i))+[0,bb0(1,iplus(i))],color=white
  endfor
  for i=0,n_inward-1 do begin
    oplot,xx0(0,iplus(inward(i)))+[0,bb0(0,iplus(inward(i)))], $
          xx0(1,iplus(inward(i)))+[0,bb0(1,iplus(inward(i)))],color=green
  endfor

; Print output tables

  if(keyword_set(outfile)) then begin
    close,1 & openw,1,outfile

;    printf,1,'abund='
;    i1=0
;    for lat=0,nlat-1 do begin
;      i2=i1+nlon(lat)-1
;      printf,1,form='(10(f6.3,'',''))',abund(i1:i2)
;      i1=i1+nlon(lat)
;    endfor
    printf,1,'fld_R='
    i1=0
    for lat=0,nlat-1 do begin
      i2=i1+nlon(lat)-1
      printf,1,form='(10(f6.3,'',''))',Br(i1:i2)*1.d-3
      printf,1
      i1=i1+nlon(lat)
    endfor
    printf,1,'fld_M='
    i1=0
    for lat=0,nlat-1 do begin
      i2=i1+nlon(lat)-1
      printf,1,form='(10(f6.3,'',''))',Bm(i1:i2)*1.d-3
      printf,1
      i1=i1+nlon(lat)
    endfor

    printf,1,'fld_L='
    i1=0
    for lat=0,nlat-1 do begin
      i2=i1+nlon(lat)-1
      printf,1,form='(10(f6.3,'',''))',Bp(i1:i2)*1.d-3
      printf,1
      i1=i1+nlon(lat)
    endfor
    close,1
    print,'Output is written to file "'+outfile+'"'
  endif
;  stop
end
