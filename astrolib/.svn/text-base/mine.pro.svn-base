pro mine, nx=nx, ny=ny, nmine=nmine, help=help
;Minesweeper game.
;Written by Jeff Valenti on July 4, 1996.

;Print help.
  if keyword_set(help) then begin
    print, 'syntax: mine [,nx= ,ny= ,nmine= ,/help]'
    print, '  left button: probe current square'
    print, '  middle button: probe adjacent squares'
    print, '  right button: mark/unmark mine'
    print, 'click outside board to quit'
  endif

;Parameters
  if not keyword_set(nx) then nx=15
  if not keyword_set(ny) then ny=15
  if not keyword_set(nmine) then nmine=40
  offset=0.3
  sz=1.6

;Place mines.
  mark = replicate(-1,nx,ny)
  mines = intarr(nx,ny)
  for i=1,nmine do begin
    repeat begin
      indx = floor(nx*ny*randomu(seed,1))
      indx = indx(0)
    endrep until mines(indx) eq 0
    mines(indx) = 1
  endfor

  plot, [0,nx-1], [0,ny-1], /nodata $
      , xsty=4, ysty=4, ymarg=[2,2], xmarg=[4,4]
  for i=0, nx do oplot, [i,i], !y.crange
  for i=0, ny do oplot, !x.crange, [i,i]

  firstpass = 1
  print, 'There are ' + strtrim(nmine,2) + ' mines in the ' $
    + strtrim(nx,2) + ' x ' + strtrim(ny,2) + ' grid.'

  repeat begin
    cursor, x, y, wait=3
    if firstpass eq 1 then begin
      starttime = systime(1)
      firstpass = 0
    endif
    ix = floor(x)
    iy = floor(y)
    butt = !err

    if ix lt 0 or ix ge nx or iy lt 0 or iy ge ny then begin
      yorn = ''
      read, 'Really quit (y/n)? ', yorn
      if strlowcase(yorn) eq 'y' then return
    endif else begin

      if butt eq 1 then begin
        if mines(ix,iy) eq 1 then begin
          for disp=-0.05,0.05,0.01 do begin
            xyouts, ix+0.50+disp, iy+0.75*offset, align=0.5 $
              , size=1.5*sz, 'X'
          endfor
          print, 'Boom!!  You lose.'
          return
        endif else begin
          x0 = (ix-1) > 0
          x1 = (ix+1) < (nx-1)
          y0 = (iy-1) > 0
          y1 = (iy+1) < (ny-1)
          nm = round(total(mines(x0:x1,y0:y1)))
          xyouts, ix+0.5, iy+offset, align=0.5, size=sz $
            , strtrim(nm,2)
          mark(ix,iy) = nm
        endelse
      endif

      if butt eq 4 then begin
        if mark(ix,iy) eq -2 then begin
          polyfill, [ix+0.1,ix+0.1,ix+0.9,ix+0.9] $
                  , [iy+0.1,iy+0.9,iy+0.9,iy+0.1], co=0
          mark(ix,iy) = -1
        endif else begin
          if mark(ix,iy) eq -1 then begin
            for disp=-0.05,0.05,0.01 do begin
              xyouts, ix+0.50+disp, iy+0.75*offset, align=0.5 $
                , size=1.5*sz, 'X'
            endfor
            mark(ix,iy) = -2
          endif
        endelse
      endif

      if butt eq 2 and mark(ix,iy) ge 0 then begin
        x0 = (ix-1) > 0
        x1 = (ix+1) < (nx-1)
        y0 = (iy-1) > 0
        y1 = (iy+1) < (ny-1)

;Check marked mines.
        imark = where(mark(x0:x1,y0:y1) eq -2, nmark)
        if nmark ne mark(ix,iy) then begin
          if nmark lt mark(ix,iy) then begin
            print, 'You must first find all adjacent mines!'
          endif else begin
            print, 'You have have marked too many adjacent mines!'
          endelse
        endif else begin

;Check for unmarked mine.
          iwhr = where(mark(x0:x1,y0:y1) eq -1 $
                 and mines(x0:x1,y0:y1) eq 1, nwhr)
          if nwhr gt 0 then begin
            print, 'You misidentified a mine and then' $
                 + ' exploded a hidden one!
            return
          endif

;Probe all unprobed squares.
          for jx=x0,x1 do begin
            for jy=y0,y1 do begin
              if mark(jx,jy) eq -1 then begin
                x2 = (jx-1) > 0
                x3 = (jx+1) < (nx-1)
                y2 = (jy-1) > 0
                y3 = (jy+1) < (ny-1)
                nm = round(total(mines(x2:x3,y2:y3)))
                xyouts, jx+0.5, jy+offset, align=0.5, size=sz $
                  , strtrim(nm,2)
                if nm gt 0 then begin
                  mark(jx,jy) = nm
                endif else begin
                  mark(jx,jy) = -3
                endelse
              endif
            endfor
          endfor

;Probe squares around recently exposed "0" squares.
          izer = where(mark eq -3, nzer)
          while nzer gt 0 do begin
            jx = izer(0) mod ny
            jy = izer(0) / ny
            x0 = (jx-1) > 0
            x1 = (jx+1) < (nx-1)
            y0 = (jy-1) > 0
            y1 = (jy+1) < (ny-1)
            mark(jx,jy) = 0
            for jx=x0,x1 do begin
              for jy=y0,y1 do begin
                if mark(jx,jy) eq -1 then begin
                  x2 = (jx-1) > 0
                  x3 = (jx+1) < (nx-1)
                  y2 = (jy-1) > 0
                  y3 = (jy+1) < (ny-1)
                  nm = round(total(mines(x2:x3,y2:y3)))
                  xyouts, jx+0.5, jy+offset, align=0.5, size=sz $
                    , strtrim(nm,2)
                  if nm gt 0 then begin
                    mark(jx,jy) = nm
                  endif else begin
                    mark(jx,jy) = -3
                  endelse
                endif
              endfor
            endfor
            izer = where(mark eq -3, nzer)
          endwhile

        endelse
      endif

      iwhr = where(mark eq -1, nwhr)
      if nwhr eq 0 then begin
        endtime = systime(1)
        time = endtime - starttime
        print, 'Congratulations!  You win!!'
        print, 'Your cleared the field in ' $
             + strtrim(round(time), 2) + ' seconds.'
        return
      endif

      wait, 0.2
    endelse
  endrep until 0 eq 1

end
