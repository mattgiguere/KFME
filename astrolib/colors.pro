pro colors
;Set colors for plotting. Note that the number of colors in the color map
;  may change when the first window is created. This can lead to a default
;  plotting color other than white. Either create the window before calling
;  colors, or call colors again once the window has been created.
;27-Mar-94 JAV Created.

;Characterize color map. Initialize new color map.
  nshades = 256					;number of shades
  tblsiz = !d.table_size			;size of color map
  r = intarr(tblsiz)
  g = intarr(tblsiz)
  b = intarr(tblsiz)

;Specify progression of colors.
  rtmp = [ 1.0, 1.0, 0.2, 0.0, 1.0, 1.0, 1.0, 0.2 $
         , 1.0, 0.4, 0.6, 0.6, 1.0, 0.2, 0.6 ]
  gtmp = [ 1.0, 0.0, 1.0, 0.4, 1.0, 0.4, 0.6, 1.0 $
         , 0.8, 0.6, 0.0, 0.4, 0.4, 0.8, 0.6 ]
  btmp = [ 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 $
         , 0.6, 0.0, 0.6, 0.0, 0.0, 0.4, 0.4 ]

;Scale to fill available shades.
  rtmp = fix((nshades-1) * rtmp)
  gtmp = fix((nshades-1) * gtmp)
  btmp = fix((nshades-1) * btmp)

;Replicate color progression and fill color map. First entry is left black.
  tmpsiz = n_elements(rtmp)			;template size
  unit = replicate(1, tblsiz/tmpsiz+1)
  r(1:tblsiz-1) = (rtmp # unit)(0:tblsiz-2)
  g(1:tblsiz-1) = (gtmp # unit)(0:tblsiz-2)
  b(1:tblsiz-1) = (btmp # unit)(0:tblsiz-2)

;Set last entry in color map to white.
  r(tblsiz-1) = nshades-1
  g(tblsiz-1) = nshades-1
  b(tblsiz-1) = nshades-1

;Load color map.
  tvlct,r,g,b

end
