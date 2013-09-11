function c24, index
;Set colors for plotting. Note that the number of colors in the color map
;  may change when the first window is created. This can lead to a default
;  plotting color other than white. Either create the window before calling
;  colors, or call colors again once the window has been created.
;27-Mar-94 JAV Created.

;Specify progression of colors.
  r = [ 1.0, 1.0, 0.2, 0.0, 1.0, 1.0, 1.0, 0.2 $
      , 1.0, 0.4, 0.6, 0.6, 1.0, 0.2, 0.6 ]
  g = [ 1.0, 0.0, 1.0, 0.4, 1.0, 0.4, 0.6, 1.0 $
      , 0.8, 0.6, 0.0, 0.4, 0.4, 0.8, 0.6 ]
  b = [ 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 $
      , 0.6, 0.0, 0.6, 0.0, 0.0, 0.4, 0.4 ]
  ncolors = n_elements(r)

;Scale to fill full 8 bit range.
  r = long(255 * r)
  g = long(255 * g)
  b = long(255 * b)

;Set last entry in color map to white.
  imod = index mod ncolors

;Calculate return value.
  if !d.n_colors gt 256 then begin
    return, b(imod)*256L^2 + g(imod)*256L + r(imod)
  endif else begin
    colors
    return, index
  endelse

end
