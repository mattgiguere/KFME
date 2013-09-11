pro tiff2ps, filein, fileout, eps=eps
;Convert a TIFF file to postscript.

if n_params() lt 2 then begin
  print, 'syntax: tiff2ps, filein, fileout [,/eps]'
  retall
endif

  spawn, 'xloadimage ' + filein + ' -dump jpeg scratch.jpeg'
  read_jpeg, 'scratch.jpeg', im

  set_plot, 'ps'
  device, file=fileout
  device, bits_per_pixel=1
  device, /isolatin1
  device, encapsulated=keyword_set(eps)
  device, /portrait
  device, xoff=1, yoff=1, xsize=6.5, ysize=9.0, /inch

  tv, im, /device

  device, /close
  set_plot, 'X'

end
