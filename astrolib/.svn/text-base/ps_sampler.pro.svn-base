pro ps_sampler

  ps_open, 'ps_sampler', /ps, /port
  device, xsize=8.0, ysize=9, xoff=0.25, yoff=1.00, /inches

  text = 'The Quick Brown Fox Jumped Over the Lazy Dog.'
  plot, [0,1], xsty=4, ysty=4, xmarg=[0,0], ymarg=[0,0], /nodata
  x1 = 0.05
  x2 = 0.67
  x3 = 0.50
  y = 0.98
  dy = 0.025
  device, /courier
  xyouts, x1, y, text
  xyouts, x2, y, '/courier <!!11>'
  y = y - dy
  device, /courier,/bold
  xyouts, x1, y, text
  xyouts, x2, y, '/courier,/bold'
  y = y - dy
  device, /courier,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/courier,/oblique <!!12>'
  y = y - dy
  device, /courier,/bold,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/courier,/bold,/oblique'
  y = y - dy

  device, /helvetica
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica <!!3>'
  y = y - dy
  device, /helvetica,/bold
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/bold <!!4>'
  y = y - dy
  device, /helvetica,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/oblique'
  y = y - dy
  device, /helvetica,/bold,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/bold,/oblique'
  y = y - dy

  device, /helvetica,/narrow
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/narrow <!!5>'
  y = y - dy
  device, /helvetica,/bold,/narrow
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/bold,/narrow'
  y = y - dy
  device, /helvetica,/oblique,/narrow
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/oblique,/narrow'
  y = y - dy
  device, /helvetica,/bold,/oblique,/narrow
  xyouts, x1, y, text
  xyouts, x2, y, '/helvetica,/bold,/oblique,/narrow <!!6>'
  y = y - dy

  device, /avantgarde,/book
  xyouts, x1, y, text
  xyouts, x2, y, '/avantgarde,/book <!!17>'
  y = y - dy
  device, /avantgarde,/book,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/avantgarde,/book,/oblique'
  y = y - dy
  device, /avantgarde,/demi
  xyouts, x1, y, text
  xyouts, x2, y, '/avantgarde,/demi'
  y = y - dy
  device, /avantgarde,/demi,/oblique
  xyouts, x1, y, text
  xyouts, x2, y, '/avantgarde,/demi,/oblique'
  y = y - dy

  device, /bkman,/demi
  xyouts, x1, y, text
  xyouts, x2, y, '/bkman,/demi'
  y = y - dy
  device, /bkman,/demi,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/bkman,/demi,/italic'
  y = y - dy
  device, /bkman,/light
  xyouts, x1, y, text
  xyouts, x2, y, '/bkman,/light'
  y = y - dy
  device, /bkman,/light,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/bkman,/light,/italic'
  y = y - dy

  device, /zapfchancery,/medium,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/zapfchancery,/medium,/italic'
  y = y - dy
  device, /zapfdingbats
  xyouts, x1, y, text
  device, /helvetica
  xyouts, x2, y, '(/zapfdingbats <!!10>)'
  y = y - dy

  device, /schoolbook
  xyouts, x1, y, text
  xyouts, x2, y, '/schoolbook <!!18>'
  y = y - dy
  device, /schoolbook,/bold
  xyouts, x1, y, text
  xyouts, x2, y, '/schoolbook,/bold <!!19>'
  y = y - dy
  device, /schoolbook,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/schoolbook,/italic'
  y = y - dy
  device, /schoolbook,/bold,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/schoolbook,/bold,/italic'
  y = y - dy

  device, /palatino
  xyouts, x1, y, text
  xyouts, x2, y, '/palatino <!!13>'
  y = y - dy
  device, /palatino,/bold
  xyouts, x1, y, text
  xyouts, x2, y, '/palatino,/bold <!!15>'
  y = y - dy
  device, /palatino,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/palatino,/italic <!!14>'
  y = y - dy
  device, /palatino,/bold,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/palatino,/bold,/italic <!!16>'
  y = y - dy

  device, /symbol
  xyouts, x1, y, text
  device, /helvetica
  xyouts, x2, y, '(/symbol <!!9>)'
  y = y - dy

  device, /times
  xyouts, x1, y, text
  xyouts, x2, y, '/times <!!7>'
  y = y - dy
  device, /times,/bold
  xyouts, x1, y, text
  xyouts, x2, y, '/times,/bold'
  y = y - dy
  device, /times,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/times,/italic'
  y = y - dy
  device, /times,/bold,/italic
  xyouts, x1, y, text
  xyouts, x2, y, '/times,/bold,/italic <!!8>'
  y = y - dy

  device, user_font='NewCenturySchlbk-BoldItalic'
  device, /helvetica,/narrow
  y = y - dy
  xyouts, x1, y, "IDL> set_plot, 'PS'"
  xyouts, x3, y, ";these are all postscript commands"
  y = y - dy
  xyouts, x1, y, "IDL> device, /helvetica,/oblique"
  xyouts, x3, y, ";select a new default font"
  y = y - dy
  xyouts, x1, y, "IDL> xyouts, x, y, '!!19!19Use /schoolbook,/bold!x!!x'"
  xyouts, x3, y, ";temporarily change fonts"
  y = y - dy
  xyouts, x1, y, "IDL> device, user_font='NewCenturySchlbk-BoldItalic'"
  xyouts, x3, y, ";set font !!20 (see Unix command !20xlsfonts!x)"

  ps_close

end
