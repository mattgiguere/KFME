pro parensig_test, ndig, tex=tex, idl=idl, neg=neg

if n_params() lt 1 then begin
  print, 'syntax: parensig_test, ndig [,/tex ,/idl ,/neg]'
  retall
endif

  values = 1.23456789d0 * 10d0^(indgen(15)-7)
  sigmas = 3.456789d0 * 10d0^(indgen(15)-7-ndig+2)

  if keyword_set(neg) then values = -values

  for i=0, n_elements(values)-1 do begin
    print, values(i), sigmas(i), '   ' $
         , parensig(values(i), sigmas(i), tex=keyword_set(tex) $
                                        , idl=keyword_set(idl))
  endfor

end
