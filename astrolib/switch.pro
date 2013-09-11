pro switch,x,y
;Simply switches the contents of two variables.
;Input value of x is returned in y. Input value of y is returned in x.
;03-Sep-92 JAV	Create.

if n_params() lt 2 then begin
  print,'syntax: switch,x,y'
  retall
endif

  tmp = y				;save y in temporary variable
  y = x					;move contents of x into y
  x = tmp				;move original y contents into x

end
