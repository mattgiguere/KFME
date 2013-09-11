function is_number, in

  d = datatype(in)
  if d eq 'FLO' or d eq 'COM' or d eq 'DOU' or d eq 'INT' or d eq 'U64' or d eq 'L64' or d eq 'LON' or d eq 'UIN' or d eq 'BYT' or d eq 'DCO' or d eq 'ULO' then return, 1

end
