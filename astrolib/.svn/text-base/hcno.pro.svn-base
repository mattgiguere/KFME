pro hcno, temp, pfrac, spec, keq
;Equation of state solver for H, C, N, O, and certain molecules.
;See Mihalas (1967, Methods of Computational Physics, v. 7, pp. 9-15).

if n_params() lt 2 then begin
  print, 'syntax: hcno, temp, pfrac [,spec ,keq]'
  retall
endif

;General constants.
  kboltz = 1.3806580e-16

;Atomic data.
  atoms = [ 'H',    'He',   'C',    'N',    'O',    'Na',   'Mg'   $
          , 'Al',   'Si',   'S',    'K',    'Ca',   'Cr',   'Fe'   ]
  abund = [ 1.0000, 1.5e-1, 5.3e-4, 9.6e-5, 9.1e-4, 2.0e-6, 2.6e-5 $
          , 1.6e-6, 3.2e-5, 2.0e-5, 5.0e-8, 1.4e-6, 7.8e-7, 3.7e-6 ]
  ionp1 = [ 13.595, 24.581, 11.256, 14.530, 13.614,  5.138,  7.644 $
          ,  5.984,  8.149, 10.357,  4.339,  6.111,  6.764,  7.87  ]
  ionp2 = [ -999.0, 54.403, 24.376, 29.593, 35.108, 47.290, 15.031 $
          , 18.823, 16.34,  23.400, 31.81,  11.868, 16.49,  16.18  ]
  pfr10 = [  0.00,   0.60,   0.10,   0.62,  -0.05,   0.00,   0.60  $
          , -0.48,   0.12,  -0.05,   0.00,   0.60,  -0.07,   0.38  ]
  pfr21 = [ -99.0,   0.00,  -0.48,   0.12,   0.65,   1.08,   0.00  $
          ,  0.60,  -0.48,   0.65,   1.08,   0.00,   0.92,   0.22  ]

end
