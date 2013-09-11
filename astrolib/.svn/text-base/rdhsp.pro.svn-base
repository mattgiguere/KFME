pro rdhsp, spec,file

if n_params() lt 2 then begin
	print,'RDHSP, spec,file(.hsp assummed)'   
return
endif
;ReaDs Hamilton SPectrum from an integer*2 format .HSP file.
;SPEC: (output array (# columns, # orders)) array of spectral orders read
;from file
;FILE:  (input string) name of file from which to RESTORE spectrum. 
;There are 8 extra bytes at the beginning of each record. 
;These bytes contain the two transformation factors
;BSCALE and BZERO in real*4 format. These factors are used to convert the
;integers in the corresponding record into floating point
;according to the formula:
;		real = (BSCALE * integer) + BZERO
;01-Dec-89 JAV Create.
;22-Dec-89 GB Make '.HSP' the default extension

	restana,spec,file + '.hsp'		;call restana (restore)
	sz = size(spec)
	ncol = sz(1)				;number of columns
	nord = sz(2)				;number of orders (rows)

;Extract BSCALE & BZERO transformation factors for each order.
	bscale =(float(spec(0:1,*),0,nord))/4	;converted to real*4 format
	bzero = (float(spec(2:3,*),0,nord))/4	;converted to real*4 format

;Trim first four bytes of each order.
	spec = spec(4:ncol-1,*)			;trim 1st four bytes
	spec = float(spec)			;convert to floating point

;Loop through ouders and apply transformation to restore proper values.
	for i=0,nord-1 do begin
		ord = spec(*,i)
		ord = (bscale(i) * ord) + bzero(i)	;apply transformation
		spec(0,i) = ord
	endfor
end


