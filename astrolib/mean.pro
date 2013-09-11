function mean, x

if n_params() lt 1 then begin
	print,'MEAN, x'
endif

return,total(x)/n_elements(x)
end

