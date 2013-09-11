pro precess_all,epch1,hh,mm,ss,dd,dm,ds,rapm,dcpm,epch2
;  This is a routine to precess coordinates of RA (hh,mm,ss) and declination
;  (dd,dm,ds) as well as proper motions in RA (rapm) and dec (dcpm) from
;  some epoch (epch1) to some other epoch (epch2).  The RA proper motion is
;  assumed to be in seconds of time per year and the Dec proper motion is
;  assumed to be in arcsec per year.
;
if n_params() lt 10 then begin
   print,'Syntax is: Precess_all,epch1,hh,mm,ss,dd,dm,ds,rapm,dcpm,epch2'
   retall
endif

ra=360.*(((ss/60.+mm)/60.+hh)/24.)
if dd ge 0. then begin
   dec=(ds/60.+dm)/60.+dd
endif else begin
   dec=-((abs(ds)/60.0+abs(dm))/60.0 + abs(dd))
endelse

precess,ra,dec,epch1,epch2

raoff=(epch2-epch1)*rapm/(3600.*24.)*360.
ra=ra+raoff
dcoff=(epch2-epch1)*dcpm/(3600.)
dec=dec+dcoff

radec,ra,dec,ihr,imin,sec,ideg,idm,xsc

print,'Equinox(',epch2,'): '
print,'RA -- ',ihr,imin,sec
print,'Dec - ',ideg,idm,xsc

end
