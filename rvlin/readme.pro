;This is a quick sample script showing how to use some of the features of rv_fit_mp
;The heart of the program is rvlin.pro, which is the 'user-supplied function' expected by mpfit.pro.  RVLIN could also be used in something like an MCMC code for GLOBAL fitting of a system with many planets or velocities where a "by hand" or "brute force" search is not desireable.
;
;The linearization scheme and explicit (analytic) derivative magic of RVLIN is described in:
;
;Wright & Howard 2008 ApJ sumbitted.
;
;
;This is an early release.  I suppose you could call it a beta.  Things are not yet fully commented, and bugs surely remain.
;
;
;
;
;
; PLEASE REPORT ALL BUGS TO: Jason Wright <enfolder@gmail.com>
;
;
;
;
;
; RV_FIT_MP is a local fitter.  You'll have to search for planets 'by hand', but if your parameters are close, it will close in on the local chi^2 minimum very efficiently.  You need only supply the period closely in practice, but may also supply starting guesses for eccentricity and periastron passage.   Other parametrs are fit exactly, and cannot be given starting guesses.  
;
;The only case where you can fit "Blind" with no guesses, in the case of a single strong planetary signal.
;In this case, the code will run a periodogram on the data and use the tallest peak as its starting guess.
;
;
;To use this sample script, type: .run readme
;
;It will assume that it being called from a directory that contains 12661.vel.txt (you can use IDL's 'cd' procedure to change your directory) and that all of the programs in the exoplanets.org/code/rvlin directory are in your IDL path.
;
;
;You will need the Goddard IDL astronomy user's library, and MPFIT.PRO from Craig Marqwardt:
;http://idlastro.gsfc.nasa.gov/
;http://cow.physics.wisc.edu/~craigm/idl/fitting.html
;
;
;First, we use the readcol routine to read in the sample data file.  READCOL is available from the Goddard IDL library, which you will need to run these routines, and has other dependencies from the Goddard library.

readcol, '12661.vel.txt', t, v, e, tel, format = '(d,d,d,a)'  ;Read the sample data file

;
;Next we'll just try to fit blind.  This can only be done when there is a strong signal.
;
bestpars = rv_fit_mp(t, v, e, telvec = tel, offset = offset) ;first planet
;
; The telvec parameter is telling rv_fit_mp which telescope provided which velocities.  Offset is returning the DC offset between the two telescopes in this example (Lick and Keck).
;
;
; Now, let's look at the residuals.
;
vres = v-rv_drive(t, bestpars)
;
; RV_DRIVE takes the output of rv_fit_mp and produces a synthetic RV curve
;
; OK, let's ask rv_fit_mp to fit the residuals blind to find a second planet.
;
bestpars2 = rv_fit_mp(t, vres, e, telvec = tel)
;
;Now that we have a good fit, we can go for a joint, 2-planet fit
;We use the orbel keyword to provide the starting guess -- a concatination of the 1- and 2-planet fits
;
bestpars = rv_fit_mp(t, v, e, telvec = tel, orbel = [bestpars, bestpars2], tveout = tve)
;
;tveout returns a structure containing the times, velocities, and errors of the joint fit WITH THE OFFSET BETWEEN THE TELESCOPES APPLIED.
;
;
; This allows us to plot up the joint telescope, 2-planet fit along with the best-fit RV curve:
;
len = (max(t)-min(t))
m = len*1.2*findgen(1000)/999.+min(t)-len*0.1
plot, tve.jd,  tve.mnvel, ps = 2
oplot,  m, rv_drive(m, bestpars)
;
;Ta-da.
;
end
