pro detshift,x,y,range,gshft,pshft,plot=plot,focus=focus,nogauss=nogauss
;
; This program is designed to determine a pixel shift between two data
; sets by first calculating the cross-correlation of the two vectors
; and fitting this cross correlation with a Gaussian or a parabola to
; find the subpixel level of the shift.
; Both the Gaussian and parabolic estimates of the shift can be returned.  It
; is recommended that the user select the plotting keyword to examine
; the results in order to select the best estimate.  A positive shift
; means that the lines in y appear to the right of those in x, or put
; another way, y lags x.  The keyword focus can be set to narrow down on the
; peak in the cross correlation function when fitting the functions so that
; the same number of pixels on either side of the peak are used in the 
; actual fits.
; INPUTS:
;   x - data set number 1
;   y - data set number 2
;   range - the one sided range of pixels to try offsets of.  The program
;           will search from -range to +range
;
; OUTPUTS:
;   gshft - the shift estimated by fitting a gaussian to the cross
;           correlation
;   pshft - the shift estimated by fitting a parabola to the cross
;           correlation
;  
; KEYWORDS
;   plot - if set will plot the cross correlation function and the
;          two fits.
;   focus - can be set to a value to focus in on the peak of the cross
;           correlation function for the purpose of fitting the curves
;           to the peak.  When set, the routine will fit to the position
;           of the peak using the pixels within +/- focus of the peak.
;           Typically, the value of focus is less than the value of
;           range.
;
; 04-Sep-98 CMJ Written
; 31-Dec-98 CMJ Added keyword focus and associated code to fit a restricted
;               region around the peak of the cross correlation peak.  Also
;               changed default logic so that it always centers on the peak
;               in the cross correlation function when fitting.  If focus
;               is not set, range is used.
; 12-May-99 JAV Added /nogauss keyword for cases when Gaussian is hopeless.
;

if n_params() lt 4 then begin
   print,'Syntax is: detshift,x,y,range,gshft[,pshft,/plot,focus=,/nogauss]'
   return
endif

gauss = not keyword_set(nogauss)		; true if guassian fit needed
range = fix(round(range))                       ; make it an integer for sure
lag = indgen(2.*range+1)-range                  ; make the vector of lags
ccor = c_correlate(x,y,lag)                     ; get the cross correlation
if keyword_set(focus) then begin                ; focus in?
   if focus ge range then begin                 ; make sure smaller range
      message,'Range for focussing larger than original range',/info
      message,'Using full original range',/info
      ibeg=0                                    ; set lower bound
      iend=fix(2.*range)                        ; set upper bound
   endif else begin
      peak=max(ccor,imax)                       ; find the peak
      if imax lt focus then begin               ; too close to the left?
         message,/info,'Peak too close to left edge'
         message,/info,'Starting from left edge of cross correlation function'
         ibeg=0                                 ; set lower bound
      endif else begin
         ibeg=imax-fix(focus)                   ; set lower bound
      endelse
      iend=imax+fix(focus)                      ; set upper bound
      if fix(2.*range) lt iend then begin       ; too close to right?
         message,/info,'Peak too close to right edge'
         message,/info,'Using to right edge of cross correlation function'
         iend=fix(2.*range)                     ; resetting upper bound
      endif
   endelse
endif else begin
   ibeg=0                                       ; set lower bound
   iend=fix(2.*range)                           ; set upper bound
endelse
if gauss then begin
  gfit = gaussft((float(lag))(ibeg:iend),ccor(ibeg:iend),gc)  ; fit gaussian
endif 
pc = poly_fit((float(lag))(ibeg:iend),ccor(ibeg:iend),2,/double)
                                                ; fit the parabola
parabcoff,pc,pc2                                ; get proper coefficients
if gauss then gshft = gc(1) else gshft = -999.0 ; load gaussian shift
pshft = pc2(1)                                  ; load parabolic shift

if keyword_set(plot) then begin                 ; do we plot?
   tt='Histogram: Data, dash-dot: gaussin, dashed: parabola'
   plot,lag,ccor,ps=10,/xsty,tit=tt             ; make plot
   if gauss then oplot,lag(ibeg:iend),gfit,lin=3  ; overplot gaussian
   oplot,lag(ibeg:iend),poly(float(lag(ibeg:iend)),pc),lin=2 ; overplot parab
endif

end
