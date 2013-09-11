pro autozap, infile, outfile, npix=npix, auto=auto, notwo=notwo
;Automatically identifies cosmic rays in McDonald R=120,000 spectra.
;Interactively allows user to view and modify changes to spectrum.
;Full automatic mode available via the /auto keyword.
;Inputs:
; infile (string) name of a .isp file containing spectrum to be zapped
; outfile (string) name of a .isp file in which to write zapped spectrum
; npix (scalar) maximum number of pixels to include in any given plot
; /auto (switch) forces full automatic mode - supresses all interaction
; /notwo (switch) supresses check for two pixel wide blemishes
;Algorithm:
; Complicated and very heuristic. Uses the first derivatives of pixel pairs
; separated by 1-4 pixels. Basically looks for places where the first
; derivative is relatively "large" and changes sign across a 1 or 2 pixel
; shift. However, line cores also satisfy this condition, so do not zap
; pixels where first derivatives are just as large 1 pixel further out on
; either side. These criterea work pretty well because line profiles are
; still getting steeper 1-2 pixels from the core, whereas the unblemished
; spectrum at these pixel offsets is usually flatter than the blemish itself.
;History:
; 03-Jun-97 Valenti  Finally got this #%@& thing working!

if n_params() lt 2 then begin
  print, 'syntax: autozap, infile, outfile [,npix= ,/auto ,/notwo]'
  retall
endif
if not keyword_set(npix) then npix = 256

;Read spectrum from disk.
  rdisp, s, infile, obase=obase
  sz = size(s)
  ncol = sz(1)
  nord = sz(2)
  nseg = ceil(ncol/float(npix))

;Loop through orders.
  for j=0, nord-1 do begin
    ss = s(*,j)
    ssorig = ss
    nfix = 0

;Fix 1-pixel wide blemishes.
    fdss = ss(1:ncol-1) - ss(0:ncol-2)
    sdss = fdss(1:ncol-2) - fdss(0:ncol-3)
    filt1 = -fdss(1:ncol-4)*fdss(2:ncol-3)
    filt2 = -fdss(0:ncol-5)*fdss(3:ncol-2)
    filt = (filt1*(filt1 gt filt2)) > 0
    iwhr = where(filt gt 0, nwhr)
    if nwhr gt 0 then begin
      isort = sort(filt(iwhr))
      thresh = 2.0 * filt(iwhr(isort(0.90*nwhr)))
      iwhr = 1 + where(filt gt thresh, nwhr)
      if not keyword_set(auto) then begin
        plot, alog10(1+filt), /xsty, ysty=3 $
	    , yr=alog10(1+[0>(thresh/10), max(1+filt)])
        oplot, !x.crange, alog10([1,1]+thresh), co=2
        oplot, !x.crange, alog10([1,1]+thresh/2.0), co=2, li=2
        wait, 0.5
        cursor, junk, junk, wait=3
      endif
      for k=0, nwhr-1 do begin
        m = iwhr(k) + 1
        ss(m) = 0.5 * (ss(m-1) + ss(m+1))
      endfor
      nfix = nfix + nwhr
    endif

;Fix 2-pixel wide blemishes. Be more conservative.
    fdss = ss(1:ncol-1) - ss(0:ncol-2)
    sdss = fdss(1:ncol-2) - fdss(0:ncol-3)
    filt1 = -fdss(1:ncol-5)*fdss(3:ncol-3)
    filt2 = -fdss(0:ncol-6)*fdss(4:ncol-2)
    filt = (filt1*(filt1 gt filt2)) > 0
    iwhr = where(filt gt 0, nwhr)
    if nwhr gt 0 and not keyword_set(notwo) then begin
      isort = sort(filt(iwhr))
      thresh = 4.0 * filt(iwhr(isort(0.98*nwhr)))
      iwhr = 1 + where(filt gt thresh, nwhr)
      if not keyword_set(auto) then begin
        plot, alog10(1+filt), /xsty, ysty=3 $
	    , yr=alog10(1+[0>(thresh/10), max(1+filt)])
        oplot, !x.crange, alog10([1,1]+thresh/4.0), co=4, li=2
        oplot, !x.crange, alog10([1,1]+thresh), co=4
        wait, 0.5
        cursor, junk, junk, wait=3
      endif
      for k=0, nwhr-1 do begin
        m = iwhr(k) + 1
        ss(m)   = 0.5 * (ss(m-1) + ss(m+2))
        ss(m+1) = 0.5 * (ss(m-1) + ss(m+2))
      endfor
      nfix = nfix + nwhr
    endif

;Loop through segments within this order.
    print, string(nfix, form='(i6)') + ' pixels fixed' $
         + ' in order' + string(j+1, form='(i4)')
    if not keyword_set(auto) then begin
      for k=0, nseg-1 do begin
        ib = 0 > (k*npix-10)
        ie = ((k+1)*npix-1+10) < (ncol-1)
        sss = ss(ib:ie)
        sssorig = ssorig(ib:ie)

;Use mouse inside plot border to change pixels (left: zap,  right: unzap).
;Use mouse outside plot border to continue to next segment.
        flag = 0
        repeat begin
          if flag eq 1 then begin
            m = round(cx)
            if !err eq 1 then begin
              sss(m) = 0.5 * (sss(m-1) + sss(m+1))
            endif else begin
              sss(m) = sssorig(m)
            endelse
          endif
          plot, sssorig, ps=10, /yno, xsty=3, /nodata
          oplot, sssorig, ps=10, co=3
          oplot, sss, ps=10
          wait, 0.3
          cursor, cx, cy, wait=3
          flag = 1
        endrep until cx lt !x.crange(0) or cx gt !x.crange(1) $
                  or cy lt !y.crange(0) or cy gt !y.crange(1)

        ss(ib:ie) = sss				;replace old segment
      endfor
    endif

    s(*,j) = ss					;replace old order
  endfor

;Write output file.
  wdisp, s, outfile, obase=obase

end
