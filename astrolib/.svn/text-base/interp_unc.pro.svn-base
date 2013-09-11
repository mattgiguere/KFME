pro interp_unc, xold, yold, uold, xnew, ynew, unew, ilo=ilo, trim=trim
;Linearly interpolate data on a monotonically increasing input grid onto
; a monotonically increasing output grid. Also propagate uncertainties for
; input data to calculate uncertainties for output data.
;
;Inputs:
; xold (vector(nold)) monotonically increasing input grid
; yold (vector(nold)) input data for each point in xold
; uold (vector(nold)) uncertainties for input data in yold
; [/trim] (switch) causes input xnew and output ynew and unew to consist
;   only of the overlap region between xold and xnew.
;
;Input/Output:
; xnew (vector(nnew)) monotonically increasing output grid. Will be trimmed
;   if /trim keyword is specified and xnew is not fully contained in xold.
;
;Outputs:
; ynew (vector(nnew)) output data for each point in xnew
; unew (vector(nnew)) uncertainties for output data in ynew
; [ilo=] (vector(nnew)) for each pixel in wnew (before trimming), ilo
;   contains the index of the pixel in xold less than or equal to the
;   value of the current pixel in xnew. Points off the beginning edge
;   are flagged with (invalid) index values of -1, while points off the
;   ending edge are flagged with (invalid) index values of -2.
;
;History:
; 09-Sep-2000 Valenti  Initial coding.

if n_params() lt 6 then begin
  print, 'syntax: interp_unc, xold, yold, uold, xnew, ynew, unew' $
       + ' [,ilo= ,/trim]'
  return
endif

;Check sizes of input arguments.
  nold = n_elements(xold)
  nnew = n_elements(xnew)
  if nold lt 2 then begin
    message, /info, 'xold must contain at least two points'
    return
  endif
  if n_elements(yold) ne nold then begin
    message, /info, 'yold must have same length as xold'
    return
  endif
  if keyword_set(uold) then begin
    if n_elements(uold) ne nold then begin
      message, /info, 'uold must be zero or have same length as xold'
      return
    endif
  endif

;Check that abscissae are monotonically increasing.
  dxold = xold(1:nold-1) - xold(0:nold-2)
  if min(dxold) le 0 then begin
    message, /info, 'xold must be monotonically increasing'
    return
  endif
  if nnew gt 1 then begin
    dxnew = xnew(1:nnew-1) - xnew(0:nnew-2)
    if min(dxnew) le 0 then begin
      message, /info, 'xnew must be monotonically increasing'
      return
    endif
  endif

;Check that wnew is at least partially contained within wold.
  if xnew(nnew-1) lt xold(0) or xnew(0) gt xold(nold-1) then begin
    message, /info, 'xold and xnew do not overlap at all'
    return
  endif

;Find starting and ending pixels in xnew that are contained in xold.
  ilap = where(xnew ge xold(0) and xnew le xold(nold-1), nlap)
  if nlap eq 0 then message, 'logic error in program - no overlap'
  inlo = ilap(0)			;first new pixel to interpolate
  inhi = ilap(nlap-1)			;last new pixel to interpolate


;For each new pixel, find index of old pixel that brackets on the low side.
;An output index of -1 means the pixel in xnew is off beginning of xold.
;An output index of -2 means the pixel in xnew is off end of xold.
  ilo = replicate(-2L, nnew)		;vector of low side bracket indexes
  isort = sort( [xold, xnew] )		;interleave old and new pixels
  dummy = where(xnew ge xold(nold-1), nend)	;pixels off end of xold
  io = -1
  for i=0L, nold+nnew-nend-1 do $	;loop thru filling in indexes
    if isort(i) ge nold $
      then ilo(isort(i)-nold) = io $
      else io = isort(i)
  dummy = where(xnew gt xold(nold-1), nbeg)	;pixels off beginning of xold

;Determine fractional weights for low and high bracket pixels.
  ib = nbeg				;beginning of overlap region in xnew
  ie = nnew - nend - 1			;end of overlap region in xnew
  frac = xnew(ib:ie) - xold(ilo(ib:ie))	;fraction step between old pixels

;Init ynew with xold(0), which preserves variable type and handles points off
;the beginning edge. Fill overlap region with interpolated values and points
;off ending edge with xold(nold-1).
  ynew = replicate(yold(0), nnew)	;init output vector with yold(0)
  ynew(ib:ie) = (1-frac) * yold(ilo(ib:ie)) $
              + frac * yold(ilo(ib:ie)+1)	;linear interpolation
  if nend gt 0 then ynew(ie+1:nnew-1) = yold(nold-1)	;fix point off end

;Calculated uncertainties using propagation of errors. For points off either
;edge, just copy the uncertainty for the input pixel value.
  if keyword_set(uold) then begin
    unew = replicate(uold(0), nnew)	;init output vector with uold(0)
    unew(ib:ie) = sqrt( ((1-frac) * uold(ilo(ib:ie)))^2 $
                      + (frac * uold(ilo(ib:ie)+1))^2   )
    if nend gt 0 then unew(ie+1:nnew-1) = uold(nold-1)	;fix point off end
  endif

;Trim to overlap region, if requested.
  if keyword_set(trim) then begin
    xnew = xnew(ib:ie)
    ynew = ynew(ib:ie)
    unew = unew(ib:ie)
  endif

end
