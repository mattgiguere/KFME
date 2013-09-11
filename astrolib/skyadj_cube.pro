;+
; NAME:                    
;      SKYADJ_CUBE
; PURPOSE:
;       Sky adjust the planes of a datacube.
;
; EXPLANATION:
;       When removing cosmic rays from a set of images, it is desirable that
;       all images have the same sky level.    This procedure (called by
;       CR_REJECT) removes the sky from each image in a data cube.    
;
; CALLING SEQUENCE:
;
;       SKYADJ_CUBE,Datacube,Skyvals,Totsky
;
; INPUT/OUTPUT PARAMETER:
;       Datacube:  3-D array with one image of same field in each plane.
;                  Returned with sky in each plane adjusted to zero.
;
; OUTPUT ARGUMENTS:
;       Skyvals:   Array of sky values used on each plane of datacube.
;                  For a scalar sky, this parameter is a vector
;                  containing the sky value for each image plane.  For a
;                  vector sky, this parameter is a 2-D array where each
;                  line corresponds to one image plane.
;
; INPUT KEYWORD PARAMETERS:
;
;       REGION   - [X0,X1,Y0,Y1] to restrict area used for computation
;                  of sky.  Default is 0.1*Xdim, 0.9*Xdim, 0.1*Ydim,
;                  0.9*Ydim.  If INPUT_MASK is specified, this parameter is
;                  ignored.
;       VERBOSE  - Flag.  If set, print information on skyvals.
;       NOEDIT   - Flag.  If set, return sky values without changing
;                  datacube.
;       XMEDSKY  - Flag.  If set, return vector sky as a function of X.
;       SELECT   - Array of subscripts of planes of the cube to process.
;                  (Default=all)
;       EXTRAPR  - Applies only in XMEDSKY mode.
;                  Subregion to use for polynomial extrapolation of sky
;                  vector into portions excluded by REGION parameter.
;                  (Default=first and last 10% of pixels; set to zero
;                  to defeat extrapolation)
;       EDEGREE  - Applies only in XMEDSKY mode.  
;                  Degree of polynomial for extrapolation (Default=1)
;       INPUT_MASK - Cube of flags corresponding to data cube.  If used,
;                  the sky computation is restricted to the smallest 
;                  contiguous rectangle containing all the pixels flagged
;                  valid (with 1 rather than 0).
;
; PROCEDURE:
;       Uses astronomy library "sky" routine for scalar sky and
;       column-by-column median for vector sky.
;
; PROCEDURE CALLS:
;        MEANCLIP, SKY, XMEDSKY
; MODIFICATION HISTORY:
;   10 Jul. 1997   - Written.  R. S. Hill, Hughes STX
;   20 Oct. 1997   - 1-D sky option.  RSH
;    7 Aug. 1998   - SELECT keyword.  RSH
;    6 Oct. 1998   - Extrapolation.  RSH
;    7 Oct. 1998   - INPUT_MASK added.  RSH
;   21 Oct. 1998   - Fallback to 3-sigma clipped mean if mode fails.  RSH
;-
pro EXTRAP, Deg, X, Y, Y2, LIMS=lims
;+
; NAME:
;       EXTRAP
;
; PURPOSE:
;       Fills in the ends of a 1-D array using polynomial extrapolation.
;
; CALLING SEQUENCE:
;       EXTRAP, Deg, X, Y, Y2
;
; INPUT POSITIONAL PARAMETERS:
;       Deg:   Degree of polynomial
;       X:     Independent variable
;       Y:     Dependent variable
;
; KEYWORD PARAMETERS:
;       LIMS:  3-element array giving range of X to be used to fit
;              polynomial and starting point where extrapolation is
;              to be substituted; if not given, you click on a plot;
;              order of elements is [xmin, xmax, xstart]; if LIMS is
;              specified, then program is silent
;
; OUTPUT POSITIONAL PARAMETERS:
;       Y2:    Dependent variable with extrapolated portion filled in
;
; SIDE EFFECTS:
;     May pop a window for selecting range.
;
; MODIFICATION HISTORY:
;     Written by RSH, RITSS, 14 Aug 98
;     Spiffed up for library.  RSH, 6 Oct 98
;-
IF n_params(0) LT 1 THEN BEGIN
    print, 'CALLING SEQUENCE:  extrap, deg, x, y, y2'
    print, 'KEYWORD PARAMETER:  lims'
    RETALL
ENDIF
IF NOT keyword_set(lims) THEN BEGIN
    verbose = 1b
    savedev = strtrim(strupcase(!D.name),2)
    set_plot, 'X'
    window, /free
    plot,x,y
    print, 'Click on fit limit 1'
    cursor, xx1, yy1, /down, /data
    print, 'Click on fit limit 2'
    cursor, xx2, yy2, /down, /data
    print, 'Click starting point of extrapolation'
    cursor, xx3, yy3, /down, /data
    wdelete, !D.window
    IF savedev NE 'X' THEN set_plot, savedev
ENDIF ELSE BEGIN
    verbose = 0b
    xx1 = lims[0]
    xx2 = lims[1]
    xx3 = lims[2]
ENDELSE
IF verbose THEN print,'Extrapolating from region ',xx1, ' to ', xx2
wmin = min(where(x ge min([xx1,xx2])))
wmax = max(where(x le max([xx1,xx2])))
coeff = poly_fit(x[wmin:wmax],y[wmin:wmax], deg, yfit, /double)
xhalf = 0.5*(min(x)+max(x))
up = 1b
if xx3 lt xhalf then up = 0b
ypoly = poly(x, coeff)
y2 = y
IF up THEN BEGIN
    if verbose then print, 'Extrapolating above x = ',xx3
    y2[xx3] = ypoly[xx3:*]
ENDIF ELSE BEGIN
    if verbose then print, 'Extrapolating below x = ',xx3
    y2[0]   = ypoly[0:xx3]
ENDELSE
RETURN
END

PRO SKYADJ_CUBE,Datacube,Skyvals,Totsky, XMEDSKY=xmedsky, $
                REGION=region,VERBOSE=verbose,NOEDIT=noedit, $
                SELECT=select,EXTRAPR=extrapr,EDEGREE=edegree, $
                INPUT_MASK=input_mask
xmed = keyword_set(xmedsky)
verbose=keyword_set(verbose)
ipm = keyword_set(input_mask)
szc = size(datacube)
xdim = szc[1]
ydim = szc[2]
zdim = szc[3]

IF ipm THEN BEGIN
    szm = size(input_mask)
    w_dim_ne = where(szc[0:3] NE szm[0:3], cw_dim_ne)
    IF cw_dim_ne GT 0 THEN BEGIN
        print, 'SKYADJ_CUBE:  INPUT_MASK has different dims from ' $
          + 'DATACUBE'
        print, 'Executing RETALL.'
        retall
    ENDIF
    xmin = intarr(zdim)
    xmax = xmin
    ymax = xmin
    ymin = xmin
    FOR i=0,zdim-1 DO BEGIN
        xtot = total(input_mask[*,*,i],2)
        ytot = total(input_mask[*,*,i],1)
        wxt = where(xtot GT 0,cwxt)
        wyt = where(ytot GT 0,cwyt)
        IF cwxt LE 0 OR cwyt LE 0 THEN BEGIN
            print, 'SKYADJ_CUBE:  INPUT_MASK invalid'
            print, 'Executing RETALL'
            retall
        ENDIF
        xmin1 = min(wxt,max=xmax1)
        ymin1 = min(wyt,max=ymax1)
        xmin[i] = xmin1
        ymin[i] = ymin1
        xmax[i] = xmax1
        ymax[i] = ymax1
    ENDFOR
ENDIF ELSE BEGIN
    IF n_elements(region) LT 1 THEN BEGIN
        xmarg = xdim/10
        ymarg = ydim/10
        region = [xmarg,xdim-xmarg,ymarg,ydim-ymarg]
    ENDIF
ENDELSE

IF n_elements(edegree) LT 1 THEN edegree=1
IF n_elements(extrapr) LT 1 THEN extrapr=0.1
do_extrap=keyword_set(extrapr)

IF n_elements(select) LT 1 THEN select=indgen(zdim)
nsel = n_elements(select)

IF xmed THEN BEGIN
    skyvals = fltarr(xdim,zdim) - 32768.
ENDIF ELSE BEGIN
    skyvals = fltarr(zdim) - 32768.
ENDELSE 
skyplane = fltarr(xdim,ydim)
FOR i=0,nsel-1 DO BEGIN
    sel = select[i]
    plane = datacube[*,*,sel]
    IF ipm THEN clip_par = [xmin[sel],xmax[sel],ymin[sel],ymax[sel]] $
           ELSE clip_par = region            
    IF xmed THEN BEGIN
        xmedsky, plane, bkg, clip=clip_par
        IF do_extrap THEN BEGIN
            xrange = clip_par[1]-clip_par[0]+1
            extsize = round(extrapr*xrange)
            indx = indgen(xdim)
            extrap, edegree, indx, temporary(bkg), bkg2, $
                lims=[clip_par[0],clip_par[0]+extsize, $
                clip_par[0]+0.4*extsize]
            extrap, edegree, temporary(indx), temporary(bkg2), bkg3, $
                lims=[clip_par[1]-extsize,clip_par[1], $
                clip_par[1]-0.4*extsize]
        ENDIF ELSE BEGIN
            bkg3 = temporary(bkg)
        ENDELSE
        skyvals[0,sel] = bkg3
        FOR j=0,ydim-1 DO BEGIN 
            skyplane[0,j] = bkg3
        ENDFOR 
    ENDIF ELSE BEGIN 
        sky, plane[clip_par[0]:clip_par[1],clip_par[2]:clip_par[3]], $
          skymode, skysig, /silent
        IF skysig LT 0 THEN BEGIN
            print, 'SKYADJ_CUBE:  Fallback to 3-sigma clipped sky ' $
                + 'for plane '+strn(i)
            meanclip, plane[clip_par[0]:clip_par[1],clip_par[2]:clip_par[3]], $
                skymode, skysig, verbose=verbose
        ENDIF
        skyvals[sel] = skymode
        skyplane[*] = skymode
    ENDELSE 
    IF NOT keyword_set(noedit) THEN BEGIN
        IF verbose THEN print,'SKYADJ_CUBE:  Adjusting plane ', $
            strn(sel)
        datacube[0,0,sel] = plane-skyplane
    ENDIF
ENDFOR
IF verbose THEN BEGIN
    IF xmed THEN BEGIN 
        print,'SKYADJ_CUBE:  1-D sky as function of X'
        print,'              Average values per image plane are'
        FOR i=0,zdim-1 DO $
          print,'             ',avg(skyvals[*,i])
    ENDIF ELSE BEGIN
        print,'SKYADJ_CUBE:  Scalar sky for each image plane'
        print,'              Values are '
        print,'              ',skyvals
    ENDELSE
ENDIF 
IF xmed THEN BEGIN
    totsky = total(skyvals[*,select],2)
ENDIF ELSE begin
    totsky = total(skyvals[select])
ENDELSE
return
END

