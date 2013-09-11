pro plot_keywords, $
	TYPE=itype, BACKGROUND=back, CHANNEL=chan, CHARSIZE=chsiz, $
	CHARTHICK=chthck, COLOR=color, DATA=data, DEVICE=device, $
	FONT=font, LINESTYLE=linest, NOCLIP=noclip, NODATA=nodata, $
	NOERASE=noerase, NORMAL=normal, NSUM=nsum, PSYM=psym, $
	SUBTITLE=subtit, SYMSIZ=symsiz, T3D=t3d, THICK=thick, TICKLEN=ticklen, $
	TITLE=title, XCHARSIZE=xchsiz, XMARGIN=xmargn, XMINOR=xminor, $
	XRANGE=xrange, XSTYLE=xstyle, XTICKLEN=xtickln, XTICKNAME=xticknm, $
	XTICKS=xticks, XTICKV=xtickv, XTITLE=xtitle, XTYPE=xtype, $
	YCHARSIZE=ychsiz, YMARGIN=ymargn, YMINOR=yminor, YNOZERO = ynozero, $
	YRANGE=yrange, YSTYLE=ystyle, YTICKLEN=ytickln, YTICKNAME=yticknm, $
	YTICKS=yticks, YTICKV=ytickv, YTITLE=ytitle, YTYPE=ytype
;+
; NAME:
;   PLOT_KEYWORDS
; PURPOSE:
;   Return default value(s) of plotting keywords.    Useful inside of
;   plotting procedure.
; CALLING SEQUENCE:
;   plot_keywords, [BACKGROUND =, CHANNEL =, CHARSIZE =, CHARTHICK =,
;     COLOR = , DATA = , DEVICE = , FONT = , LINESTYLE =, NOCLIP = ,
;     NODATA = , NOERASE = , NORMAL =, NSUM = , PSYM =, SUBTITLE =,
;     SYMSIZ = , T3D = , THICK = , THICKLEN = , TITLE = , XCHARSIZE = ,
;     XMARGIN =, XMINOR =, XRANGE =, XSTYLE = , XTICKLEN =, XTICKNAME =,
;     XTICKS = , XTICKV = , XTITLE =, XTYPE =, YCHARSIZE = , YMARGIN = ,
;     YMINOR = , YNOZERO = , YRANGE = , YSTYLE =, YTICKLEN = ,YTICKNAME =, 
;     YTICKS =,  YTICKV = , YTITLE = , YTYPE = ]
; INPUT - OUTPUTS:
;     None.
; OPTIONAL OUTPUT KEYWORDS:
;     Any of the plotting keywords above.   These keywords are all recognized 
;     by the PLOT procedure, and listed in Appendix D.2 of the IDL manual.
;     PLOT_KEYWORDS does *not* include any of the Z axis keywords used
;     for 3-d plotting.
;
;     An undefined variable assigned to the keyword will be returned with the 
;     default value, usually from the correpsonding system variable.
; EXAMPLE:
;     Suppose that one has a procedure PLOT_PROC that will make a call
;     to the IDL PLOT procedure.    One wishes to include the optional 
;     plot keywords XRANGE and YRANGE in PLOT_PROC and pass these to PLOT
;
;     pro plot_proc, XRANGE = xrange, YRANGE = yrange
;     ......
;     plot_keywords, XRANGE = xran, $      ;Get default values if user did
;                    YRANGE = yran         ;not supply any values
;     plot,.... XRANGE = xran, YRANGE = yran         ;Pass to PLOT procedure
; NOTES:
;     Plotting keywords that return values (such as XTICK_GET) are not 
;     included since there is no need to specify a default.
;
;     The default of XTYPE is 0 and not !X.TYPE
; MODIFICATION HISTORY:
;     Written    Wayne Landsman                January, 1991
;     Modified default for XTYPE and YTYPE
;-
On_error, 2
;
if not keyword_set( BACK )  then back = !P.background
if not keyword_set( CHAN )  then chan = !P.channel
if not keyword_set( CHSIZ ) then chsiz = !P.charsize
if not keyword_set( CHTHCK ) then chthck = !P.charthick
if not keyword_set( CLIP ) then clip = !P.clip
if not keyword_set( COLOR ) then color = !P.color
if not keyword_set( DATA ) then data = 0
if not keyword_set( DEVICE ) then device = 0
if not keyword_set( FONT ) then font = !P.font
if not keyword_set( LINEST ) then linest = !P.linestyle
if not keyword_set( NOCLIP ) then noclip = 0
if not keyword_set( NODATA ) then nodata = 0
if not keyword_set( NOERASE ) then noerase = 0
if not keyword_set( NORMAL ) then normal = 0
if not keyword_set( NSUM ) then nsum = !P.nsum
if not keyword_set( POSITION ) then position = !P.position
if not keyword_set( PSYM ) then psym = !P.psym
if not keyword_set( SUBTIT ) then subtit = !P.subtitle
if not keyword_set( SYMSIZ ) then symsiz = 1.0
if not keyword_set( T3D )  then t3d = 0
if not keyword_set( THICK ) then thick = !P.thick
if not keyword_set( TICKLEN ) then ticklen = !P.ticklen
if not keyword_set( TITLE ) then title = !P.title
;
;				X-axis keywords.
;
if not keyword_set( XCHSIZ ) then xchsiz = !X.charsize
if not keyword_set( XMARGN ) then xmargn = !X.margin
if not keyword_set( XMINOR ) then xminor = !X.minor
if not keyword_set( XRANGE ) then xrange = !X.range
if not keyword_set( XSTYLE ) then xstyle = !X.style
if not keyword_set( XTICKLN ) then xtickln = !X.ticklen
if not keyword_set( XTICKNM ) then xticknm = !X.tickname
if not keyword_set( XTICKS ) then xticks = !X.ticks
if not keyword_set( XTICKV ) then xtickv = !X.tickv
if not keyword_set( XTITLE ) then xtitle = !X.title
if not keyword_set( XTYPE ) then xtype = 0
;
;
if not keyword_set( YCHSIZ ) then ychsiz = !Y.charsize
if not keyword_set( YMARGN ) then ymargn = !Y.margin
if not keyword_set( YMINOR ) then yminor = !Y.minor
if not keyword_set( YNOZERO ) then ynozero = (!Y.STYLE and 16)
if not keyword_set( YRANGE ) then yrange = !Y.range
if not keyword_set( YSTYLE ) then ystyle = !Y.style
if not keyword_set( YTICKLN ) then ytickln = !Y.ticklen
if not keyword_set( YTICKNM ) then yticknm = !Y.tickname
if not keyword_set( YTICKS ) then yticks = !Y.ticks
if not keyword_set( YTICKV ) then ytickv = !Y.tickv
if not keyword_set( YTITLE ) then ytitle = !Y.title
if not keyword_set( YTYPE ) then ytype = 0
;
return
end
