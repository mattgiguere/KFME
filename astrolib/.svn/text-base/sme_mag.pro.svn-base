pro sme_mag

restore,'adM2004A.dat'
x=where(ad.fe_spect lt 2.,nx)
d=ad(x)

ps_open,'Magellan_SME',/color
    loadct,39
    !p.background=255
    plot,d.fe_phot,d.fe_spect,ps=8,xra=[-0.1,0.5],yra=[-0.1,0.5],/xsty,/ysty,$
      xtit='[Fe/H]_phot', ytit='[Fe/H]_spect',titl='Analysis of 88 stars',col=1
    plots,[-0.1,0.5],[-0.1,0.5],col=240

