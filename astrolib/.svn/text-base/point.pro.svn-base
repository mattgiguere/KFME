PRO	POINT,x,y,vx,vy,ip,plot=pl
if n_params(0) lt 2 then begin
	print,'POINT, x,y[,cx,cy,ipix,plot=pl]'
retall
endif
;prints current values of x and y at cursor, and current pixel #
	if keyword_set(pl) then plot,x,y,psym=10
		cursor,vx,vy,/down
        	near=(x-vx)*(x-vx)
		pos=min(near,ip)
		print,vx,vy,x(ip),y(ip),ip $
   	,format='(" Cursor(x,y): ",2(G0.2,1x)," Vector(x,y,i): ",2(G0.2,1x),I4)'
return & end
