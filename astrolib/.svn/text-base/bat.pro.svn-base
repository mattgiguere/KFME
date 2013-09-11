pro bat

x=findfile('hj/hd*inp',count=nstars)
file1=x(0:30)
file2=x(31:60)
file3=x(61:89)

openw,1,'sme_hjm1.batch'
for i=0,n_elements(file1)-1 do begin
  file=strmid(file1(i),0,strlen(file1(i))-4)
  str1='/home/fischer/sme/'+file+'.inp'
  str2='/home/fischer/sme/'+file+'.out'
  printf,1,'restore, '''+str1+'''
  printf,1,'sme_main,sme'
  printf,1,'save,sme,file= '''+str2+'''
  printf,1,' '
end
close,1

openw,1,'sme_hjm2.batch'
for i=0,n_elements(file2)-1 do begin
  file=strmid(file2(i),0,strlen(file2(i))-4)
  str1='/home/fischer/sme/'+file+'.inp'
  str2='/home/fischer/sme/'+file+'.out'
  printf,1,'restore, '''+str1+'''
  printf,1,'sme_main,sme'
  printf,1,'save,sme,file= '''+str2+'''
  printf,1,' '
end
close,1

openw,1,'sme_hjm3.batch'
for i=0,n_elements(file3)-1 do begin
  file=strmid(file3(i),0,strlen(file3(i))-4)
  str1='/home/fischer/sme/'+file+'.inp'
  str2='/home/fischer/sme/'+file+'.out'
  printf,1,'restore, '''+str1+'''
  printf,1,'sme_main,sme'
  printf,1,'save,sme,file= '''+str2+'''
  printf,1,' '
end
close,1

stop
end
