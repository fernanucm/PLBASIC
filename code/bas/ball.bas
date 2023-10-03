  1 cls
 10 def fnmod10(x)=(x/10-int(x/10))*10
 20 def fnmod5(x)=(x/5-int(x/5))*5
 30 l0=int(rnd(1)*4)
 40 c0=int(rnd(1)*10)
 50 dl=int(rnd(1)*2-1)
 60 dc=int(rnd(1)*2-1)
 70 for i=1 to 10
 80 rem print l;" ";c
 90 c1=int(abs(fnmod10(c0+dc)))
100 l1=int(abs(fnmod5(l0+dl)))
110 locate l0,c0
120 print " ";
130 locate l1,c1
140 print "*";
150 l0=l1:c0=c1
160 pause 0.2
170 next i 
