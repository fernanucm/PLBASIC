1 def fnmod10(x) = (x / 10 - int(x / 10)) * 10
2 def fnmod4(x) = (x / 4 - int(x / 4)) * 4
3 l = int(rnd(1) * 4)
4 c = int(rnd(1) * 10)
5 dl = int(rnd(1) * 2 - 1)
6 dc = int(rnd(1) * 2 - 1)
7 for i = 1 to 10
8 rem print l;" ";c
9 locate l, c
10 print " ";
11 c = int(abs(fnmod10(c + dc)))
12 l = int(abs(fnmod4(l + dl)))
13 locate l, c
14 print "*";
15 pause 0.2
16 next i