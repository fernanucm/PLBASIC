10 REM Source: https://8bitworkshop.com/docs/platforms/basic/
100 REM ***23 MATCHES
110 LET M=23
115 PRINT
120 PRINT "WE START WITH 23 MATCHES. WHEN IT IS YOUR"
130 PRINT "TURN, YOU MAY TAKE 1, 2, OR 3 MATCHES. THE"
140 PRINT "ONE WHO MUST TAKE THE LAST MATCH LOSES."
150 PRINT
200 REM***THE HUMAN MOVES
205 PRINT
210 PRINT "THERE ARE NOW";M;" MATCHES."
215 PRINT
220 PRINT "HOW MANY MATCHES DO YOU TAKE";
230 INPUT H
240 IF H>M THEN 260
250 IF H=INT(H) THEN 252
251 GOTO 260
252 IF H>0 THEN 254
253 GOTO 260
254 IF H<4 THEN 280
260 PRINT "YOU CHEATED! I'LL GIVE YOU ANOTHER CHANCE."
270 GOTO 215
280 LET M=M-H
290 IF M=0 THEN 410
300 REM***THE COMPUTER MOVES
310 LET R=M-4*INT(M/4)
320 IF R<>1 THEN 350
330 LET C=INT(3*RND(0))+1
340 GOTO 360
350 LET C=(R+3)-4*INT((R+3)/4)
360 LET M=M-C
370 IF M=0 THEN 440
375 PRINT
380 PRINT "I TOOK";C;" MATCHES."
390 GOTO 200
400 REM***SOMEBODY ONE(SEE LINES 290 AND 370)
410 PRINT
420 PRINT "I WON!!! BETTER LUCK NEXT TIME."
430 GOTO 100
440 PRINT
450 PRINT "O.K. SO YOU WON. LET'S PLAY AGAIN."
460 GOTO 100
999 END