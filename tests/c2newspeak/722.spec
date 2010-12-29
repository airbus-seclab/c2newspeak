Warning: 722.c:29#0: comma in expression accepted
Warning: 722.c:30#0: comma in expression accepted
Warning: 722.c:31#0: comma in expression accepted
Warning: 722.c:29#2: assignment within expression accepted
Warning: 722.c:31#2: assignment within expression accepted
Newspeak output
---------------
void (722.c:26#5)^main(void) {
  (722.c:27#6)^int32 x;
  (722.c:28#6)^int32 y;
  (722.c:29#2)^y =(int32) 1;
  (722.c:29#2)^x =(int32) y_int32;
  (722.c:30#21)^y =(int32) 1;
  (722.c:30#2)^x =(int32) 2;
  (722.c:31#2)^y =(int32) 1;
  (722.c:31#2)^x =(int32) y_int32;
}


