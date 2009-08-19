Warning: 722.c:29#0: comma in expression accepted
Warning: 722.c:30#0: comma in expression accepted
Warning: 722.c:31#0: comma in expression accepted
Warning: 722.c:29#2: assignment within expression accepted
Warning: 722.c:31#2: assignment within expression accepted
Newspeak output
---------------
722.c
void main(void) {
  (722.c:27#6)^int32 x;
  (722.c:28#6)^int32 y;
  (722.c:29#2)^0- =(int32) 1;
  (722.c:29#2)^1- =(int32) 0-_int32;
  (722.c:30#21)^0- =(int32) 1;
  (722.c:30#2)^1- =(int32) 2;
  (722.c:31#2)^0- =(int32) 1;
  (722.c:31#2)^1- =(int32) 0-_int32;
}


