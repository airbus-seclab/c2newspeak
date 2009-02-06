Warning: 332.c:29#1086: goto statement accepted
Newspeak output
---------------
332.c
void main(void) {
  (332.c:27#6)^int32 x;
  (332.c:32#1)^do {
    (332.c:28#2)^choose {
     -->
      (332.c:28#2)^guard(! (0-_int32 ==_int32 0));
      (332.c:29#4)^goto lbl1;
     -->
      (332.c:28#2)^guard((0-_int32 ==_int32 0));
    }
    (332.c:31#2)^0- =(int32) 3;
  } with lbl1: {
  }
  (332.c:33#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


