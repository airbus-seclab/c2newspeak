Warning: 334.c:30#0: goto statement accepted
Newspeak output
---------------
334.c
void main(void) {
  (334.c:27#6)^int32 x;
  (334.c:29#2)^do {
    (334.c:29#2)^while (1) {
      (334.c:29#2)^choose {
       -->
        (334.c:29#2)^guard(! (0-_int32 ==_int32 0));
       -->
        (334.c:29#2)^guard((0-_int32 ==_int32 0));
        (334.c:29#2)^goto lbl2;
      }
      (334.c:29#2)^do {
        (334.c:30#4)^goto lbl4;
        (334.c:31#4)^0- =(int32) 0;
      } with lbl4: {
      }
    }
  } with lbl2: {
  }
}


