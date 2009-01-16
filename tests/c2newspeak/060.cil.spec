Newspeak output
---------------
060.c
void main(void) {
  (060.c:30#1133)^int32 x;
  (060.c:29#1112)^do {
    (060.c:31#1138)^do {
      (060.c:31#1138)^while (1) {
        (060.c:31#1138)^choose {
         -->
          (060.c:31#1138)^guard((0-_int32 > 0));
         -->
          (060.c:31#1138)^guard(! (0-_int32 > 0));
          (060.c:31#1138)^goto lbl1;
        }
        (060.c:32#1158)^goto lbl0;
      }
    } with lbl1: {
    }
    (060.c:34#1172)^0- =(int32) 2;
  } with lbl0: {
  }
}


