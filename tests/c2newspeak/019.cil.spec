Newspeak output
---------------
019.c
void main(void) {
  (019.c:31#1133)^int32 x;
  (019.c:30#1113)^do {
    (019.c:33#1153)^do {
      (019.c:32#1138)^choose {
       -->
        (019.c:32#1138)^guard(! (0-_int32 ==_int32 2));
        (019.c:32#1138)^goto lbl0;
       -->
        (019.c:32#1138)^choose {
         -->
          (019.c:32#1138)^guard((0-_int32 ==_int32 2));
          (019.c:32#1138)^goto lbl1;
         -->
        }
      }
    } with lbl1: {
    }
    (019.c:34#1165)^0- =(int32) 1;
  } with lbl0: {
  }
}


