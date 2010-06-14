Newspeak output
---------------
020.c
void main(void) {
  (020.c:31#1133)^int32 x;
  (020.c:30#1113)^do {
    (020.c:33#1153)^do {
      (020.c:32#1138)^choose {
       -->
        (020.c:32#1138)^guard(! (x_int32 ==_int32 1));
        (020.c:32#1138)^guard(! (x_int32 ==_int32 2));
        (020.c:32#1138)^goto lbl0;
       -->
        (020.c:32#1138)^choose {
         -->
          (020.c:32#1138)^guard((x_int32 ==_int32 1));
          (020.c:32#1138)^goto lbl1;
         -->
          (020.c:32#1138)^choose {
           -->
            (020.c:32#1138)^guard((x_int32 ==_int32 2));
            (020.c:32#1138)^goto lbl1;
           -->
          }
        }
      }
    } with lbl1: {
    }
    (020.c:35#1175)^x =(int32) 1;
  } with lbl0: {
  }
}


