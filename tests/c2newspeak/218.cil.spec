Newspeak output
---------------
218.c
void main(void) {
  (218.c:30#1132)^int32 x;
  (218.c:29#1112)^do {
    (218.c:36#1189)^do {
      (218.c:32#1152)^do {
        (218.c:31#1137)^choose {
         -->
          (218.c:31#1137)^guard(! (x_int32 ==_int32 1));
          (218.c:31#1137)^guard(! (x_int32 ==_int32 2));
          (218.c:31#1137)^goto lbl0;
         -->
          (218.c:31#1137)^choose {
           -->
            (218.c:31#1137)^guard((x_int32 ==_int32 1));
            (218.c:31#1137)^goto lbl2;
           -->
            (218.c:31#1137)^choose {
             -->
              (218.c:31#1137)^guard((x_int32 ==_int32 2));
              (218.c:31#1137)^goto lbl1;
             -->
            }
          }
        }
      } with lbl2: {
      }
      (218.c:33#1164)^x =(int32) 2;
      (218.c:34#1175)^goto lbl0;
    } with lbl1: {
    }
    (218.c:37#1201)^x =(int32) 1;
  } with lbl0: {
  }
}


