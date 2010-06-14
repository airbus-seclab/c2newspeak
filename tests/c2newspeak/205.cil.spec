Newspeak output
---------------
205.c
void main(void) {
  (205.c:27#1072)^int32 x;
  (205.c:26#1052)^do {
    (205.c:31#1117)^do {
      (205.c:30#1093)^do {
        (205.c:29#1078)^choose {
         -->
          (205.c:29#1078)^guard(! (x_int32 ==_int32 1));
          (205.c:29#1078)^goto lbl1;
         -->
          (205.c:29#1078)^choose {
           -->
            (205.c:29#1078)^guard((x_int32 ==_int32 1));
            (205.c:29#1078)^goto lbl2;
           -->
          }
        }
      } with lbl2: {
      }
      (205.c:30#1101)^x =(int32) 0;
      (205.c:30#1108)^goto lbl0;
    } with lbl1: {
    }
    (205.c:31#1126)^x =(int32) 1;
  } with lbl0: {
  }
}


