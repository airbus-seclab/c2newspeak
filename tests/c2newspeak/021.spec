Newspeak output
---------------
void (021.c:29#5)^main(void) {
  (021.c:30#6)^int32 x;
  (021.c:31#6)^int32 y;
  (021.c:32#2)^do {
    (021.c:33#2)^do {
      (021.c:32#2)^choose {
       -->
        (021.c:32#2)^guard((x_int32 ==_int32 1));
        (021.c:33#2)^goto lbl2;
       -->
        (021.c:32#2)^guard(! (x_int32 ==_int32 1));
        (021.c:32#2)^goto lbl1;
      }
    } with lbl2:
    (021.c:34#4)^do {
      (021.c:34#4)^do {
        (021.c:39#4)^do {
          (021.c:35#4)^do {
            (021.c:34#4)^choose {
             -->
              (021.c:34#4)^guard((y_int32 ==_int32 2));
              (021.c:35#4)^goto lbl6;
             -->
              (021.c:34#4)^choose {
               -->
                (021.c:34#4)^guard((y_int32 ==_int32 3));
                (021.c:39#4)^goto lbl5;
               -->
                (021.c:34#4)^guard(! (y_int32 ==_int32 3));
                (021.c:34#4)^guard(! (y_int32 ==_int32 2));
                (021.c:34#4)^goto lbl4;
              }
            }
          } with lbl6:
          (021.c:36#6)^y =(int32) 2;
          (021.c:37#6)^goto lbl3;
        } with lbl5:
        (021.c:40#6)^y =(int32) 3;
        (021.c:41#6)^goto lbl3;
      } with lbl4:
      (021.c:44#6)^y =(int32) 4;
    } with lbl3:
  } with lbl1:
}


