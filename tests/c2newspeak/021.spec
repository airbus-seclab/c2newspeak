Newspeak output
---------------
021.c
void main(void) {
  (021.c:30#6)^int32 x;
  (021.c:31#6)^int32 y;
  (021.c:32#2)^do {
    (021.c:33#2)^do {
      (021.c:32#2)^choose {
       -->
        (021.c:32#2)^guard((1-_int32 ==_int32 1));
        (021.c:33#2)^goto lbl2;
       -->
        (021.c:32#2)^guard(! (1-_int32 ==_int32 1));
        (021.c:32#2)^goto lbl1;
      }
    } with lbl2: {
    }
    (021.c:34#4)^do {
      (021.c:34#4)^do {
        (021.c:39#4)^do {
          (021.c:35#4)^do {
            (021.c:34#4)^choose {
             -->
              (021.c:34#4)^guard((0-_int32 ==_int32 2));
              (021.c:35#4)^goto lbl6;
             -->
              (021.c:34#4)^choose {
               -->
                (021.c:34#4)^guard((0-_int32 ==_int32 3));
                (021.c:39#4)^goto lbl5;
               -->
                (021.c:34#4)^guard(! (0-_int32 ==_int32 3));
                (021.c:34#4)^guard(! (0-_int32 ==_int32 2));
                (021.c:34#4)^goto lbl4;
              }
            }
          } with lbl6: {
          }
          (021.c:36#6)^0- =(int32) 2;
          (021.c:37#6)^goto lbl3;
        } with lbl5: {
        }
        (021.c:40#6)^0- =(int32) 3;
        (021.c:41#6)^goto lbl3;
      } with lbl4: {
      }
      (021.c:44#6)^0- =(int32) 4;
    } with lbl3: {
    }
  } with lbl1: {
  }
}


