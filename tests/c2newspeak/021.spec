Newspeak output
---------------
021.c
main() {
  (021.c:30#6)^int32 x;
  (021.c:31#6)^int32 y;
  (021.c:32#2)^do {
    (021.c:32#2)^do {
      (021.c:33#2)^do {
        (021.c:32#2)^choose {
          | (1-_int32 ==_int32 1) -->
            (021.c:33#2)^goto lbl4;
          | ! (1-_int32 ==_int32 1) -->
            (021.c:32#2)^goto lbl3;
        }
      } with lbl4: {
      }
      (021.c:34#4)^do {
        (021.c:34#4)^do {
          (021.c:39#4)^do {
            (021.c:35#4)^do {
              (021.c:34#4)^choose {
                | (0-_int32 ==_int32 2) -->
                  (021.c:35#4)^goto lbl5;
                | (0-_int32 ==_int32 3) -->
                  (021.c:39#4)^goto lbl4;
                | ! (0-_int32 ==_int32 3) & ! (0-_int32 ==_int32 2) -->
                  (021.c:34#4)^goto lbl3;
              }
            } with lbl5: {
            }
            (021.c:36#6)^0- =(int32) 2;
            (021.c:37#6)^goto lbl2;
          } with lbl4: {
          }
          (021.c:40#6)^0- =(int32) 3;
          (021.c:41#6)^goto lbl2;
        } with lbl3: {
        }
        (021.c:44#6)^0- =(int32) 4;
      } with lbl2: {
      }
      (021.c:46#4)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


