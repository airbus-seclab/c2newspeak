Newspeak output
---------------
021.c
void main(void) {
  (021.c:30#1132)^int32 x;
  (021.c:31#1141)^int32 y;
  (021.c:29#1112)^do {
    (021.c:33#1161)^do {
      (021.c:32#1146)^choose {
        | ! (1-_int32 ==_int32 1) -->
          (021.c:32#1146)^goto lbl0;
        | (1-_int32 ==_int32 1) -->
          (021.c:32#1146)^goto lbl1;
      }
    } with lbl1: {
    }
    (021.c:34#1173)^do {
      (021.c:43#1280)^do {
        (021.c:39#1235)^do {
          (021.c:35#1190)^do {
            (021.c:34#1173)^choose {
              | ! (0-_int32 ==_int32 2) & ! (0-_int32 ==_int32 3) -->
                (021.c:34#1173)^goto lbl3;
              | (0-_int32 ==_int32 2) -->
                (021.c:34#1173)^goto lbl5;
              | (0-_int32 ==_int32 3) -->
                (021.c:34#1173)^goto lbl4;
            }
          } with lbl5: {
          }
          (021.c:36#1204)^0- =(int32) 2;
          (021.c:37#1217)^goto lbl2;
        } with lbl4: {
        }
        (021.c:40#1249)^0- =(int32) 3;
        (021.c:41#1262)^goto lbl2;
      } with lbl3: {
      }
      (021.c:44#1296)^0- =(int32) 4;
    } with lbl2: {
    }
  } with lbl0: {
  }
}


