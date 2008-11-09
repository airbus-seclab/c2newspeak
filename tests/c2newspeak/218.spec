Newspeak output
---------------
218.c
main() {
  (218.c:30#6)^int32 x;
  (218.c:31#2)^do {
    (218.c:36#2)^do {
      (218.c:32#2)^do {
        (218.c:31#2)^choose {
          | (0-_int32 ==_int32 1) -->
            (218.c:32#2)^goto lbl3;
          | (0-_int32 ==_int32 2) -->
            (218.c:36#2)^goto lbl2;
          | ! (0-_int32 ==_int32 2) & ! (0-_int32 ==_int32 1) -->
            (218.c:31#2)^goto lbl1;
        }
      } with lbl3: {
      }
      (218.c:33#4)^0- =(int32) 2;
      (218.c:34#4)^goto lbl1;
    } with lbl2: {
    }
    (218.c:37#4)^0- =(int32) 1;
  } with lbl1: {
  }
}


