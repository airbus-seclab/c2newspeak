Newspeak output
---------------
037.c
main() {
  (037.c:30#6)^int32 y;
  (037.c:31#2)^do {
    (037.c:31#2)^do {
      (037.c:35#2)^do {
        (037.c:32#2)^do {
          (037.c:31#2)^choose {
            | (0-_int32 ==_int32 2) -->
              (037.c:32#2)^goto lbl5;
            | (0-_int32 ==_int32 1) -->
              (037.c:35#2)^goto lbl4;
            | ! (0-_int32 ==_int32 1) & ! (0-_int32 ==_int32 2) -->
              (037.c:31#2)^goto lbl3;
          }
        } with lbl5: {
        }
        (037.c:33#4)^goto lbl2;
      } with lbl4: {
      }
      (037.c:36#4)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


