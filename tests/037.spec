Newspeak output
---------------
037.c
main() {
  (037.c:30#1132)^int32 y;
  (037.c:31#1137)^do {
    (037.c:31#1137)^do {
      (037.c:35#1178)^do {
        (037.c:32#1152)^do {
          (037.c:31#1137)^choose {
            | (0-_int32 ==_int32 2) -->
              (037.c:32#1152)^goto lbl5;
            | (0-_int32 ==_int32 1) -->
              (037.c:35#1178)^goto lbl4;
            | ! (0-_int32 ==_int32 1) & ! (0-_int32 ==_int32 2) -->
              (037.c:31#1137)^goto lbl3;
          }
        } with lbl5: {
        }
        (037.c:33#1164)^goto lbl2;
      } with lbl4: {
      }
      (037.c:36#1190)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


