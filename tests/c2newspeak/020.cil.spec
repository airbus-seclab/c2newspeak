Newspeak output
---------------
020.c
main() {
  (020.c:31#1133)^int32 x;
  (020.c:32#1138)^do {
    (020.c:33#1153)^do {
      (020.c:32#1138)^choose {
        | ! (0-_int32 ==_int32 1) & ! (0-_int32 ==_int32 2) -->
          (020.c:32#1138)^goto lbl2;
        | (0-_int32 ==_int32 1) -->
          (020.c:32#1138)^goto lbl3;
        | (0-_int32 ==_int32 2) -->
          (020.c:32#1138)^goto lbl3;
      }
    } with lbl3: {
    }
    (020.c:35#1175)^0- =(int32) 1;
  } with lbl2: {
  }
}


