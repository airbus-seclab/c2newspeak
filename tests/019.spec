Newspeak output
---------------
main() {
  (019.c:31#1133)^int32;
  (019.c:32#1138)^do {
    (019.c:33#1153)^do {
      (019.c:32#1138)^choose {
        | ! (0-_int32 ==_int32 2) -->
          (019.c:32#1138)^goto lbl2;
        | (0-_int32 ==_int32 2) -->
          (019.c:32#1138)^goto lbl3;
      }
    } with lbl3: {
    }
    (019.c:34#1165)^0- =(int32) 1;
  } with lbl2: {
  }
}


