Newspeak output
---------------
019.c
main() {
  (019.c:31#6)^int32 x;
  (019.c:32#2)^do {
    (019.c:33#2)^do {
      (019.c:32#2)^choose {
        | (0-_int32 ==_int32 2) -->
          (019.c:33#2)^goto lbl2;
        | ! (0-_int32 ==_int32 2) -->
          (019.c:32#2)^goto lbl1;
      }
    } with lbl2: {
    }
    (019.c:34#4)^0- =(int32) 1;
  } with lbl1: {
  }
}


