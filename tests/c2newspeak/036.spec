Newspeak output
---------------
036.c
main() {
  (036.c:30#6)^int32 x;
  (036.c:31#6)^int32 y;
  (036.c:32#2)^do {
    (036.c:33#2)^do {
      (036.c:32#2)^choose {
        | (1-_int32 ==_int32 1) -->
          (036.c:33#2)^goto lbl2;
        | ! (1-_int32 ==_int32 1) -->
          (036.c:32#2)^goto lbl1;
      }
    } with lbl2: {
    }
    (036.c:36#6)^0- =(int32) 4;
  } with lbl1: {
  }
}


