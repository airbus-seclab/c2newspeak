Newspeak output
---------------
036.c
main() {
  (036.c:30#1132)^int32;
  (036.c:31#1141)^int32;
  (036.c:32#1146)^do {
    (036.c:32#1146)^do {
      (036.c:33#1161)^do {
        (036.c:32#1146)^choose {
          | (1-_int32 ==_int32 1) -->
            (036.c:33#1161)^goto lbl4;
          | ! (1-_int32 ==_int32 1) -->
            (036.c:32#1146)^goto lbl3;
        }
      } with lbl4: {
      }
      (036.c:36#1206)^0- =(int32) 4;
      (036.c:38#1223)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


