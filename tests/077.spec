Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int4;
  (28)^0- =(int4) 0;
  (29)^1- =(int4) 0;
  (29)^do {
    (29)^while (1) {
      (29)^do {
        (29)^choose {
          | (10 > 1-_int4) -->
          | ! (10 > 1-_int4) -->
            (29)^goto lbl2;
        }
        (30)^0- =(int4) coerce[-2147483648,2147483647] (1 - 0-_int4);
        (31)^choose {
          | ! (0-_int4 ==_int4 0) -->
            (32)^goto lbl1;
          | (0-_int4 ==_int4 0) -->
        }
      } with lbl1: {
      }
      (29)^1- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 1);
    }
  } with lbl2: {
  }
}


