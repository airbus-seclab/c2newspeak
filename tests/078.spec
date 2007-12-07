Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int4;
  (28)^0- =(int4) 0;
  (29)^1- =(int4) 0;
  (30)^do {
    (30)^while (1) {
      (30)^do {
        (30)^choose {
          | (10 > 1-_int4) -->
          | ! (10 > 1-_int4) -->
            (30)^goto lbl2;
        }
        (31)^0- =(int4) coerce[-2147483648,2147483647] (1 - 0-_int4);
        (32)^choose {
          | ! (0-_int4 ==_int4 0) -->
            (33)^goto lbl1;
          | (0 ==_int4 0-_int4) -->
        }
        (35)^1- =(int4) coerce[-2147483648,2147483647] (1-_int4 + 1);
      } with lbl1: {
      }
    }
  } with lbl2: {
  }
}


