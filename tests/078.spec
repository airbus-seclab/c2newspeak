Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int4;
  (35)^int4;
  (28)^1- =(int4) 0;
  (29)^2- =(int4) 0;
  (30)^do {
    (30)^while (1) {
      (30)^do {
        (30)^choose {
          | (10 > 2-_int4) -->
          | ! (10 > 2-_int4) -->
            (30)^goto lbl2;
        }
        (31)^1- =(int4) coerce[-2147483648,2147483647] (1 - 1-_int4);
        (32)^choose {
          | ! (1-_int4 ==_int4 0) -->
            (33)^goto lbl1;
          | (1-_int4 ==_int4 0) -->
        }
        (35)^0- =(int4) 2-_int4;
        (35)^2- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
      } with lbl1: {
      }
    }
  } with lbl2: {
  }
}


