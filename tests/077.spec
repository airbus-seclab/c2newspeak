Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int4;
  (29)^int4;
  (28)^1- =(int4) 0;
  (29)^2- =(int4) 0;
  (29)^do {
    (29)^while (1) {
      (29)^do {
        (29)^choose {
          | (10 > 2-_int4) -->
          | ! (10 > 2-_int4) -->
            (29)^goto lbl2;
        }
        (30)^1- =(int4) coerce[-2147483648,2147483647] (1 - 1-_int4);
        (31)^choose {
          | ! (1-_int4 ==_int4 0) -->
            (32)^goto lbl1;
          | (1-_int4 ==_int4 0) -->
        }
      } with lbl1: {
      }
      (29)^0- =(int4) 2-_int4;
      (29)^2- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    }
  } with lbl2: {
  }
}


