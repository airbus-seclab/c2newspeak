Newspeak output
---------------
main() {
  (27)^int32;
  (28)^int32;
  (29)^int32;
  (28)^1- =(int32) 0;
  (29)^2- =(int32) 0;
  (29)^do {
    (29)^while (1) {
      (29)^do {
        (29)^choose {
          | (10 > 2-_int32) -->
          | ! (10 > 2-_int32) -->
            (29)^goto lbl2;
        }
        (30)^1- =(int32) coerce[-2147483648,2147483647] (1 - 1-_int32);
        (31)^choose {
          | ! (1-_int32 ==_int32 0) -->
            (32)^goto lbl1;
          | (1-_int32 ==_int32 0) -->
        }
      } with lbl1: {
      }
      (29)^0- =(int32) 2-_int32;
      (29)^2- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


