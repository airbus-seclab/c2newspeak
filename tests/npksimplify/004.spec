main() {
  (27)^int4 stop;
  (28)^int4 i;
  (30)^0- =(int4) 0;
  (31)^do {
    (31)^while (1) {
      (32)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
      (33)^1- =(int4) ! (10 > 0-_int4);
      (31)^choose {
        | (10 > 0-_int4) -->
        | ! (10 > 0-_int4) -->
          (31)^goto lbl1;
      }
    }
  } with lbl1: {
  }
}

