Newspeak output
---------------
main() {
  (30)^int4 i;
  (30)^0- =(int4) 0;
  (32)^do {
    (33)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    (32)^while (1) {
      (32)^choose {
        | (100 > 0-_int4) -->
        | ! (100 > 0-_int4) -->
          (32)^goto lbl1;
      }
      (33)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    }
  } with lbl1: {
  }
}


