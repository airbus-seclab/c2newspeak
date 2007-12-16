Newspeak output
---------------
main() {
  (30)^int4;
  (32)^0- =(int4) 0;
  (32)^do {
    (32)^while (1) {
      (32)^choose {
        | (10 > 0-_int4) -->
        | ! (10 > 0-_int4) -->
          (32)^goto lbl2;
      }
      (32)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    }
  } with lbl2: {
  }
}


