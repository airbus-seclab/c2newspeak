Newspeak output
---------------
main() {
  (33)^int4;
  (34)^0- =(int4) 0;
  (35)^do {
    (35)^while (1) {
      (35)^choose {
        | (10 > 0-_int4) -->
        | ! (10 > 0-_int4) -->
          (35)^goto lbl2;
      }
      (36)^0- =(int4) coerce[-2147483648,2147483647] (0-_int4 + 1);
    }
  } with lbl2: {
  }
}

