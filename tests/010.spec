Newspeak output
---------------
main() {
  (33)^int32;
  (36)^int32;
  (34)^1- =(int32) 0;
  (35)^do {
    (35)^while (1) {
      (35)^choose {
        | (10 > 1-_int32) -->
        | ! (10 > 1-_int32) -->
          (35)^goto lbl2;
      }
      (36)^0- =(int32) 1-_int32;
      (36)^1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


