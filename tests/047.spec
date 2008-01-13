Newspeak output
---------------
main() {
  (30)^int32;
  (32)^int32;
  (32)^1- =(int32) 0;
  (32)^do {
    (32)^while (1) {
      (32)^choose {
        | (10 > 1-_int32) -->
        | ! (10 > 1-_int32) -->
          (32)^goto lbl2;
      }
      (32)^0- =(int32) 1-_int32;
      (32)^1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl2: {
  }
}


