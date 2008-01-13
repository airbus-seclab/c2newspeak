Newspeak output
---------------
main() {
  (31)^int32;
  (32)^do {
    (33)^do {
      (32)^choose {
        | ! (0-_int32 ==_int32 2) -->
          (32)^goto lbl2;
        | (0-_int32 ==_int32 2) -->
          (32)^goto lbl3;
      }
    } with lbl3: {
    }
    (34)^0- =(int32) 1;
  } with lbl2: {
  }
}


