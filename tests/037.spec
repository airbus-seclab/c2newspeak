Newspeak output
---------------
main() {
  (30)^int32;
  (31)^do {
    (35)^do {
      (32)^do {
        (31)^choose {
          | ! (0-_int32 ==_int32 2) & ! (0-_int32 ==_int32 1) -->
            (31)^goto lbl2;
          | (0-_int32 ==_int32 2) -->
            (31)^goto lbl4;
          | (0-_int32 ==_int32 1) -->
            (31)^goto lbl3;
        }
      } with lbl4: {
      }
      (33)^goto lbl2;
    } with lbl3: {
    }
  } with lbl2: {
  }
}


