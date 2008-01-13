Newspeak output
---------------
main() {
  (27)^int32;
  (28)^do {
    (28)^while (1) {
      (28)^choose {
        | ! (0 > 0-_int32) -->
          (28)^choose {
            | (10 > 0-_int32) -->
            | ! (10 > 0-_int32) -->
              (28)^goto lbl2;
          }
        | (0 > 0-_int32) -->
          (28)^goto lbl2;
      }
    }
  } with lbl2: {
  }
}


