Newspeak output
---------------
main() {
  (27)^int4;
  (28)^int4;
  (29)^choose {
    | ! (1-_int4 ==_int4 0) -->
    | (1-_int4 ==_int4 0) -->
      (29)^choose {
        | ! (0-_int4 ==_int4 0) -->
        | (0-_int4 ==_int4 0) -->
      }
  }
}


