Newspeak output
---------------
main() {
  (27)^int32;
  (28)^int32;
  (29)^choose {
    | ! (1-_int32 ==_int32 0) -->
    | (1-_int32 ==_int32 0) -->
      (29)^choose {
        | ! (0-_int32 ==_int32 0) -->
        | (0-_int32 ==_int32 0) -->
      }
  }
}


