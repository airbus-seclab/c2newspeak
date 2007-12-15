Newspeak output
---------------
main() {
  (30)^int4;
  (30)^int4;
  (30)^int4;
  (31)^choose {
    | ! (2-_int4 ==_int4 0) -->
      (31)^choose {
        | ! (1-_int4 ==_int4 0) -->
          (32)^2- =(int4) 1;
        | (1-_int4 ==_int4 0) -->
          (33)^choose {
            | ! (0-_int4 ==_int4 0) -->
              (34)^1- =(int4) 2;
            | (0-_int4 ==_int4 0) -->
          }
      }
    | (2-_int4 ==_int4 0) -->
      (33)^choose {
        | ! (0-_int4 ==_int4 0) -->
          (34)^1- =(int4) 2;
        | (0-_int4 ==_int4 0) -->
      }
  }
}


