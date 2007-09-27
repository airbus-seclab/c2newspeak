Newspeak output
---------------
main() {
  (30)^int4 a;
  (30)^int4 b;
  (30)^int4 c;
  (31)^choose {
    | ! (2-_int4 ==_int4 0) -->
      (31)^choose {
        | ! (1-_int4 ==_int4 0) -->
          (32)^2- =(int4) 1;
        | (0 ==_int4 1-_int4) -->
          (33)^choose {
            | ! (0-_int4 ==_int4 0) -->
              (34)^1- =(int4) 2;
            | (0 ==_int4 0-_int4) -->
          }
      }
    | (0 ==_int4 2-_int4) -->
      (33)^choose {
        | ! (0-_int4 ==_int4 0) -->
          (34)^1- =(int4) 2;
        | (0 ==_int4 0-_int4) -->
      }
  }
}


