Newspeak output
---------------
main() {
  (30)^int4 a;
  (30)^int4 b;
  (30)^int4 c;
  (31)^choose {
    | ! (0 ==_int4 2-_int4) -->
      (31)^choose {
        | ! (0 ==_int4 1-_int4) -->
          (32)^2- =(int4) 1;
        | (1-_int4 ==_int4 0) -->
          (33)^choose {
            | ! (0 ==_int4 0-_int4) -->
              (34)^1- =(int4) 2;
            | (0-_int4 ==_int4 0) -->
          }
      }
    | (2-_int4 ==_int4 0) -->
      (33)^choose {
        | ! (0 ==_int4 0-_int4) -->
          (34)^1- =(int4) 2;
        | (0-_int4 ==_int4 0) -->
      }
  }
}


