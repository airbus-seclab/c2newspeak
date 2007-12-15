Newspeak output
---------------
main() {
  (30)^int4;
  (30)^int4;
  (31)^choose {
    | ! (1-_int4 ==_int4 0) -->
      (32)^1- =(int4) 1;
    | (1-_int4 ==_int4 0) -->
      (33)^choose {
        | ! (0-_int4 ==_int4 0) -->
          (34)^0- =(int4) 2;
        | (0-_int4 ==_int4 0) -->
      }
  }
}


