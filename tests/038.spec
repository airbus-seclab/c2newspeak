Newspeak output
---------------
main() {
  (30)^int32;
  (30)^int32;
  (31)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (32)^1- =(int32) 1;
    | (1-_int32 ==_int32 0) -->
      (33)^choose {
        | ! (0-_int32 ==_int32 0) -->
          (34)^0- =(int32) 2;
        | (0-_int32 ==_int32 0) -->
      }
  }
}


