Newspeak output
---------------
main() {
  (32)^int32;
  (34)^int32;
  (33)^choose {
    | (10 > 1-_int32) -->
      (34)^0- =(int32) 1-_int32;
      (34)^1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
    | ! (10 > 1-_int32) -->
  }
}


