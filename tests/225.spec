Newspeak output
---------------
225.c
main() {
  (225.c:27#1073)^int32;
  (225.c:29#1081)^int32;
  (225.c:29#1081)^choose {
    | ! (1-_int32 ==_int32 0) -->
      (225.c:29#1081)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
      (225.c:29#1081)^0- =(int32) 1;
    | (1-_int32 ==_int32 0) -->
      (225.c:29#1081)^1- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 1);
      (225.c:29#1081)^0- =(int32) 1-_int32;
  }
  (225.c:29#1081)^1- =(int32) 0-_int32;
}


