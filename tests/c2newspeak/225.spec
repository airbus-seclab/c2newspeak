Newspeak output
---------------
225.c
main() {
  (225.c:27#6)^int32 x;
  (225.c:29#2)^choose {
    | ! (0-_int32 ==_int32 0) -->
      (225.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
      (225.c:29#2)^0- =(int32) 1;
    | (0-_int32 ==_int32 0) -->
      (225.c:29#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
      (225.c:29#2)^0- =(int32) ! (0-_int32 ==_int32 0);
  }
}


