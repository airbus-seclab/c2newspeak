Newspeak output
---------------
011.c
f() {
  (011.c:33#1269)^2- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
}

main() {
  (011.c:37#1306)^int32 x;
  (011.c:37#1309)^int32 y;
  (011.c:37#1312)^int32 z;
  (011.c:38#1317)^int32 value_of_f;
  (011.c:38#1317)^{
    int32 f.arg1;
    (011.c:38#1317)^0- =(int32) 4-_int32;
    (011.c:38#1317)^{
      int32 f.arg2;
      (011.c:38#1317)^0- =(int32) 4-_int32;
      (011.c:38#1317)^f();
    }
  }
  (011.c:38#1317)^1- =(int32) 0-_int32;
}


