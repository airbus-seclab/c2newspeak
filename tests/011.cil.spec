Newspeak output
---------------
f() {
  (011.c:33#1269)^2- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
}

main() {
  (011.c:37#1306)^int32;
  (011.c:37#1309)^int32;
  (011.c:37#1312)^int32;
  (011.c:38#1317)^int32;
  (011.c:38#1317)^{
    int32;
    (011.c:38#1317)^int32;
    (011.c:38#1317)^1- =(int32) 5-_int32;
    (011.c:38#1317)^0- =(int32) 4-_int32;
    (011.c:38#1317)^f();
  }
  (011.c:38#1317)^1- =(int32) 0-_int32;
}


