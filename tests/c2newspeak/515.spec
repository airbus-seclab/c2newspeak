Newspeak output
---------------
515.c
f() {
  (515.c:27#2)^1- =(int32) 0-_int32;
}

main() {
  (515.c:31#6)^int32 y;
  (515.c:32#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (515.c:33#2)^{
    int32 f.arg1;
    (515.c:33#2)^0- =(int32) 2;
    (515.c:33#2)^f();
  }
}


