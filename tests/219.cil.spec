Newspeak output
---------------
219.c
f() {
  (219.c:27#1064)^0- =(int32) 0;
}

main() {
  (219.c:31#1097)^int32;
  int32;
  (219.c:32#1102)^{
    int32;
    (219.c:32#1102)^f();
    (219.c:32#1102)^1- =(int32) 0-_int32;
  }
  (219.c:32#1102)^1- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (219.c:33#1117)^1- =(int32) 2;
}


