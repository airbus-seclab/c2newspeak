Warning: the order of execution of side-effects in expressions not specified, picking a random one, be careful in 065.c line 32
Newspeak output
---------------
065.c
f() {
  (065.c:27#1069)^1- =(int32) 0-_int32;
}

main() {
  (065.c:31#1102)^int32 x;
  (065.c:32#1107)^int32 !tmp-1073741820;
  (065.c:32#1107)^{
    int32 f.arg1;
    (065.c:32#1107)^0- =(int32) 2;
    (065.c:32#1107)^f();
  }
  (065.c:32#1107)^{
    int32 !tmp-1073741819;
    (065.c:32#1107)^{
      int32 f.arg1;
      (065.c:32#1107)^0- =(int32) 3;
      (065.c:32#1107)^f();
    }
    (065.c:32#1107)^2- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
  }
}


