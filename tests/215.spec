Newspeak output
---------------
215.c
f() {
  (215.c:30#1100)^0- =(int32) 4;
}

main() {
  (215.c:34#1133)^int32[10];
  (215.c:36#1166)^int32;
  (215.c:36#1166)^0- =(int32) 1- + (belongs[0,9] x_int32 * 32)_int32;
  (215.c:36#1166)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (215.c:36#1166)^{
    int32;
    (215.c:36#1166)^f();
    (215.c:36#1166)^1- =(int32) 0-_int32;
  }
}

int32 x = {0: int32 1};

