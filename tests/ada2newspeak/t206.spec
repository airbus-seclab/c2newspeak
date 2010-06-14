Newspeak output
---------------
t206.adb
int32 t206.id(int32 x) {
  (t206.adb:6#9)^!return =(int32) x_int32;
}

void t206.main(void) {
  (t206.adb:10#7)^int32 xn;
  (t206.adb:12#8)^{
    int32 !tmp1;
    (t206.adb:12#8)^!tmp1 <- t206.id(2147483647);
    (t206.adb:12#8)^xn =(int32) !tmp1_int32;
  }
  (t206.adb:14#8)^{
    int32 !tmp0;
    (t206.adb:14#8)^!tmp0 <- t206.id(belongs[-2147483648,2147483647] 2147483648);
    (t206.adb:14#8)^xn =(int32) !tmp0_int32;
  }
}


