Newspeak output
---------------
int32 t206.id(int32 x) {
  (t206.adb:6#9)^!return =(int32) x_int32;
}

void t206.main(void) {
  (t206.adb:10#7)^int32 xn;
  (t206.adb:12#8)^{
    int32 tmp_cir!1;
    (t206.adb:12#8)^tmp_cir!1: int32 <- t206.id(2147483647: int32);
    (t206.adb:12#8)^xn =(int32) tmp_cir!1_int32;
  }
  (t206.adb:14#8)^{
    int32 tmp_cir!0;
    (t206.adb:14#8)^tmp_cir!0: int32 <- t206.id(belongs[-2147483648,2147483647] 2147483648: int32);
    (t206.adb:14#8)^xn =(int32) tmp_cir!0_int32;
  }
}


