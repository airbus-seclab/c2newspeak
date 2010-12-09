Newspeak output
---------------
t033.adb
void t033.appelfonction(void) {
  (t033.adb:11#9)^int32 x;
  (t033.adb:14#10)^int32 tmp_cir!0;
  (t033.adb:14#10)^tmp_cir!0: int32 <- t033.f(2: int32);
  (t033.adb:14#10)^x =(int32) tmp_cir!0_int32;
}

int32 t033.f(int32 y) {
  (t033.adb:6#12)^!return =(int32) belongs[-2147483648,2147483647] (y_int32 + 1);
}


