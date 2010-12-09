Newspeak output
---------------
t104.adb
int32 t104.a(void) {
  (t104.adb:5#12)^!return =(int32) 12;
}

void t104.main(void) {
  (t104.adb:10#9)^int32 z;
  (t104.adb:12#10)^int32 tmp_cir!0;
  (t104.adb:12#10)^tmp_cir!0: int32 <- t104.a();
  (t104.adb:12#10)^z =(int32) belongs[0,12] tmp_cir!0_int32;
}


