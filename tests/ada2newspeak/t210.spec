Newspeak output
---------------
t210.adb
int32 t210.f(int32 y) {
  (t210.adb:5#12)^!return =(int32) 1;
}

void t210.main(void) {
  (t210.adb:9#9)^int32 x;
  (t210.adb:11#10)^int32 tmp_cir!0;
  (t210.adb:11#10)^tmp_cir!0 <- t210.f(2);
  (t210.adb:11#10)^x =(int32) tmp_cir!0_int32;
}


