Newspeak output
---------------
t286.adb
int32 t286.f(void) {
  (t286.adb:9#10)^!return =(int32) 0;
}

void t286.main(void) {
  (t286.adb:13#7)^int32 x;
  (t286.adb:15#8)^int32 !tmp0;
  (t286.adb:15#8)^!tmp0 <- t286.f();
  (t286.adb:15#8)^x =(int32) !tmp0_int32;
}


