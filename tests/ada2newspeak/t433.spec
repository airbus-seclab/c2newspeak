Newspeak output
---------------
t433.adb
t433a.adb
void t433(void) {
  (t433.adb:4#8)^int32 res;
  (t433.adb:6#9)^int32 tmp_cir!0;
  (t433.adb:6#9)^tmp_cir!0 <- t433a.orig(3);
  (t433.adb:6#9)^res =(int32) tmp_cir!0_int32;
}

int32 t433a.orig(int32 y) {
  (t433a.adb:5#12)^!return =(int32) 6;
}


