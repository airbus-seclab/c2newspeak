Newspeak output
---------------
void t060(void) {
  (t060.adb:3#5)^int32 x;
  (t060.adb:4#5)^uint1 y;
  (t060.adb:6#6)^{
    int32 tmp_cir!0;
    (t060.adb:6#6)^tmp_cir!0: int32 <- t102.b(2: int32);
    (t060.adb:6#6)^x =(int32) tmp_cir!0_int32;
  }
  (t060.adb:7#6)^y =(uint1) 1;
}

int32 t102.b(int32 x) {
  (t102.adb:5#12)^!return =(int32) x_int32;
}


