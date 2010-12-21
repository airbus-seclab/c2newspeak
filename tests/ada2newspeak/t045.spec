Newspeak output
---------------
void t045(void) {
  (t045.adb:6#5)^uint1 tmp_cir!0;
  (t045.adb:6#5)^tmp_cir!0: uint1 <- t092.z(42: int32);
  (t045.adb:6#5)^choose {
   -->
    (t045.adb:6#5)^guard(tmp_cir!0_uint1);
   -->
    (t045.adb:6#5)^guard(! tmp_cir!0_uint1);
  }
}

uint1 t092.z(int32 x) {
  (t092.adb:5#12)^!return =(uint1) (x_int32 > 0);
}


