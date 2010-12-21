Newspeak output
---------------
void t046(void) {
  (t046.adb:6#5)^uint1 tmp_cir!0;
  (t046.adb:6#5)^tmp_cir!0: uint1 <- t093.z(42: int32);
  (t046.adb:6#5)^choose {
   -->
    (t046.adb:6#5)^guard(tmp_cir!0_uint1);
   -->
    (t046.adb:6#5)^guard(! tmp_cir!0_uint1);
  }
}

uint1 t093.z(int32 x) {
  (t093.adb:5#12)^!return =(uint1) (0 > x_int32);
}


