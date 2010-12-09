Newspeak output
---------------
t047.adb
t094.adb
void t047(void) {
  (t047.adb:7#5)^uint1 tmp_cir!0;
  (t047.adb:7#5)^tmp_cir!0: uint1 <- t094.z(42: int32);
  (t047.adb:7#5)^choose {
   -->
    (t047.adb:7#5)^guard(tmp_cir!0_uint1);
   -->
    (t047.adb:7#5)^guard(! tmp_cir!0_uint1);
  }
}

uint1 t094.z(int32 x) {
  (t094.adb:5#12)^!return =(uint1) (0 > x_int32);
}


