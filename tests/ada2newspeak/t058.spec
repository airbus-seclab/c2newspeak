Newspeak output
---------------
void t058(void) {
  (t058.adb:3#6)^int32 y;
  (t058.adb:9#10)^int32 tmp_cir!0;
  (t058.adb:9#10)^tmp_cir!0: int32 <- t100(10: int32);
  (t058.adb:5#5)^choose {
   -->
    (t058.adb:5#5)^guard((tmp_cir!0_int32 > 15));
    (t058.adb:7#10)^{
      int32 tmp_cir!1;
      (t058.adb:7#10)^tmp_cir!1: int32 <- t100(2: int32);
      (t058.adb:7#10)^y =(int32) tmp_cir!1_int32;
    }
   -->
    (t058.adb:5#5)^guard(! (tmp_cir!0_int32 > 15));
    (t058.adb:9#10)^{
      int32 tmp_cir!2;
      (t058.adb:9#10)^tmp_cir!2: int32 <- t100(-2: int32);
      (t058.adb:9#10)^y =(int32) tmp_cir!2_int32;
    }
  }
}


