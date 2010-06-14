Newspeak output
---------------
t058.adb
void t058(void) {
  (t058.adb:3#6)^int32 y;
  (t058.adb:9#10)^int32 !tmp0;
  (t058.adb:9#10)^!tmp0 <- t100(10);
  (t058.adb:5#5)^choose {
   -->
    (t058.adb:5#5)^guard((!tmp0_int32 > 15));
    (t058.adb:7#10)^{
      int32 !tmp1;
      (t058.adb:7#10)^!tmp1 <- t100(2);
      (t058.adb:7#10)^y =(int32) !tmp1_int32;
    }
   -->
    (t058.adb:5#5)^guard(! (!tmp0_int32 > 15));
    (t058.adb:9#10)^{
      int32 !tmp2;
      (t058.adb:9#10)^!tmp2 <- t100(-2);
      (t058.adb:9#10)^y =(int32) !tmp2_int32;
    }
  }
}


