Newspeak output
---------------
t432.adb
t432a.adb
void t432(void) {
  (t432.adb:6#9)^int32[10] left;
  (t432.adb:8#7)^int32 res;
  (t432.adb:6#9)^left =(int32) 0;
  (t432.adb:6#9)^left + 32 =(int32) 0;
  (t432.adb:6#9)^left + 64 =(int32) 0;
  (t432.adb:6#9)^left + 96 =(int32) 0;
  (t432.adb:6#9)^left + 128 =(int32) 0;
  (t432.adb:6#9)^left + 160 =(int32) 0;
  (t432.adb:6#9)^left + 192 =(int32) 0;
  (t432.adb:6#9)^left + 224 =(int32) 0;
  (t432.adb:6#9)^left + 256 =(int32) 0;
  (t432.adb:6#9)^left + 288 =(int32) 0;
  (t432.adb:11#9)^{
    int32 tmp_cir!1;
    (t432.adb:11#9)^tmp_cir!1 <- t432a.mular(left_int32[10], res_int32);
    (t432.adb:11#9)^res =(int32) tmp_cir!1_int32;
  }
  (t432.adb:12#8)^{
    int32 tmp_cir!0;
    (t432.adb:12#8)^tmp_cir!0 <- t432a.mular(left_int32[10], 3);
    (t432.adb:12#8)^res =(int32) tmp_cir!0_int32;
  }
}

int32 t432a.mular(int32[10] left, int32 right) {
  (t432a.adb:10#12)^!return =(int32) 5;
}


