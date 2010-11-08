Newspeak output
---------------
t240.adb
void t240(void) {
  (t240.adb:7#7)^int32 x;
  (t240.adb:9#7)^{
    int32 i;
    (t240.adb:9#7)^i =(int32) 4;
    (t240.adb:9#7)^do {
      (t240.adb:9#7)^while (1) {
        (t240.adb:9#7)^choose {
         -->
          (t240.adb:9#7)^guard(! (i_int32 > 8));
         -->
          (t240.adb:9#7)^guard((i_int32 > 8));
          (t240.adb:9#7)^goto lbl1;
        }
        (t240.adb:10#12)^x =(int32) 3;
        (t240.adb:9#7)^i =(int32) belongs[-2147483648,2147483647] (i_int32 + 1);
      }
    } with lbl1:
  }
  (t240.adb:12#7)^{
    int32 j;
    (t240.adb:12#7)^j =(int32) 16;
    (t240.adb:12#7)^do {
      (t240.adb:12#7)^while (1) {
        (t240.adb:12#7)^choose {
         -->
          (t240.adb:12#7)^guard(! (12 > j_int32));
         -->
          (t240.adb:12#7)^guard((12 > j_int32));
          (t240.adb:12#7)^goto lbl2;
        }
        (t240.adb:13#12)^x =(int32) 8;
        (t240.adb:12#7)^j =(int32) belongs[-2147483648,2147483647] (j_int32 - 1);
      }
    } with lbl2:
  }
  (t240.adb:15#8)^x =(int32) 58;
}


