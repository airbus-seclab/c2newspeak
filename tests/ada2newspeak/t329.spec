Newspeak output
---------------
void t329(void) {
  (t329.adb:9#5)^float32[10] a;
  (t329.adb:11#7)^int32 i;
  (t329.adb:11#7)^i =(int32) 1;
  (t329.adb:11#7)^do {
    (t329.adb:11#7)^while (1) {
      (t329.adb:11#7)^choose {
       -->
        (t329.adb:11#7)^guard(! (i_int32 > 10));
       -->
        (t329.adb:11#7)^guard((i_int32 > 10));
        (t329.adb:11#7)^goto lbl1;
      }
      (t329.adb:11#7)^i =(int32) belongs[-2147483648,2147483647] (i_int32 + 1);
    }
  } with lbl1:
}


