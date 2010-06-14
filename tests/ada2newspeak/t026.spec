Newspeak output
---------------
t026.adb
void t026(void) {
  (t026.adb:3#6)^int32 x;
  (t026.adb:4#6)^int32 y;
  (t026.adb:3#6)^x =(int32) 10;
  (t026.adb:4#6)^y =(int32) 0;
  (t026.adb:6#8)^do {
    (t026.adb:6#8)^while (1) {
      (t026.adb:6#8)^choose {
       -->
        (t026.adb:6#8)^guard((x_int32 > 0));
       -->
        (t026.adb:6#8)^guard(! (x_int32 > 0));
        (t026.adb:6#8)^goto lbl1;
      }
      (t026.adb:7#10)^y =(int32) belongs[-2147483648,2147483647] (y_int32 + x_int32);
      (t026.adb:8#10)^x =(int32) belongs[-2147483648,2147483647] (x_int32 - 1);
    }
  } with lbl1: {
  }
}


