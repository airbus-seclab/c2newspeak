Newspeak output
---------------
t026.adb
void t026(void) {
  (t026.adb:3#6)^int32 x;
  (t026.adb:4#6)^int32 y;
  (t026.adb:3#6)^1- =(int32) 10;
  (t026.adb:4#6)^0- =(int32) 0;
  (t026.adb:6#8)^do {
    (t026.adb:6#8)^while (1) {
      (t026.adb:6#8)^choose {
       -->
        (t026.adb:6#8)^guard((1-_int32 > 0));
       -->
        (t026.adb:6#8)^guard(! (1-_int32 > 0));
        (t026.adb:6#8)^goto lbl1;
      }
      (t026.adb:7#10)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1-_int32);
      (t026.adb:8#10)^1- =(int32) belongs[-2147483648,2147483647] (1-_int32 - 1);
    }
  } with lbl1: {
  }
}


