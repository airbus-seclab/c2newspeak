Newspeak output
---------------
t027.adb
void t027(void) {
  (t027.adb:3#6)^int32 x;
  (t027.adb:4#6)^int32 y;
  (t027.adb:3#6)^1- =(int32) 10;
  (t027.adb:4#6)^0- =(int32) 0;
  (t027.adb:6#7)^do {
    (t027.adb:6#7)^while (1) {
      (t027.adb:7#10)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1-_int32);
      (t027.adb:8#10)^1- =(int32) belongs[-2147483648,2147483647] (1-_int32 - 1);
      (t027.adb:9#10)^choose {
       -->
        (t027.adb:9#10)^guard((0 > 1-_int32));
        (t027.adb:9#10)^goto lbl1;
       -->
        (t027.adb:9#10)^guard(! (0 > 1-_int32));
      }
    }
  } with lbl1: {
  }
}


