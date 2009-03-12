Newspeak output
---------------
t027.adb
void t027(void) {
  (t027.adb:3#62)^int32 x;
  (t027.adb:4#84)^int32 y;
  (t027.adb:3#62)^1- =(int32) 10;
  (t027.adb:4#84)^0- =(int32) 0;
  (t027.adb:6#111)^do {
    (t027.adb:6#111)^while (1) {
      (t027.adb:7#119)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1-_int32);
      (t027.adb:8#137)^1- =(int32) belongs[-2147483648,2147483647] (1-_int32 - 1);
      (t027.adb:9#153)^choose {
       -->
        (t027.adb:9#153)^guard((0 > 1-_int32));
        (t027.adb:9#153)^goto lbl1;
       -->
        (t027.adb:9#153)^guard(! (0 > 1-_int32));
      }
    }
  } with lbl1: {
  }
}


