Newspeak output
---------------
t337.adb
void t337(void) {
  (t337.adb:9#7)^int32 i;
  (t337.adb:9#7)^0- =(int32) 0;
  (t337.adb:9#7)^do {
    (t337.adb:9#7)^while (1) {
      (t337.adb:9#7)^choose {
       -->
        (t337.adb:9#7)^guard(! (0-_int32 > 10));
       -->
        (t337.adb:9#7)^guard((0-_int32 > 10));
        (t337.adb:9#7)^goto lbl1;
      }
      (t337.adb:9#7)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}

