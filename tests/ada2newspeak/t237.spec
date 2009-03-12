Newspeak output
---------------
t237.adb
void t237(void) {
  (t237.adb:7#117)^int32 x;
  (t237.adb:9#140)^int32 i;
  (t237.adb:9#140)^0- =(int32) 85;
  (t237.adb:9#140)^do {
    (t237.adb:9#140)^while (1) {
      (t237.adb:9#140)^choose {
       -->
        (t237.adb:9#140)^guard((21 > 0-_int32));
        (t237.adb:9#140)^goto lbl1;
       -->
        (t237.adb:9#140)^guard(! (21 > 0-_int32));
      }
      (t237.adb:10#173)^1- =(int32) 56;
      (t237.adb:9#140)^0- =(int32) belongs[-2147483648,2147483647] (0-_int32 - 1);
    }
  } with lbl1: {
  }
}


