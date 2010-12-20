Newspeak output
---------------
void t237(void) {
  (t237.adb:7#7)^int32 x;
  (t237.adb:9#7)^int32 i;
  (t237.adb:9#7)^i =(int32) 85;
  (t237.adb:9#7)^do {
    (t237.adb:9#7)^while (1) {
      (t237.adb:9#7)^choose {
       -->
        (t237.adb:9#7)^guard(! (21 > i_int32));
       -->
        (t237.adb:9#7)^guard((21 > i_int32));
        (t237.adb:9#7)^goto lbl1;
      }
      (t237.adb:10#12)^x =(int32) 56;
      (t237.adb:9#7)^i =(int32) belongs[-2147483648,2147483647] (i_int32 - 1);
    }
  } with lbl1:
}


