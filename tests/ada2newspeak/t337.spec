Newspeak output
---------------
void t337(void) {
  (t337.adb:9#7)^int32 i;
  (t337.adb:9#7)^i =(int32) 0;
  (t337.adb:9#7)^do {
    (t337.adb:9#7)^while (1) {
      (t337.adb:9#7)^choose {
       -->
        (t337.adb:9#7)^guard(! (i_int32 > 10));
       -->
        (t337.adb:9#7)^guard((i_int32 > 10));
        (t337.adb:9#7)^goto lbl1;
      }
      (t337.adb:9#7)^i =(int32) belongs[0,10] (i_int32 + 1);
    }
  } with lbl1:
}


