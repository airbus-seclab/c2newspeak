Newspeak output
---------------
void t473.r(void) {
  (t473.adb:4#9)^int32 i;
  (t473.adb:4#9)^i =(int32) 1;
  (t473.adb:4#9)^do {
    (t473.adb:4#9)^while (1) {
      (t473.adb:4#9)^choose {
       -->
        (t473.adb:4#9)^guard(! (i_int32 > 14));
       -->
        (t473.adb:4#9)^guard((i_int32 > 14));
        (t473.adb:4#9)^goto lbl1;
      }
      (t473.adb:4#9)^i =(int32) belongs[1,14] (i_int32 + 1);
    }
  } with lbl1:
}


