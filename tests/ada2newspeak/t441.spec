Newspeak output
---------------
void t441.s(void) {
  (t441.adb:4#11)^int4 s;
  (t441.adb:4#11)^s =(int4) 1;
  (t441.adb:4#11)^do {
    (t441.adb:4#11)^while (1) {
      (t441.adb:4#11)^choose {
       -->
        (t441.adb:4#11)^guard(! (s_int4 > 4));
       -->
        (t441.adb:4#11)^guard((s_int4 > 4));
        (t441.adb:4#11)^goto lbl1;
      }
      (t441.adb:4#11)^s =(int32) belongs[0,4] (s_int4 + 1);
    }
  } with lbl1:
}


