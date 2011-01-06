Warning: t440a.ads:3#7: Ignoring representation clause for 't_u'
Newspeak output
---------------
void t440.s(void) {
  (t440.adb:9#11)^int17 s;
  (t440.adb:9#11)^s =(int17) 1;
  (t440.adb:9#11)^do {
    (t440.adb:9#11)^while (1) {
      (t440.adb:9#11)^choose {
       -->
        (t440.adb:9#11)^guard(! (s_int17 > t440.c_int17));
       -->
        (t440.adb:9#11)^guard((s_int17 > t440.c_int17));
        (t440.adb:9#11)^goto lbl1;
      }
      (t440.adb:9#11)^s =(int32) belongs[0,65535] (s_int17 + 1);
    }
  } with lbl1:
}

int17 t440.c;
(t440.adb:6#5)^t440.c =(int17) 6;

