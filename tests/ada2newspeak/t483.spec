Warning: t483.adb:6#6: UNSOUND static evaluation of cast
Newspeak output
---------------
void t483(void) {
  (t483.adb:2#7)^float32 mi;
  (t483.adb:3#7)^float32 ma;
  (t483.adb:5#16)^float32 c_m;
  (t483.adb:6#6)^float32 c;
  (t483.adb:2#7)^mi =(float32) (0. -. 0.5);
  (t483.adb:3#7)^ma =(float32) 0.5;
  (t483.adb:5#16)^c_m =(float32) 65536.;
  (t483.adb:6#6)^c =(float32) ((0.5 -. -0.5) /. c_m_float32);
}


