Warning: t471.adb:4#78: Already renamed '!op=' but added
Newspeak output
---------------
void t471.f(void) {
  (t471.adb:6#9)^uint1 c;
  (t471.adb:6#9)^c =(uint1) 0;
  (t471.adb:8#8)^choose {
   -->
    (t471.adb:8#8)^guard((c_uint1 ==_uint1 1));
   -->
    (t471.adb:8#8)^guard(! (c_uint1 ==_uint1 1));
  }
}


