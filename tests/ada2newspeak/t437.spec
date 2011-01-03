Warning: t437.adb:5#31: Be carefull binary operator not explicitely declared
Newspeak output
---------------
void t437(void) {
  (t437.adb:3#5)^uint2 y;
  (t437.adb:3#5)^uint2 z;
  (t437.adb:5#3)^choose {
   -->
    (t437.adb:5#3)^guard((z_uint2 ==_uint2 y_uint2));
   -->
    (t437.adb:5#3)^guard(! (z_uint2 ==_uint2 y_uint2));
  }
}


