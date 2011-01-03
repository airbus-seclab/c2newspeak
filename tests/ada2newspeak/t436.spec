Warning: t436.adb:6#32: Be carefull binary operator not explicitely declared
Newspeak output
---------------
void t436.mm(void) {
  (t436.adb:4#7)^uint2 y;
  (t436.adb:4#7)^uint2 z;
  (t436.adb:6#2)^choose {
   -->
    (t436.adb:6#2)^guard((z_uint2 ==_uint2 y_uint2));
   -->
    (t436.adb:6#2)^guard(! (z_uint2 ==_uint2 y_uint2));
  }
}


