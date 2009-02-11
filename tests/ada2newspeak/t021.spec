Newspeak output
---------------
t021.adb
void t021(void) {
  (t021.adb:3#33)^uint1 b1;
  (t021.adb:3#33)^uint1 b2;
  (t021.adb:4#62)^uint1 b3;
  (t021.adb:3#33)^2- =(uint1) 1;
  (t021.adb:3#33)^1- =(uint1) 1;
  (t021.adb:6#85)^{
    uint1 tmp0;
    (t021.adb:6#85)^choose {
     -->
      (t021.adb:6#85)^guard(3-_uint1);
      (t021.adb:6#85)^0- =(uint1) 2-_uint1;
     -->
      (t021.adb:6#85)^guard(! 3-_uint1);
      (t021.adb:6#85)^0- =(uint1) 0;
    }
    (t021.adb:6#85)^1- =(uint1) 0-_uint1;
  }
}


