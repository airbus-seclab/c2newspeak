Newspeak output
---------------
t022.adb
void t022(void) {
  (t022.adb:3#32)^uint1 b1;
  (t022.adb:3#32)^uint1 b2;
  (t022.adb:4#61)^uint1 b3;
  (t022.adb:3#32)^2- =(uint1) 1;
  (t022.adb:3#32)^1- =(uint1) 1;
  (t022.adb:6#84)^{
    uint1 tmp0;
    (t022.adb:6#84)^choose {
     -->
      (t022.adb:6#84)^guard(3-_uint1);
      (t022.adb:6#84)^0- =(uint1) 1;
     -->
      (t022.adb:6#84)^guard(! 3-_uint1);
      (t022.adb:6#84)^0- =(uint1) 2-_uint1;
    }
    (t022.adb:6#84)^1- =(uint1) 0-_uint1;
  }
}


