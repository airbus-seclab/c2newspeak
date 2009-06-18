Newspeak output
---------------
t022.adb
void t022(void) {
  (t022.adb:3#11)^uint1 b1;
  (t022.adb:3#11)^uint1 b2;
  (t022.adb:4#7)^uint1 b3;
  (t022.adb:3#11)^2- =(uint1) 1;
  (t022.adb:3#11)^1- =(uint1) 1;
  (t022.adb:6#8)^{
    uint1 tmp0;
    (t022.adb:6#8)^choose {
     -->
      (t022.adb:6#8)^guard(3-_uint1);
      (t022.adb:6#8)^0- =(uint1) 1;
     -->
      (t022.adb:6#8)^guard(! 3-_uint1);
      (t022.adb:6#8)^0- =(uint1) 2-_uint1;
    }
    (t022.adb:6#8)^1- =(uint1) 0-_uint1;
  }
}


