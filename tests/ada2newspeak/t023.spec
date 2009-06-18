Newspeak output
---------------
t023.adb
void t023(void) {
  (t023.adb:3#11)^uint1 b1;
  (t023.adb:3#11)^uint1 b2;
  (t023.adb:4#7)^uint1 b3;
  (t023.adb:3#11)^2- =(uint1) 1;
  (t023.adb:3#11)^1- =(uint1) 1;
  (t023.adb:6#8)^{
    uint1 tmp0;
    (t023.adb:6#8)^choose {
     -->
      (t023.adb:6#8)^guard(3-_uint1);
      (t023.adb:6#8)^0- =(uint1) ! 2-_uint1;
     -->
      (t023.adb:6#8)^guard(! 3-_uint1);
      (t023.adb:6#8)^0- =(uint1) 2-_uint1;
    }
    (t023.adb:6#8)^1- =(uint1) 0-_uint1;
  }
}


