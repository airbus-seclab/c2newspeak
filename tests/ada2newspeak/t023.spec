Newspeak output
---------------
t023.adb
void t023(void) {
  (t023.adb:3#11)^uint1 b1;
  (t023.adb:3#11)^uint1 b2;
  (t023.adb:4#7)^uint1 b3;
  (t023.adb:3#11)^b1 =(uint1) 1;
  (t023.adb:3#11)^b2 =(uint1) 1;
  (t023.adb:6#8)^{
    uint1 tmp0;
    (t023.adb:6#8)^choose {
     -->
      (t023.adb:6#8)^guard(b1_uint1);
      (t023.adb:6#8)^tmp0 =(uint1) ! b2_uint1;
     -->
      (t023.adb:6#8)^guard(! b1_uint1);
      (t023.adb:6#8)^tmp0 =(uint1) b2_uint1;
    }
    (t023.adb:6#8)^b3 =(uint1) tmp0_uint1;
  }
}


