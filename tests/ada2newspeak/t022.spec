Newspeak output
---------------
void t022(void) {
  (t022.adb:3#11)^uint1 b1;
  (t022.adb:3#11)^uint1 b2;
  (t022.adb:4#7)^uint1 b3;
  (t022.adb:3#11)^b1 =(uint1) 1;
  (t022.adb:3#11)^b2 =(uint1) 1;
  (t022.adb:6#8)^{
    uint1 tmp_ada_firstpass!0;
    (t022.adb:6#8)^choose {
     -->
      (t022.adb:6#8)^guard(b1_uint1);
      (t022.adb:6#8)^tmp_ada_firstpass!0 =(uint1) 1;
     -->
      (t022.adb:6#8)^guard(! b1_uint1);
      (t022.adb:6#8)^tmp_ada_firstpass!0 =(uint1) b2_uint1;
    }
    (t022.adb:6#8)^b3 =(uint1) tmp_ada_firstpass!0_uint1;
  }
}


