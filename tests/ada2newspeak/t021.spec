Newspeak output
---------------
void t021(void) {
  (t021.adb:3#11)^uint1 b1;
  (t021.adb:3#11)^uint1 b2;
  (t021.adb:4#7)^uint1 b3;
  (t021.adb:3#11)^b1 =(uint1) 1;
  (t021.adb:3#11)^b2 =(uint1) 1;
  (t021.adb:6#8)^{
    uint1 tmp_ada_firstpass!0;
    (t021.adb:6#8)^choose {
     -->
      (t021.adb:6#8)^guard(b1_uint1);
      (t021.adb:6#8)^tmp_ada_firstpass!0 =(uint1) b2_uint1;
     -->
      (t021.adb:6#8)^guard(! b1_uint1);
      (t021.adb:6#8)^tmp_ada_firstpass!0 =(uint1) 0;
    }
    (t021.adb:6#8)^b3 =(uint1) tmp_ada_firstpass!0_uint1;
  }
}


