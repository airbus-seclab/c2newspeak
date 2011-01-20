Newspeak output
---------------
void t436.mm(void) {
  (t436.adb:4#7)^uint2 y;
  (t436.adb:4#7)^uint2 z;
  (t436.adb:6#2)^uint1 tmp_cir!0;
  (t436.adb:6#2)^tmp_cir!0: uint1 <- t436a.!op=(z_uint2: uint2, y_uint2: uint2);
  (t436.adb:6#2)^choose {
   -->
    (t436.adb:6#2)^guard(tmp_cir!0_uint1);
   -->
    (t436.adb:6#2)^guard(! tmp_cir!0_uint1);
  }
}


