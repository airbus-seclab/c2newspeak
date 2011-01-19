Newspeak output
---------------
void t469(void) {
  (t469.adb:5#5)^uint1 tmp_cir!0;
  (t469.adb:5#5)^tmp_cir!0: uint1 <- t469a.x();
  (t469.adb:5#5)^choose {
   -->
    (t469.adb:5#5)^guard((tmp_cir!0_uint1 ==_uint1 1));
   -->
    (t469.adb:5#5)^guard(! (tmp_cir!0_uint1 ==_uint1 1));
  }
}

uint1 t469a.x(void) {
  (t469a.adb:3#9)^uint1 r;
  (t469a.adb:5#12)^!return =(uint1) r_uint1;
}


