Newspeak output
---------------
void t472(void) {
  (t472.adb:4#5)^uint1 tmp_cir!0;
  (t472.adb:4#5)^tmp_cir!0: uint1 <- t472c.s();
  (t472.adb:4#5)^choose {
   -->
    (t472.adb:4#5)^guard((tmp_cir!0_uint1 ==_uint1 1));
   -->
    (t472.adb:4#5)^guard(! (tmp_cir!0_uint1 ==_uint1 1));
  }
}

uint1 t472c.s(void) {
  (t472c.adb:4#12)^!return =(uint1) 0;
}


