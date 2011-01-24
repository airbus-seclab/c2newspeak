Newspeak output
---------------
void t470(void) {
  (t470.adb:7#5)^uint1 tmp_cir!0;
  (t470.adb:7#5)^tmp_cir!0: uint1 <- t470b.xx();
  (t470.adb:7#5)^choose {
   -->
    (t470.adb:7#5)^guard((tmp_cir!0_uint1 ==_uint1 1));
   -->
    (t470.adb:7#5)^guard(! (tmp_cir!0_uint1 ==_uint1 1));
  }
}

uint1 t470b.xx(void) {
  (t470b.adb:3#9)^uint1 r;
  (t470b.adb:5#12)^!return =(uint1) r_uint1;
}


