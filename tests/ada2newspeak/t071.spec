Newspeak output
---------------
t071.adb
t103.adb
void t071(void) {
  (t071.adb:4#6)^uint1 x;
  (t071.adb:5#6)^uint1 y;
  (t071.adb:6#6)^uint1 z;
  (t071.adb:4#6)^x =(uint1) 0;
  (t071.adb:5#6)^{
    uint1 tmp_cir!0;
    (t071.adb:5#6)^tmp_cir!0: uint1 <- t103.f();
    (t071.adb:5#6)^y =(uint1) tmp_cir!0_uint1;
  }
  (t071.adb:6#6)^z =(uint1) y_uint1;
}

uint1 t103.f(void) {
  (t103.adb:5#12)^!return =(uint1) 0;
}


