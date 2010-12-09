Newspeak output
---------------
t059.adb
t101.adb
void t059(void) {
  (t059.adb:3#5)^int32 x;
  (t059.adb:4#5)^uint1 y;
  (t059.adb:6#6)^{
    int32 tmp_cir!0;
    (t059.adb:6#6)^tmp_cir!0: int32 <- t101.b();
    (t059.adb:6#6)^x =(int32) tmp_cir!0_int32;
  }
  (t059.adb:7#6)^y =(uint1) 1;
}

int32 t101.b(void) {
  (t101.adb:5#12)^!return =(int32) 0;
}


