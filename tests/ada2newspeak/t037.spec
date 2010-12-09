Newspeak output
---------------
t037.adb
void t037.appelfonction(void) {
  (t037.adb:9#9)^uint1 x;
  (t037.adb:12#10)^{
    uint1 tmp_cir!0;
    (t037.adb:12#10)^tmp_cir!0: uint1 <- t037.f(10: int32);
    (t037.adb:12#10)^x =(uint1) tmp_cir!0_uint1;
  }
  (t037.adb:13#10)^x =(uint1) 1;
}

uint1 t037.f(int32 x) {
  (t037.adb:5#12)^!return =(uint1) 0;
}


