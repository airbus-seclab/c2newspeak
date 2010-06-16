Newspeak output
---------------
t038.adb
uint1 t038.a(void) {
  (t038.adb:5#12)^!return =(uint1) 0;
}

void t038.appel_fonction(void) {
  (t038.adb:9#8)^uint1 x;
  (t038.adb:10#8)^uint1 y;
  (t038.adb:12#10)^x =(uint1) 0;
  (t038.adb:13#10)^{
    uint1 tmp_cir!0;
    (t038.adb:13#10)^tmp_cir!0 <- t038.a();
    (t038.adb:13#10)^y =(uint1) tmp_cir!0_uint1;
  }
}


