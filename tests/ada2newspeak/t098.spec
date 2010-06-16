Newspeak output
---------------
t098.adb
int32 t098.a(void) {
  (t098.adb:32#11)^!return =(int32) 8;
}

uint3 t098.g(void) {
  (t098.adb:13#12)^!return =(uint3) 1;
}

void t098.main(uint2 z) {
  (t098.adb:19#9)^int32 x;
  (t098.adb:21#10)^{
    int32 tmp_cir!0;
    (t098.adb:21#10)^tmp_cir!0 <- t098.t096();
    (t098.adb:21#10)^x =(int32) tmp_cir!0_int32;
  }
  (t098.adb:22#11)^t099(0);
}

void t098.proc(int32 z) {
  (t098.adb:27#11)^t098.main(3);
}

int32 t098.t096(void) {
  (t098.adb:8#12)^!return =(int32) 5;
}


