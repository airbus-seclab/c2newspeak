Newspeak output
---------------
int32 t089.a(void) {
  (t089.adb:25#12)^!return =(int32) 12;
}

int32 t089.b(void) {
  (t089.adb:20#12)^!return =(int32) 2;
}

uint3 t089.jourdusoleil(void) {
  (t089.adb:15#12)^!return =(uint3) 5;
}

void t089.main(void) {
  (t089.adb:30#9)^int32 x;
  (t089.adb:31#9)^uint3 y;
  (t089.adb:32#9)^int32 z;
  (t089.adb:33#9)^uint3 w;
  (t089.adb:35#10)^{
    int32 tmp_cir!2;
    (t089.adb:35#10)^tmp_cir!2: int32 <- t089.b();
    (t089.adb:35#10)^x =(int32) tmp_cir!2_int32;
  }
  (t089.adb:36#10)^y =(uint3) 5;
  (t089.adb:37#10)^{
    int32 tmp_cir!1;
    (t089.adb:37#10)^tmp_cir!1: int32 <- t089.a();
    (t089.adb:37#10)^z =(int32) belongs[10,15] tmp_cir!1_int32;
  }
  (t089.adb:38#10)^{
    uint3 tmp_cir!0;
    (t089.adb:38#10)^tmp_cir!0: uint3 <- t089.jourdusoleil();
    (t089.adb:38#10)^w =(uint3) belongs[0,6] tmp_cir!0_uint3;
  }
}


