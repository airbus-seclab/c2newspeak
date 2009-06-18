Newspeak output
---------------
t089.adb
int32 t089.a(void) {
  (t089.adb:25#12)^0- =(int32) 12;
}

int32 t089.b(void) {
  (t089.adb:20#12)^0- =(int32) 2;
}

uint3 t089.jourdusoleil(void) {
  (t089.adb:15#12)^0- =(uint3) 5;
}

void t089.main(void) {
  (t089.adb:30#9)^int32 x;
  (t089.adb:31#9)^uint3 y;
  (t089.adb:32#9)^int32 z;
  (t089.adb:33#9)^uint3 w;
  (t089.adb:35#10)^{
    int32 !tmp2;
    (t089.adb:35#10)^t089.b();
    (t089.adb:35#10)^4- =(int32) 0-_int32;
  }
  (t089.adb:36#10)^2- =(uint3) 5;
  (t089.adb:37#10)^{
    int32 !tmp1;
    (t089.adb:37#10)^t089.a();
    (t089.adb:37#10)^2- =(int32) belongs[10,15] 0-_int32;
  }
  (t089.adb:38#10)^{
    uint3 !tmp0;
    (t089.adb:38#10)^t089.jourdusoleil();
    (t089.adb:38#10)^1- =(uint3) belongs[0,6] 0-_uint3;
  }
}


