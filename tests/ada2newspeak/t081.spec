Newspeak output
---------------
t081.adb
int32 t081.a(void) {
  (t081.adb:8#178)^0- =(int32) 5;
}

void t081.main(void) {
  (t081.adb:14#243)^int32 x;
  (t081.adb:15#276)^int32 y;
  (t081.adb:14#243)^{
    int32 !tmp0;
    (t081.adb:14#243)^t081.a();
    (t081.adb:14#243)^2- =(int32) 0-_int32;
  }
  (t081.adb:19#336)^0- =(int32) belongs[-2147483648,2147483647] (((1-_int32 + 15) + t081.c_int32) + 15);
}

int32 t081.c = 0;

