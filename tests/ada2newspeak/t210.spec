Newspeak output
---------------
t210.adb
int32 t210.f(int32) {
  (t210.adb:5#12)^1- =(int32) 1;
}

void t210.main(void) {
  (t210.adb:9#9)^int32 x;
  (t210.adb:11#10)^int32 !tmp0;
  (t210.adb:11#10)^{
    int32 y;
    (t210.adb:11#10)^0- =(int32) 2;
    (t210.adb:11#10)^t210.f();
  }
  (t210.adb:11#10)^1- =(int32) 0-_int32;
}


