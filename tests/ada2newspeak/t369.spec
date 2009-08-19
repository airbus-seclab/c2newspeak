Newspeak output
---------------
t369.adb
void t369.f(int32, int32, int32) {
  (t369.adb:9#8)^1- =(int32) belongs[-2147483648,2147483647] (1-_int32 + 1);
  (t369.adb:10#8)^0- =(int32) 8;
}

void t369.main(void) {
  (t369.adb:14#7)^int32 u;
  (t369.adb:15#7)^int32 v;
  (t369.adb:17#8)^1- =(int32) 4;
  (t369.adb:18#7)^{
    int32 x;
    (t369.adb:18#7)^0- =(int32) 3;
    (t369.adb:18#7)^{
      int32 y;
      (t369.adb:18#7)^{
        int32 z;
        (t369.adb:18#7)^t369.f();
        (t369.adb:18#7)^3- =(int32) 0-_int32;
      }
      (t369.adb:18#7)^3- =(int32) 0-_int32;
    }
  }
}


