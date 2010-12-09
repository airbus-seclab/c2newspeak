Newspeak output
---------------
t369.adb
void t369.f(int32 x, int32 y, int32 z) {
  (t369.adb:9#8)^y =(int32) belongs[-2147483648,2147483647] (y_int32 + 1);
  (t369.adb:10#8)^z =(int32) 8;
}

void t369.main(void) {
  (t369.adb:14#7)^int32 u;
  (t369.adb:15#7)^int32 v;
  (t369.adb:17#8)^u =(int32) 4;
  (t369.adb:18#7)^(u: int32, v: int32) <- t369.f(3: int32);
}


