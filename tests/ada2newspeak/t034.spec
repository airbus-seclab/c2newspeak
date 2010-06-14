Newspeak output
---------------
t034.adb
void t034.appelfonction(void) {
  (t034.adb:11#9)^int32 y;
  (t034.adb:13#10)^int32 !tmp0;
  (t034.adb:13#10)^!tmp0 <- t034.f();
  (t034.adb:13#10)^y =(int32) !tmp0_int32;
}

int32 t034.f(void) {
  (t034.adb:6#10)^t034.x =(int32) belongs[-2147483648,2147483647] (t034.x_int32 + 1);
  (t034.adb:7#12)^!return =(int32) t034.x_int32;
}

int32 t034.x;
(t034.adb:2#6)^t034.x =(int32) 1;

