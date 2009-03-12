Newspeak output
---------------
t236.adb
void t236(void) {
  (t236.adb:8#167)^int32 x;
  (t236.adb:10#190)^int32 i;
  (t236.adb:10#190)^0- =(int32) 5;
  (t236.adb:10#190)^do {
    (t236.adb:10#190)^while (1) {
      (t236.adb:10#190)^choose {
       -->
        (t236.adb:10#190)^guard((0-_int32 > 10));
        (t236.adb:10#190)^goto lbl1;
       -->
        (t236.adb:10#190)^guard(! (0-_int32 > 10));
      }
      (t236.adb:11#214)^1- =(int32) 0-_int32;
      (t236.adb:10#190)^0- =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (0-_int32 + 1);
    }
  } with lbl1: {
  }
}


